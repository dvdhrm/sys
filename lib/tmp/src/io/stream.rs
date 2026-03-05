//! # Streaming I/O Utilities
//!
//! This module provides utilities to work with data streams. The [`Read`] and
//! [`Write`] abstractions allow consumers and producers to be written
//! independently without requiring knowledge about each other.

use core::mem::MaybeUninit as Uninit;
use core::ops::ControlFlow as Flow;

/// An enumeration of all possible generic errors the streaming traits can
/// return.
///
/// These errors are all related to buffer allocation, management, and mapping.
/// I/O operations are performed separately and thus do not use this error
/// type.
#[derive(Clone, Copy, Debug, Hash)]
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub enum Error {
    /// The offset calculation exceeds the limits of the implementation. This
    /// indicates that the buffer would need to exceed `isize::MAX` to hold the
    /// requested data linearly. This is a hard limit for Rust objects, though.
    ///
    /// A buffered [`Read`] or [`Write`] implementation could technically hold
    /// more than [`isize::MAX`] bytes, if indexed accessors are used. However,
    /// the trait is free to refuse any buffered operations that exceed this
    /// limit by throwing this error.
    Overflow,
}

/// `More` describes the data extents needed to serve a request.
///
/// The main use is for [`Read::map()`] and its derivatives to signal how
/// much more data is needed to serve the request.
///
/// The type describes the extents necessary to serve a request. However, those
/// extents might overlap with available data. That is, a value of
/// `{ idx: 4, len: 8 }` means the total length needs to be at least `12` to
/// serve a request, yet the current extents of the buffer might be anything
/// between `0` and `7`.
#[derive(Clone, Copy, Debug, Hash)]
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub struct More {
    /// Index at which the request was performed. This is relative to the
    /// current position of the stream.
    pub idx: usize,
    /// Minimum number of bytes the buffer must have past the index to serve
    /// the request.
    pub len: usize,
}

/// A shared slice for reading from a stream.
///
/// [`Self`] represents a non-linear data slice into an instance of [`Read`].
/// It can be thought of as the non-linear equivalent to `&[u8]`.
///
/// Since data buffers are not necessarily available linearly, [`Self`] can be
/// used to treat the data as if it was available linearly. It provides
/// functions to copy data out of the buffers into linear memory.
#[derive(Clone, Debug, Hash)]
#[derive(Eq, PartialEq)]
pub struct Slice<'read, T: ?Sized> {
    read: &'read T,
    range: core::ops::Range<usize>,
}

/// Input stream with buffered reads.
///
/// This trait allows reading from stream buffers without having to rely on
/// a specific stream implementation. The trait is an abstraction over the
/// buffers used by input streams. The actual I/O operations are outside the
/// scope of this trait.
///
/// The trait is similar to [`std::io::Read`] but is designed for buffered
/// streams. The actual I/O operations are not part of this trait, but must
/// be handled separately. This trait is just an abstraction for the data
/// buffer. That is, when a read request cannot be served due to insufficient
/// buffered data, the request will return [`ControlFlow::Break`](Flow::Break)
/// with a payload of type [`More`]. This indicates how much more data is
/// necessary to serve the request. It is up to the caller to decide how to
/// pass this information to the transport layer. This is outside of the scope
/// of this trait.
///
/// ## Safety
///
/// The implementation must guarantee that no inner mutability of the data can
/// happen. Two consecutive reads of the data buffers must yield the same
/// logical result if performed via the same shared reference.
///
/// A rearrangement of the data buffers is allowed. That is, data can be
/// linearized or split, or moved at the will of the implementation even via
/// interior mutability. However, these operations must be infallible and never
/// change the logical content of the stream buffers.
pub unsafe trait Read {
    /// Advance the stream by the specified number of bytes.
    ///
    /// This will irrevocably discard the specified number of bytes from the
    /// beginning of the buffered data, and thus make room for more incoming
    /// data.
    ///
    /// The underlying stream will buffer data until this function is called.
    fn advance(&mut self, len: usize);

    /// Return the length of the stream buffers.
    ///
    /// This represents the amount of data that is currently stored in the
    /// stream buffers. It does not represent the overall capacity of the
    /// buffers.
    fn len(&self) -> usize;

    /// Map the data buffers of the stream.
    ///
    /// This will return a linear memory mapping of the data buffers of the
    /// stream at the specified index relative to the current stream position.
    ///
    /// The returned slice covers the maximally possible extents that are
    /// linearly available at the given index.
    ///
    /// The slice has a length of 0 if, and only if, the data stream was
    /// terminated and no more data will be made available via any means. If
    /// the stream can still get new data, it will never return a slice of
    /// length 0. Instead, if the index points past the extents of the data
    /// buffers, [`ControlFlow::Break`](Flow::Break) with a value of [`More`]
    /// is returned, indicating that I/O must be performed to serve the
    /// requrest.
    ///
    /// This function does not advance the position of the underlying stream.
    /// Repeated calls to this function will operate on the same data. Use
    /// [`Self::advance()`] to advance the position of the stream.
    /// Furthermore, this function does not perform any I/O. This function
    /// merely maps the available data buffers or rearranges the data to ensure
    /// it is available as a linear mapping, if desired.
    fn map(&self, idx: usize) -> Flow<More, &[u8]>;
}

impl<'read, T: ?Sized + Read> Slice<'read, T> {
    /// Create a new slice with the given extents.
    ///
    /// ## Panics
    ///
    /// This will panic if `range` exceeds the extents of `read`.
    pub fn new(
        read: &'read T,
        range: core::ops::Range<usize>,
    ) -> Self {
        assert!(
            range.len() == 0
            || (
                range.start <= read.len()
                && range.end <= read.len()
            )
        );
        Self {
            read: read,
            range: range,
        }
    }

    /// Try creating a new slice with the given extents.
    ///
    /// Unlike [`Self::new()`] this will never panic but return
    /// [`FlowControl::Break`](Flow::Break) with a value of [`More`] if the
    /// extents are not covered by the stream buffers.
    pub fn try_new(
        read: &'read T,
        range: core::ops::Range<usize>,
    ) -> Flow<More, Self> {
        if range.len() == 0 {
            Flow::Continue(Slice::new(read, range))
        } else if range.start <= read.len() && range.end <= read.len() {
            Flow::Continue(Slice::new(read, range))
        } else {
            Flow::Break(More {
                idx: range.start,
                len: range.end - range.start,
            })
        }
    }

    /// Return the underlying stream referenced by this slice.
    pub fn stream(&self) -> &'read T {
        self.read
    }

    /// Return the extents of this slice.
    pub fn extents(&self) -> core::ops::Range<usize> {
        self.range.clone()
    }

    /// Return the length of the slice.
    pub fn len(&self) -> usize {
        self.range.len()
    }

    /// Return a linear mapping of the slice at the given offset.
    ///
    /// This will always return the longest possible linear mapping at the
    /// given offset. If `idx` points past the end of the slice, an empty
    /// mapping is returned.
    ///
    /// This function will never return a mapping that exceeds the extents
    /// of the slice.
    pub fn map(&self, idx: usize) -> &'read [u8] {
        match self.range.start.checked_add(idx) {
            None => &[],
            Some(v) if v >= self.range.end => &[],
            Some(v) => {
                // `Read` guarantees immutability of its content. Repeated
                // mappings thus must succeed.
                let map = self.read.map(v).continue_value().unwrap();
                &map[..core::cmp::min(map.len(), self.len() - idx)]
            },
        }
    }

    /// Copy data from the slice into a linear buffer.
    ///
    /// Data from the slice will be copied into the linear buffer `dst`. The
    /// destination buffer must be equal to, or shorter than, the size of the
    /// slice. If shorter, the data is truncated.
    ///
    /// ## Panics
    ///
    /// This function will panic if `dst` is longer than `self`.
    pub fn copy_uninit(&self, dst: &mut [Uninit<u8>]) {
        let mut n: usize = 0;
        while n < dst.len() {
            let map = osi::mem::slice_as_uninit(self.map(n));
            assert_ne!(map.len(), 0);
            let map = &map[..core::cmp::min(map.len(), dst.len() - n)];

            let end = n.strict_add(map.len());
            dst[n..end].copy_from_slice(map);
            n = end;
        }
    }

    /// Read the entire slice into a linear mapping.
    ///
    /// If the data is available linearly, a shared reference is returned.
    /// Otherwise, an allocated buffer with the linear data is returned.
    pub fn read(
        &self,
    ) -> osi::mown::Mown<'read, [u8], alloc::boxed::Box<[u8]>> {
        let map = self.map(0);
        if map.len() >= self.len() {
            osi::mown::Mown::new_borrowed(map)
        } else {
            let mut buf_u = alloc::boxed::Box::new_uninit_slice(self.len());
            self.copy_uninit(&mut *buf_u);

            // SAFETY: `buf_u` just got fully initialized.
            let buf = unsafe { buf_u.assume_init() };
            osi::mown::Mown::new_owned(buf)
        }
    }
}

/// `Break` describes an interruption of buffered data accesses.
///
/// This type is used as break value in a [`ControlFlow`](Flow). It indicates
/// either a hard error that cannot be served by the stream (denoted by an
/// `Err<Error>` value), or a lack of buffers that needs to be dealt with
/// out-of-band by the stream implementation (denoted by a `Ok<More>` value).
pub type Break = Result<More, Error>;

/// `Write` allows buffered writes to a data stream.
///
/// This trait is a connection between protocol implementations and transport
/// layers. That is, it allows writing code that writes structured data to a
/// data stream without knowing the transport layer used to stream the data.
///
/// The trait is similar to [`std::io::Write`] but is designed for buffered
/// streams that perform transport layer operations 
///
/// The actual transport layer operations are not part of this trait, but must
/// be handled separately. This trait is just an abstraction for the data
/// buffer. That is, when a write request cannot be served due to insufficient
/// buffer space, the request will return [`ControlFlow::Break`](Flow::Break)
/// with a payload of type [`More`]. This indicates how much more space is
/// necessary to serve the request. It is up to the caller to decide how to
/// pass this information to the transport layer. This is outside of the scope
/// of this trait.
///
/// If this trait is used for non-streamed operations (i.e., buffers grow until
/// all data has been written), then a break value of type [`More`] indicates
/// that no more buffer space could be allocated (e.g., reaching a configured
/// limit, or exceeding the allowance of the process).
pub trait Write {
    /// Commit the specified number of bytes to the stream.
    ///
    /// This will mark the given number of bytes as ready to be written and
    /// advance the position of the stream. No data is actually written to the
    /// transport layer, but merely marked to be ready. Transport layer
    /// operations must be handled separately.
    ///
    /// ## Safety
    ///
    /// The caller must ensure that the first `len` bytes of the stream buffer
    /// have been initialized via [`Self::map_raw()`] or one of its
    /// derivatives.
    unsafe fn commit(&mut self, len: usize);

    /// Map the raw buffers of the stream.
    ///
    /// This will return a linear memory mapping of the stream buffers at
    /// the specified index relative to the current stream position.
    ///
    /// This will only return an empty slice if `len` is 0 and the index points
    /// to the end of the stream. In all other cases it will always return the
    /// longest possible slice it can linearly mutably borrow at the indicated
    /// position.
    ///
    /// Moreover, `len` is nothing more than a hint to indicate how much data
    /// the caller expects to write to the stream. That is, if the
    /// implementation can deduce that there is insufficient buffer space
    /// available to serve `len` bytes, and it cannot allocate more, then it
    /// shall break with a suitable value of [`More`], rather than returning
    /// short mappings. This is not a necessity, though.
    ///
    /// This function does not advance the position of the underlying stream.
    /// Repeated calls to this function will operate on the same data. Use
    /// [`Self::commit()`] to commit data irrevocably to the stream.
    /// Furthermore, this function does not perform any I/O. This function
    /// merely maps the available data buffers or rearranges the buffers to
    /// ensure it is available as a linear mapping.
    ///
    /// If the underlying stream does not have sufficient space available, this
    /// will return [`ControlFlow::Break`](Flow::Break) with a value of
    /// [`More`] indicating how much space is needed. It is up to the caller to
    /// pass this information to the stream operators to ensure pending data is
    /// flushed to the transport layer, or more space is made available.
    fn map_raw(&mut self, idx: usize, len: usize) -> Flow<Break, &mut [Uninit<u8>]>;

    /// Map limited buffers at a specific offset.
    ///
    /// This works like [`Self::map_raw()`] but will always limit the
    /// returned slice to a maximum of `len` bytes.
    fn map_at(&mut self, idx: usize, len: usize) -> Flow<Break, &mut [Uninit<u8>]> {
        let map = self.map_raw(idx, len)?;
        let n = core::cmp::min(len, map.len());
        Flow::Continue(&mut map[..n])
    }

    /// Map limited buffers.
    ///
    /// This works like [`Self::map_at()`] but uses an index of 0.
    fn map(&mut self, len: usize) -> Flow<Break, &mut [Uninit<u8>]> {
        self.map_at(0, len)
    }

    /// Map the initialized data buffers of the stream for writing.
    ///
    /// This works like [`Self::map_at()`] but returns an initialized
    /// reference.
    ///
    /// ## Safety
    ///
    /// The caller must ensure that `len` bytes of the stream buffer at the
    /// requested index have been initialized via [`Self::map_raw()`] or one of
    /// its derivatives.
    unsafe fn map_at_unchecked(&mut self, idx: usize, len: usize) -> Flow<Break, &mut [u8]> {
        self.map_raw(idx, len).map_continue(|v| {
            // SAFETY: Propagated to caller. Truncate the slice to ensure
            //     faulty implementations do not break safety guarantees.
            unsafe { osi::mem::slice_assume_init_mut(&mut v[..len]) }
        })
    }

    /// Write data directly to the stream at the specified index.
    ///
    /// This takes a data buffer and writes it to the stream buffers at the
    /// specified index.
    ///
    /// This will use repeated calls to [`Self::map_raw()`] to write the data.
    /// If any of those calls break, the break value is forwarded to the
    /// caller. In this case some of the buffers might have already been
    /// written. However, an implementation is recommended to break early, to
    /// avoid partial copies.
    fn write_at(&mut self, idx: usize, data: &[u8]) -> Flow<Break> {
        let data_u = osi::mem::slice_as_uninit(data);
        let mut at = 0;

        while at < data.len() {
            let mut n = data.len() - at;
            let Some(idx_at) = idx.checked_add(at) else {
                return Flow::Break(Err(Error::Overflow));
            };
            let map = self.map_at(idx_at, n)?;
            n = core::cmp::min(n, map.len());
            map.copy_from_slice(&data_u[at..at + n]);
            at += n;
        }

        Flow::Continue(())
    }

    /// Write data directly to the stream.
    ///
    /// This works like [`Self::write_at()`] but with an index of 0.
    fn write(&mut self, data: &[u8]) -> Flow<Break> {
        self.write_at(0, data)
    }

    /// Commit data directly to the stream.
    ///
    /// This calls [`Self::write()`] with `data`, followed by
    /// [`Self::commit()`] with `data.len()`.
    ///
    /// If a break value is returned, no data was committed, but some data
    /// might have been copied.
    fn commit_write(&mut self, data: &[u8]) -> Flow<Break> {
        self.write_at(0, data)?;
        // SAFETY: `data.len()` bytes were copied, so they must be initialized.
        unsafe { self.commit(data.len()) };
        Flow::Continue(())
    }
}

impl Write for alloc::vec::Vec<u8> {
    unsafe fn commit(&mut self, len: usize) {
        // SAFETY: Propagated to caller.
        unsafe {
            self.set_len(self.len().strict_add(len));
        }
    }

    fn map_raw(&mut self, idx: usize, len: usize) -> Flow<Break, &mut [Uninit<u8>]> {
        match idx.checked_add(len) {
            None => Flow::Break(Err(Error::Overflow)),
            Some(v) => {
                self.reserve(v);
                Flow::Continue(&mut self.spare_capacity_mut()[idx..])
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// A trivial streaming implementation based on a mapped implementation.
    ///
    /// This simply combines a mapping with a mutable index to get a streaming
    /// implementation that never appends any data.
    // SAFETY: Backing memory uses plain refs so it has no interior mutability.
    unsafe impl<T: ?Sized + crate::io::map::Read> Read for (usize, &T) {
        fn advance(&mut self, len: usize) {
            self.0 = self.0.saturating_add(len);
        }

        fn len(&self) -> usize {
            self.1.len().saturating_sub(self.0)
        }

        fn map(&self, idx: usize) -> Flow<More, &[u8]> {
            if let Some(v) = self.0.checked_add(idx) {
                Flow::Continue(self.1.map(v))
            } else {
                Flow::Continue(&[])
            }
        }
    }

    fn test_read_sealed<T: ?Sized + Read>(read: &mut T, expect: &[u8]) {
        // Total length must match the expected data.
        assert_eq!(read.len(), expect.len());

        // A mapping at each possible offset must match the expected data.
        for i in 0..expect.len() {
            let v = read.map(i).continue_value().unwrap();
            assert_ne!(v.len(), 0);
            assert_eq!(v, &expect[i..i+v.len()]);
        }

        // Reading past the end returns emtpy slices for sealed buffers.
        assert_eq!(read.map(expect.len()).continue_value().unwrap(), &[]);

        // Slicing the data must match the expectation.
        for i in 0..expect.len() {
            let v = Slice::new(read, i..expect.len());
            assert_eq!(&*v.read(), &expect[i..i+v.len()]);
            let v = Slice::new(read, i..i+1);
            assert_eq!(&*v.read(), &expect[i..i+1]);
        }

        // Stripping half the data at the beginning and then verifying again.
        read.advance(expect.len() / 2);
        {
            let expect = &expect[expect.len() / 2..];
            for i in 0..expect.len() {
                let v = read.map(i).continue_value().unwrap();
                assert_ne!(v.len(), 0);
                assert_eq!(v, &expect[i..i+v.len()]);
            }
        }

        // Stripping all data and then verifying the buffers are empty.
        read.advance(expect.len() - (expect.len() / 2));
        assert_eq!(read.map(0).continue_value().unwrap(), &[]);
    }

    #[test]
    fn read_dyn() {
        let data = *b"foobar";
        let mut read: (usize, &[u8; _]) = (0, &data);
        let read_dyn: &mut dyn Read = &mut read;
        test_read_sealed(read_dyn, &data);
    }

    #[test]
    fn read_linear() {
        let data = *b"foobar";
        let mut read: (usize, &[u8]) = (0, &data);
        test_read_sealed(&mut read, &data);
    }

    #[test]
    fn read_vectored() {
        let data: [[u8; 1]; _] = [[b'f'], [b'o'], [b'o'], [b'b'], [b'a'], [b'r']];
        let mut read: (usize, &[[u8; 1]]) = (0, &data);
        test_read_sealed(&mut read, b"foobar");
    }

/*
    // A basic test of the `Write` trait and its helpers, using the trivial
    // and vectored implementations.
    #[test]
    fn write_basic() {
        let mut data_plain = alloc::vec::Vec::new();
        let mut data_vec: (usize, [[Uninit<u8>; 2]; 16]) = (0, [[Uninit::uninit(); 2]; 16]);
        let data_plain_p: &mut dyn Write = &mut data_plain;
        let data_vec_p: &mut dyn Write = &mut data_vec;

        for data_p in [data_plain_p, data_vec_p] {
            let v = data_p.map_raw(0, 32).continue_value().unwrap();
            assert!(v.len() >= 2);

            // Initialize with 0x1f, then 0x10 repeatedly.
            data_p.write_at(0, &[0x1f; 16]).continue_value().unwrap();
            data_p.write_at(16, &[0x10; 16]).continue_value().unwrap();

            // Verify that a re-map correctly shows the values.
            for i in 0..16 {
                let v = unsafe { data_p.map_at_unchecked(i, 1).continue_value().unwrap() };
                assert!(v.len() >= 1);
                assert_eq!(v[0], 0x1f);
            }
            for i in 16..32 {
                let v = unsafe { data_p.map_at_unchecked(i, 1).continue_value().unwrap() };
                assert!(v.len() >= 1);
                assert_eq!(v[0], 0x10);
            }

            // Commit only part of the written data, then verify the remaining
            // part is now at the front.
            unsafe { data_p.commit(16); }
            for i in 0..16 {
                let v = unsafe { data_p.map_at_unchecked(i, 1).continue_value().unwrap() };
                assert!(v.len() >= 1);
                assert_eq!(v[0], 0x10);
            }

            // Rewrite to 0x1f again and directly commit.
            data_p.commit_write(&[0x1f; 16]).continue_value().unwrap();
        }

        // Verify the data in both implementations is all 32 times 0x1f.
        assert_eq!(data_plain, [0x1f; 32]);
        for v in data_vec.1 {
            assert_eq!(
                unsafe { osi::mem::slice_assume_init(&v) },
                &[0x1f; 2],
            );
        }
    }
    */
}
