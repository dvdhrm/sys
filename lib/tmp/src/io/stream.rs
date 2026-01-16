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
/// The main use is for [`Read::map_raw()`] and its derivatives to signal how
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

/// `Break` describes an interruption of buffered data accesses.
///
/// This type is used as break value in a [`ControlFlow`](Flow). It indicates
/// either a hard error that cannot be served by the stream (denoted by an
/// `Err<Error>` value), or a lack of buffers that needs to be dealt with
/// out-of-band by the stream implementation (denoted by a `Ok<More>` value).
pub type Break = Result<More, Error>;

/// A type alias for mapped buffers.
///
/// In most cases data mapped from a [`Read`] implementation can be borrowed
/// directly from the underlying buffers. However, implementations might use
/// vectored buffers, and as such the buffers are not entirely located in
/// linear memory. If areas overlapping multiple vectors are mapped, the memory
/// is copied instead.
///
/// This type hides the details whether data is directly borrowed or in a
/// temporary copy.
pub type Map<'a> = osi::mown::Mown<'a, [u8], alloc::boxed::Box<[u8]>>;

/// `Read` allows buffered reads from a data stream.
///
/// This trait is a connection between protocol implementations and transport
/// layers. That is, it allows writing code that reads structured data from a
/// data stream without knowing the transport layer used to stream the data.
///
/// The trait is similar to [`std::io::Read`] but is designed for buffered
/// streams that perform transport layer operations 
///
/// The actual transport layer operations are not part of this trait, but must
/// be handled separately. This trait is just an abstraction for the data
/// buffer. That is, when a read request cannot be served due to insufficient
/// buffered data, the request will return [`ControlFlow::Break`](Flow::Break)
/// with a payload of type [`More`]. This indicates how much more data is
/// necessary to serve the request. It is up to the caller to decide how to
/// pass this information to the transport layer. This is outside of the scope
/// of this trait.
///
/// If this trait is used for non-streamed operations (i.e., all data has been
/// prefetched), then a break value of type [`More`] indicates that the end of
/// the data has been reached.
pub trait Read {
    /// Advance the stream by the specified number of bytes.
    ///
    /// This will irrevocably discard the specified number of bytes from the
    /// beginning of the buffered data, and thus make room for more incoming
    /// data.
    ///
    /// The underlying stream will buffer data until this function is called.
    fn advance(&mut self, len: usize);

    /// Map the raw data of the stream.
    ///
    /// This will return a linear memory mapping of the data of the stream at
    /// the specified index relative to the current stream position.
    ///
    /// This will only return an empty slice if `len` is 0 and the index points
    /// to the end of the stream. In all other cases it will always return the
    /// longest possible slice it can linearly borrow at the indicated
    /// position.
    ///
    /// Moreover, `len` is nothing more than a hint to indicate how much data
    /// the caller expects to read from the stream. That is, if the
    /// implementation can deduce that there is insufficient data available to
    /// serve `len` bytes, it shall break with a suitable value of [`More`],
    /// rather than returning short mappings. This is not a necessity, though.
    ///
    /// This function does not advance the position of the underlying stream.
    /// Repeated calls to this function will operate on the same data. Use
    /// [`Self::advance()`] to advance the position of the stream.
    /// Furthermore, this function does not perform any I/O. This function
    /// merely maps the available data buffers or rearranges the data to ensure
    /// it is available as a linear mapping.
    ///
    /// If the underlying stream does not have sufficient data buffered, this
    /// will return [`ControlFlow::Break`](Flow::Break) with a value of
    /// [`More`] indicating how much data is needed. It is up to the caller to
    /// pass this information to the stream operators to ensure more data is
    /// made available.
    fn map_raw(&self, idx: usize, len: usize) -> Flow<Break, &[u8]>;

    /// Map limited data at a specific offset.
    ///
    /// This works like [`Self::map_raw()`] but will always limit the
    /// returned slice to a maximum of `len` bytes.
    fn map_at(&self, idx: usize, len: usize) -> Flow<Break, &[u8]> {
        let map = self.map_raw(idx, len)?;
        Flow::Continue(&map[..core::cmp::min(len, map.len())])
    }

    /// Map limited data.
    ///
    /// This works like [`Self::map_at()`] but uses an index of 0.
    fn map(&self, len: usize) -> Flow<Break, &[u8]> {
        self.map_at(0, len)
    }

    /// Read data at a specific offset.
    ///
    /// This works like [`Self::map_raw()`] but guarantees that the returned
    /// slice has a length of `len`. If the requested data is not available
    /// in linear memory, this will copy the data into a slice using repeated
    /// calls to [`Self::map_raw()`].
    fn read_at(&self, idx: usize, len: usize) -> Flow<Break, Map<'_>> {
        if idx.checked_add(len).is_none() {
            return Flow::Break(Err(Error::Overflow));
        }

        let mut map = self.map_at(idx, len)?;
        if map.len() >= len {
            return Flow::Continue(Map::new_borrowed(map));
        }

        let mut buf_u = alloc::boxed::Box::new_uninit_slice(len);
        let mut n: usize = 0;
        loop {
            let map_u = osi::mem::slice_as_uninit(map);
            let end = n.strict_add(map_u.len());
            buf_u[n..end].copy_from_slice(map_u);
            n = end;

            if n >= len {
                break;
            }

            map = self.map_at(idx + n, len - n)?;
        }

        // SAFETY: `buf_u` just got fully initialized.
        let buf = unsafe { buf_u.assume_init() };
        Flow::Continue(Map::new_owned(buf))
    }

    /// Read data from the stream.
    ///
    /// This works like [`Self::read_at()`] but uses an index of 0.
    fn read(&self, len: usize) -> Flow<Break, Map<'_>> {
        self.read_at(0, len)
    }
}

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

/// Read data from the stream for as long as the predicate indicates.
///
/// This extends [`Read::read_at()`] by reading buffered data for as long as
/// the provided predicate returns `true`. The predicate will be called for
/// each byte past the position given to this function. Once the predicate
/// returns `false`, the data starting from the passed index up until
/// (including) this position is returned.
///
/// The behavior otherwise matches [`Read::read_at()`].
///
/// `idx` is an offset relative to the current streaming position. All access
/// is performed relative to this index. If set to 0, all access is relative to
/// the current streaming position.
///
/// `max` is an optional maximum number of bytes to check. If set, the
/// operation will stop once `max` bytes have been processed, or the predicate
/// failed, whichever occurred first. If unset, the operation will only stop
/// when the predicate fails.
///
/// `n` is an offset relative to `idx` where to start running the predicate.
/// `n` is incremented each time the predicate is run and returned `true`. This
/// is useful to resume an operation that was interrupted with
/// [`ControlFlow::Break()`](core::ops::ControlFlow::Break), but avoid
/// restarting from the beginning.
///
/// On success, the returned mapping will start at `idx` and go up to
/// (including) the first byte that failed the predicate, or `idx+max` if the
/// maximum was set and reached. That is, all data that was accessed is
/// returned.
///
/// However, `n` will be set to the length of the mapping excluding a possible
/// trailing byte that failed the predicate.
///
/// Similar to [`Read::read_at()`] the mapping will be a copy if it is not
/// provided in linear memory. Otherwise, it is a simple borrowed slice.
pub fn read_at_while<'this, This, Predicate>(
    this: &'this This,
    idx: usize,
    n: &mut usize,
    max: Option<usize>,
    mut predicate: Predicate,
) -> Flow<Break, Map<'this>>
where
    This: ?Sized + Read,
    Predicate: FnMut(usize, u8) -> bool,
{
    loop {
        let mut map: &[u8];

        let len = if let Some(v) = max {
            if *n >= v {
                return this.read_at(idx, v);
            }
            v - *n
        } else {
            usize::MAX
        };

        let Some(from) = idx.checked_add(*n) else {
            return Flow::Break(Err(Error::Overflow));
        };

        map = this.map_raw(from, 1)?;
        map = &map[..core::cmp::min(len, map.len())];

        for i in 0..map.len() {
            let Some(pos) = n.checked_add(i) else {
                return Flow::Break(Err(Error::Overflow));
            };
            if !predicate(pos, map[i]) {
                *n = pos;
                return this.read_at(idx, pos + 1);
            }
        }

        *n = if let Some(v) = n.checked_add(map.len()) {
            v
        } else {
            return Flow::Break(Err(Error::Overflow));
        };
    }
}

/// Read data from the stream for as long as the predicate indicates.
///
/// This works like [`read_at_while()`], but with an index of 0.
pub fn read_while<'this, This, Predicate>(
    this: &'this This,
    n: &mut usize,
    max: Option<usize>,
    predicate: Predicate,
) -> Flow<Break, Map<'this>>
where
    This: ?Sized + Read,
    Predicate: FnMut(usize, u8) -> bool,
{
    read_at_while(this, 0, n, max, predicate)
}

impl<'this> dyn Read + 'this {
    /// Read data from the stream for as long as the predicate indicates.
    ///
    /// This works like [`read_at_while()`].
    pub fn read_at_while<Predicate>(
        &self,
        idx: usize,
        n: &mut usize,
        max: Option<usize>,
        predicate: Predicate,
    ) -> Flow<Break, Map<'_>>
    where
        Predicate: FnMut(usize, u8) -> bool,
    {
        read_at_while(self, idx, n, max, predicate)
    }

    /// Read data from the stream for as long as the predicate indicates.
    ///
    /// This works like [`read_at_while()`], but with an index of 0.
    pub fn read_while<Predicate>(
        &self,
        n: &mut usize,
        max: Option<usize>,
        predicate: Predicate,
    ) -> Flow<Break, Map<'_>>
    where
        Predicate: FnMut(usize, u8) -> bool,
    {
        read_while(self, n, max, predicate)
    }
}

impl Read for &[u8] {
    fn advance(&mut self, len: usize) {
        let v = core::mem::take(self);
        *self = &v[len..];
    }

    fn map_raw(&self, idx: usize, len: usize) -> Flow<Break, &[u8]> {
        match idx.checked_add(len) {
            None => Flow::Break(Err(Error::Overflow)),
            Some(v) => if v > self.len() {
                Flow::Break(Ok(More { idx: idx, len: len }))
            } else {
                Flow::Continue(&self[idx..])
            },
        }
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

    // This is a trivial implementation of vectored buffers. It does not cache
    // LRU positions, nor does it verify request lengths on short reads. This
    // is allowed, but suboptimal, yet suitable for tests.
    impl Read for &mut [&[u8]] {
        fn advance(&mut self, mut len: usize) {
            while len >= self[0].len() {
                len -= self[0].len();
                let v = core::mem::take(self);
                *self = &mut v[1..];
            }

            self[0] = &self[0][len..];
        }

        fn map_raw(&self, idx: usize, len: usize) -> Flow<Break, &[u8]> {
            let mut block_off = idx;
            let Some(block_id) = self.iter().position(|v| {
                if block_off < v.len() {
                    true
                } else {
                    block_off -= v.len();
                    false
                }
            }) else {
                if block_off == 0 && len == 0 {
                    return Flow::Continue(b"");
                } else {
                    return Flow::Break(Ok(More { idx: idx, len: len }));
                }
            };

            Flow::Continue(&self[block_id][block_off..])
        }
    }

    // This is a trivial implementation of vectored buffers. It does not cache
    // LRU positions, nor does it verify request lengths on short writes. This
    // is allowed, but suboptimal, yet suitable for tests.
    impl Write for (usize, [[Uninit<u8>; 2]; 16]) {
        unsafe fn commit(&mut self, len: usize) {
            self.0 = self.0.strict_add(len);
        }

        fn map_raw(&mut self, idx: usize, len: usize) -> Flow<Break, &mut [Uninit<u8>]> {
            let Some(mut block_off) = self.0.checked_add(idx) else {
                return Flow::Break(Err(Error::Overflow));
            };
            let Some(block_id) = self.1.iter().position(|v| {
                if block_off < v.len() {
                    true
                } else {
                    block_off -= v.len();
                    false
                }
            }) else {
                if block_off == 0 && len == 0 {
                    return Flow::Continue(&mut []);
                } else {
                    return Flow::Break(Ok(More { idx: idx, len: len }));
                }
            };

            Flow::Continue(&mut self.1[block_id][block_off..])
        }
    }

    // Verification of basic `Read` functionality using the trivial
    // implementation.
    #[test]
    fn read_basic() {
        let data: &[u8] = b"foobar!";
        let data_p: &dyn Read = &data;

        // Raw mappings must cover the maximum extents possible, but might not
        // even cover the requested length.
        let map = data_p.map_raw(0, 7).continue_value().unwrap();
        assert_eq!(map, b"foobar!");
        let map = data_p.map_raw(0, 0).continue_value().unwrap();
        assert_eq!(map, b"foobar!");
        let map = data_p.map_raw(3, 0).continue_value().unwrap();
        assert_eq!(map, b"bar!");
        let map = data_p.map_raw(7, 0).continue_value().unwrap();
        assert_eq!(map, b"");
        let e = data_p.map_raw(0, 8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 0, len: 8 }));
        let e = data_p.map_raw(3, 5).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 3, len: 5 }));

        // Explicit mappings must never exceed the extents.
        let map = data_p.map_at(0, 7).continue_value().unwrap();
        assert_eq!(map, b"foobar!");
        let map = data_p.map_at(0, 0).continue_value().unwrap();
        assert_eq!(map, b"");
        let map = data_p.map_at(3, 3).continue_value().unwrap();
        assert_eq!(map, b"bar");
        let map = data_p.map_at(3, 0).continue_value().unwrap();
        assert_eq!(map, b"");
        let e = data_p.map_at(0, 8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 0, len: 8 }));
        let e = data_p.map_at(3, 5).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 3, len: 5 }));

        // `map()` is just shorthand for `map_at(0, ..)`
        let map = data_p.map(3).continue_value().unwrap();
        assert_eq!(map, b"foo");
        let map = data_p.map(0).continue_value().unwrap();
        assert_eq!(map, b"");
        let e = data_p.map(8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 0, len: 8 }));

        // Reads must always be matched exactly.
        let map = data_p.read_at(0, 7).continue_value().unwrap();
        assert_eq!(map.deref(), b"foobar!");
        let map = data_p.read_at(0, 0).continue_value().unwrap();
        assert_eq!(map.deref(), b"");
        let map = data_p.read_at(3, 3).continue_value().unwrap();
        assert_eq!(map.deref(), b"bar");
        let map = data_p.read_at(3, 0).continue_value().unwrap();
        assert_eq!(map.deref(), b"");
        let e = data_p.read_at(0, 8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 0, len: 8 }));
        let e = data_p.read_at(3, 5).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 3, len: 5 }));

        // `read()` is just shorthand for `read_at(0, ..)`
        let map = data_p.read(3).continue_value().unwrap();
        assert_eq!(map.deref(), b"foo");
        let map = data_p.read(0).continue_value().unwrap();
        assert_eq!(map.deref(), b"");
        let e = data_p.read(8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 0, len: 8 }));
    }

    // Verification of basic `Read` functionality using the vectored
    // implementation.
    #[test]
    fn read_basic_vectored() {
        let data: &mut [&[u8]] = &mut [b"fo", b"o", b"ba", b"r!"];
        let data_p: &dyn Read = &data;

        // Raw mappings must cover the maximum extents possible, but might not
        // even cover the requested length.
        let map = data_p.map_raw(0, 7).continue_value().unwrap();
        assert_eq!(map, b"fo");
        let map = data_p.map_raw(0, 0).continue_value().unwrap();
        assert_eq!(map, b"fo");
        let map = data_p.map_raw(3, 0).continue_value().unwrap();
        assert_eq!(map, b"ba");
        let map = data_p.map_raw(0, 8).continue_value().unwrap();
        assert_eq!(map, b"fo");
        let map = data_p.map_raw(3, 5).continue_value().unwrap();
        assert_eq!(map, b"ba");
        let map = data_p.map_raw(7, 0).continue_value().unwrap();
        assert_eq!(map, b"");
        let e = data_p.map_raw(7, 1).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 7, len: 1 }));

        // Explicit mappings must never exceed the extents.
        let map = data_p.map_at(0, 7).continue_value().unwrap();
        assert_eq!(map, b"fo");
        let map = data_p.map_at(0, 0).continue_value().unwrap();
        assert_eq!(map, b"");
        let map = data_p.map_at(3, 3).continue_value().unwrap();
        assert_eq!(map, b"ba");
        let map = data_p.map_at(3, 0).continue_value().unwrap();
        assert_eq!(map, b"");
        let map = data_p.map_at(0, 8).continue_value().unwrap();
        assert_eq!(map, b"fo");
        let map = data_p.map_raw(7, 1).break_value().unwrap();
        assert_eq!(map, Ok(More { idx: 7, len: 1 }));

        // `map()` is just shorthand for `map_at(0, ..)`
        let map = data_p.map(3).continue_value().unwrap();
        assert_eq!(map, b"fo");
        let map = data_p.map(0).continue_value().unwrap();
        assert_eq!(map, b"");
        let map = data_p.map(8).continue_value().unwrap();
        assert_eq!(map, b"fo");

        // Reads must always be matched exactly.
        let map = data_p.read_at(0, 7).continue_value().unwrap();
        assert_eq!(map.deref(), b"foobar!");
        let map = data_p.read_at(0, 0).continue_value().unwrap();
        assert_eq!(map.deref(), b"");
        let map = data_p.read_at(3, 3).continue_value().unwrap();
        assert_eq!(map.deref(), b"bar");
        let map = data_p.read_at(3, 0).continue_value().unwrap();
        assert_eq!(map.deref(), b"");
        let e = data_p.read_at(0, 8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 7, len: 1 }));
        let e = data_p.read_at(3, 5).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 7, len: 1 }));

        // `read()` is just shorthand for `read_at(0, ..)`
        let map = data_p.read(3).continue_value().unwrap();
        assert_eq!(map.deref(), b"foo");
        let map = data_p.read(0).continue_value().unwrap();
        assert_eq!(map.deref(), b"");
        let e = data_p.read(8).break_value().unwrap();
        assert_eq!(e, Ok(More { idx: 7, len: 1 }));
    }

    // Verification of `read_at_while()`, using both the trivial and vectored
    // implementation.
    #[test]
    fn read_at_while() {
        let data_plain: &[u8] = b"foobar!";
        let data_vec: &mut [&[u8]] = &mut [b"fo", b"o", b"ba", b"r!"];
        let data_plain_p: &dyn Read = &data_plain;
        let data_vec_p: &dyn Read = &data_vec;

        for data_p in [data_plain_p, data_vec_p] {
            // Stop when reaching the maximum, even if it would match more.
            {
                let mut n = 0;
                let map = data_p.read_while(&mut n, Some(2),
                    |_, v| matches!(v, b'f' | b'o'),
                ).continue_value().unwrap();
                assert!(map.deref() == b"fo");
                assert_eq!(n, 2);

                // This time with an offset.
                let mut n = 0;
                let map = data_p.read_at_while(3, &mut n, Some(1),
                    |_, v| matches!(v, b'b' | b'a'),
                ).continue_value().unwrap();
                assert_eq!(map.deref(), b"b");
                assert_eq!(n, 1);
            }

            // Again, stop at the maximum and do not include the next byte,
            // especially when it would not match.
            {
                let mut n = 0;
                let map = data_p.read_while(&mut n, Some(3),
                    |_, v| matches!(v, b'f' | b'o'),
                ).continue_value().unwrap();
                assert!(map.deref() == b"foo");
                assert_eq!(n, 3);

                // This time with an offset.
                let mut n = 0;
                let map = data_p.read_at_while(3, &mut n, Some(2),
                    |_, v| matches!(v, b'b' | b'a'),
                ).continue_value().unwrap();
                assert_eq!(map.deref(), b"ba");
                assert_eq!(n, 2);
            }

            // Stop when the predicate fails, but include the failing byte in the
            // map, but not in the length.
            {
                let mut n = 0;
                let map = data_p.read_while(&mut n, Some(4),
                    |_, v| matches!(v, b'f' | b'o'),
                ).continue_value().unwrap();
                assert!(map.deref() == b"foob");
                assert_eq!(n, 3);

                // This time with an offset.
                let mut n = 0;
                let map = data_p.read_at_while(3, &mut n, Some(3),
                    |_, v| matches!(v, b'b' | b'a'),
                ).continue_value().unwrap();
                assert_eq!(map.deref(), b"bar");
                assert_eq!(n, 2);
            }

            // Same as before, but without maximum length.
            {
                let mut n = 0;
                let map = data_p.read_while(&mut n, None,
                    |_, v| matches!(v, b'f' | b'o'),
                ).continue_value().unwrap();
                assert!(map.deref() == b"foob");
                assert_eq!(n, 3);

                // This time with an offset.
                let mut n = 0;
                let map = data_p.read_at_while(3, &mut n, None,
                    |_, v| matches!(v, b'b' | b'a'),
                ).continue_value().unwrap();
                assert_eq!(map.deref(), b"bar");
                assert_eq!(n, 2);
            }
        }
    }

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
}
