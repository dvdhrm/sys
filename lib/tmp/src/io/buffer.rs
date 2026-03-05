/// # I/O Buffers
///
/// This module provides utilities to work with I/O data buffers. The
/// [`Read`] and [`Write`] abstractions allow consumers and producers to be
/// written independently without requiring knowledge about each other.

use alloc::vec;

/// Abstraction to read from data buffers.
///
/// This trait can be implemented on data buffers and allows deserializers to
/// use those data buffers without requiring specific knowledge about the
/// buffers. The trait is similar to [`std::io::Read`] but purely operates on
/// buffered data rather than performing actual I/O.
///
/// Buffers do not necessarily provide data in linear memory, and this trait
/// reflects that. Consumers must be prepared to deal with scattered buffers.
///
/// Buffered data is constant and does not change while holding a shared
/// reference. However, buffer implementations can move, split, or merge data,
/// as long as the linearized data stays the same.
///
/// ## Safety
///
/// The implementation must ensure:
/// - guarantees of the individual methods are upheld.
/// - buffer data does not exhibit interior mutability and thus does not change
///   while holding a shared reference. Data is allowed to be moved or split,
///   as long as the linearized data will stay unchanged.
pub unsafe trait Read {
    /// Break value to indicate interruptions.
    ///
    /// This type is exclusively used to interrupt read operations on the
    /// buffer. It is meant to be propagated by a deserializer up to the
    /// caller, which can then react to it.
    ///
    /// Break values can have a well established meaning that a caller can make
    /// use of, or they can be opaque types that have to be forwarded to other
    /// traits on the buffer or underlying I/O transport layer.
    ///
    /// See [`Read::map()`] for the only user of this type.
    type Break: Sized;

    /// Return the length of available buffered data in bytes.
    ///
    /// The length correctly indicates the amount of data that can be mapped
    /// via [`Self::map()`] (possibly requiring multiple calls). The length
    /// must stay constant while holding a shared reference to the
    /// implementation.
    ///
    /// No call to [`Self::map()`] within the extents indicated by the length
    /// can fail. That is, buffered data must be readily available for repeated
    /// calls.
    fn len(&self) -> usize;

    /// Return a linear mapping of the buffered data at the specified index.
    ///
    /// This shall return the longest possible mapping at the specified index.
    /// It must return an empty slice if, and only if, the index points at or
    /// past the end of the buffer and no more data is expected to be pushed
    /// into the buffer.
    ///
    /// If the index points at or past the end of the buffer but more data is
    /// expected to be pushed into the buffer, an error with a value of
    /// `Break` is returned to indicate this. An error can also be returned for
    /// other reasons, depending on the choice for `Break`.
    ///
    /// Buffered data is not necessarily available in linear memory. Therefore,
    /// this function will not necessarily be able to return the full buffer.
    /// Repeated calls to increasing indices are needed to map the entire
    /// buffer.
    ///
    /// This addresses of the buffer data, as well as the data splits might
    /// change between calls, but the linearized data stays the same.
    ///
    /// ## Break Value
    ///
    /// The trait has an associated break type that is used to indicate
    /// interruptions. While it is used within a [`Result::Err`], it does not
    /// necessarily indicate a hard failure. Instead, it can be used to
    /// interrupt deserialization and propagate information up to the caller.
    ///
    /// This break value type can be freely chosen by the buffer
    /// implementation, but some choices have a well established semantic:
    ///
    /// [`Never`](osi::never::Never): This error type indicates that the
    /// implementation will never fail any requests. It is used by non-stream
    /// buffers that have a fixed size.
    ///
    /// [`Range`](core::ops::Range): This error type indicates that the
    /// implementation is based on a stream buffer and more data can be pulled
    /// in and made available. A return value of this type indicates that the
    /// mapping is at the end of the stream, but the stream is not sealed but
    /// expected to make more data available. The necessary operations to make
    /// more data available are outside the scope of this trait.
    fn map(&self, idx: usize) -> Result<&[u8], Self::Break>;
}

/// Abstraction to write into data buffers.
///
/// This trait can be implemented on data buffers and allows serializers to
/// use those data buffers without requiring specific knowledge about the
/// buffers. The trait is similar to [`std::io::Write`] but purely operates on
/// buffered data rather than performing actual I/O.
///
/// Buffers do not necessarily provide data in linear memory, and this trait
/// reflects that. Providers must be prepared to deal with scattered buffers.
///
/// Once written, buffered data is constant and does not change while holding a
/// shared reference. However, buffer implementations can move, split, or merge
/// data, as long as the linearized data stays the same.
///
/// ## Safety
///
/// The implementation must ensure:
/// - guarantees of the individual methods are upheld.
/// - buffer data does not exhibit interior mutability and thus does not change
///   while holding a shared reference. Data is allowed to be moved or split,
///   as long as the written linearized data will stay unchanged.
pub unsafe trait Write {
    /// Break value to indicate interruptions.
    ///
    /// This type is exclusively used to interrupt write operations on the
    /// buffer. It is meant to be propagated by a serializer up to the
    /// caller, which can then react to it.
    ///
    /// Break values can have a well established meaning that a caller can make
    /// use of, or they can be opaque types that have to be forwarded to other
    /// traits on the buffer or underlying I/O transport layer.
    ///
    /// A choice of [`!`](osi::never::Never`) indicates that the buffer cannot
    /// fail write operations and always keeps allocating suitable space for
    /// any request, panicking if out of memory, similar to
    /// [`Vec`](alloc::vec::Vec).
    ///
    /// Another common choice is [`core::ops::Range<usize>`] to indicate a
    /// request exceeds the extents of the buffer by the indicated range. It is
    /// up to the caller to then react suitably.
    type Break: Sized;

    /// Write partial data into the buffer.
    ///
    /// This copies data from `data` into the data buffer starting at relative
    /// index `idx`. It returns the amount of data that was written.
    ///
    /// This function never returns 0, nor does it return a value higher than
    /// the length of `data`.
    ///
    /// It is common for implementations with uninitialized buffers to
    /// zero-initialize any gaps before `idx`. That is, for best performance,
    /// writes should be serialized, unless zero-initialization is desired.
    ///
    /// If insufficient buffer space is available, the function will allocate
    /// more buffers (using `data.len()` as hint), if possible. If not
    /// possible, the function returns a suitable error code, depending on the
    /// choice for `Break`.
    fn partial_copy(&mut self, idx: usize, data: &[u8]) -> Result<usize, Self::Break>;

    /// Map the data buffers for writing.
    ///
    /// Map the data buffers at relative index `idx`. `len` is an indication of
    /// how much space is required. The implementation is free to return slices
    /// smaller or bigger than `len`.
    ///
    /// Empty slices are never returned.
    ///
    /// If insufficient buffer space is available, the function will allocate
    /// more buffers (using `len` as hint), if possible. If not possible, the
    /// function returns a suitable error code, depending on the choice for
    /// `Break`.
    ///
    /// Mapping uninitialized data will trigger a zero-initialization of that
    /// data.
    ///
    /// Written data can be read back from the returned slice for as long as a
    /// shared reference is held.
    fn map(&mut self, idx: usize, len: usize) -> Result<&mut [u8], Self::Break>;

    /// Copy data into the buffer.
    ///
    /// This copies data from `data` into the data buffer starting at relative
    /// index `idx`.
    ///
    /// Preferably, the function returns an error early in case insufficient
    /// space is available. However, an implementation is free to start copying
    /// partial data before returning an error.
    fn copy(&mut self, idx: usize, data: &[u8]) -> Result<(), Self::Break> {
        let mut off: usize = 0;
        while off < data.len() {
            let want = data.len() - off;
            let got = self.partial_copy(idx + off, &data[off..])?;
            assert!(got <= want);
            off += got;
        }
        Ok(())
    }
}

/// Abstraction to advance stream buffers as reader.
///
/// This trait extends [`Read`] and allows discarding buffer data from the
/// front. It is used to free up buffer space once a specific portion has
/// been processed and is no longer needed.
pub trait StreamRead: Read {
    /// Mark the front of the data buffer as consumed.
    ///
    /// This does not necessarily modify the data buffer, but might just mark
    /// the first `len` bytes as consumed. Any subsequent buffer operation will
    /// work as if the data was stripped from the buffer.
    fn consume(&mut self, len: usize);
}

/// Abstraction to advance stream buffers as writer.
///
/// This trait extends [`Write`] and allows committing buffer data at the
/// front once completely written. It is used to finalize serialization and
/// mark data ready to be processed or sent.
pub trait StreamWrite: Write {
    /// Mark the front of the data buffer as committed.
    ///
    /// This does not necessarily modify the data buffer, but might just mark
    /// the first `len` bytes as committed. Any subsequent buffer operation
    /// will work as if the data was stripped from the buffer.
    ///
    /// If any byte in the committed range was left uninitialized, this will
    /// initialize it to 0.
    fn commit(&mut self, len: usize);
}

/// A shared slice for reading from a data buffer.
///
/// [`Slice`] represents a non-linear data slice into an instance of [`Read`].
/// It can be thought of as the non-linear equivalent to `&[u8]`.
///
/// Since data buffers are not necessarily available linearly, [`Slice`] can be
/// used to treat the data as if it was available linearly. It provides
/// functions to copy data out of the buffers into linear memory.
#[derive(Clone, Debug, Hash)]
#[derive(Eq, PartialEq)]
pub struct Slice<'read, T: ?Sized> {
    read: &'read T,
    range: core::ops::Range<usize>,
}

impl<'read, T> Slice<'read, T>
where
    T: ?Sized + Read,
{
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
        Self { read, range }
    }

    /// Try creating a new slice with the given extents.
    ///
    /// Unlike [`Self::new()`] this will never panic but return `None` if the
    /// extents of `read` are exceeded.
    pub fn try_new(
        read: &'read T,
        range: core::ops::Range<usize>,
    ) -> Option<Self> {
        if range.len() == 0 {
            Some(Self { read, range })
        } else if range.start <= read.len() && range.end <= read.len() {
            Some(Self { read, range })
        } else {
            None
        }
    }

    /// Return the underlying buffer referenced by this slice.
    pub fn buffer(&self) -> &'read T {
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
                let map = self.read.map(v).unwrap_or_else(|_| panic!("remapping slice should not fail"));
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
    pub fn copy_uninit(&self, dst: &mut [core::mem::MaybeUninit<u8>]) {
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

    /// Copy data from the slice into a linear buffer.
    ///
    /// Data from the slice will be copied into the linear buffer `dst`. The
    /// destination buffer must be equal to, or shorter than, the size of the
    /// slice. If shorter, the data is truncated.
    ///
    /// ## Panics
    ///
    /// This function will panic if `dst` is longer than `self`.
    pub fn copy(&self, dst: &mut [u8]) {
        let mut n: usize = 0;
        while n < dst.len() {
            let map = self.map(n);
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

// SAFETY: No interior mutability is exhibited on `Self`.
unsafe impl<const N: usize> Read for [u8; N] {
    type Break = osi::never::Never;

    fn len(&self) -> usize {
        N
    }

    fn map(&self, idx: usize) -> Result<&[u8], Self::Break> {
        if idx < N {
            Ok(&self[idx..])
        } else {
            Ok(&[])
        }
    }
}

// SAFETY: No interior mutability is exhibited on `Self`.
unsafe impl Read for [u8] {
    type Break = osi::never::Never;

    fn len(&self) -> usize {
        <[u8]>::len(self)
    }

    fn map(&self, idx: usize) -> Result<&[u8], Self::Break> {
        if idx < <[u8]>::len(self) {
            Ok(&self[idx..])
        } else {
            Ok(&[])
        }
    }
}

// SAFETY: No interior mutability is exhibited on `Self`.
unsafe impl Write for vec::Vec<u8> {
    type Break = osi::never::Never;

    fn partial_copy(
        &mut self,
        idx: usize,
        data: &[u8],
    ) -> Result<usize, Self::Break> {
        let end = idx.strict_add(data.len());
        if end > self.len() {
            self.reserve(end - self.len());
        }

        // SAFETY: A shared reference cannot overlap with a mutable reference
        //     to a vector. Furthermore, data length was just verified or
        //     reserved and must thus be available. The range up to `idx` is
        //     zeroed, anything beyond is copied.
        unsafe {
            if idx > self.len() {
                self.as_mut_ptr()
                    .add(self.len())
                    .write_bytes(0u8, idx - self.len());
            }
            core::ptr::copy_nonoverlapping(
                data.as_ptr(),
                self.as_mut_ptr().add(idx),
                data.len(),
            );
            if end > self.len() {
                self.set_len(end);
            }
        }

        Ok(data.len())
    }

    fn map(
        &mut self,
        idx: usize,
        len: usize,
    ) -> Result<&mut [u8], Self::Break> {
        let end = idx.strict_add(len);
        if end > self.len() {
            let n = end - self.len();
            self.reserve(n);

            // SAFETY: The reserve was just extended to cover `n` bytes, up to
            //     index `end`.
            unsafe {
                self.spare_capacity_mut()
                    .as_mut_ptr()
                    .write_bytes(0u8, n);
                self.set_len(end);
            }
        }
        Ok(&mut self[idx..])
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // A forwarding implementation for allocated buffers.
    unsafe impl Read for alloc::boxed::Box<[u8]> {
        type Break = osi::never::Never;

        fn len(&self) -> usize {
            <[u8]>::len(self)
        }

        fn map(&self, idx: usize) -> Result<&[u8], Self::Break> {
            if idx < <[u8]>::len(self) {
                Ok(&self[idx..])
            } else {
                Ok(&[])
            }
        }
    }

    // A trivial chunking implementation that takes a buffer and chunk-size and
    // then pretends the buffer was only available in chunks of the given size.
    unsafe impl Read for (usize, &[u8]) {
        type Break = osi::never::Never;

        fn len(&self) -> usize {
            self.1.len()
        }

        fn map(&self, idx: usize) -> Result<&[u8], Self::Break> {
            if idx < self.1.len() {
                let end = idx.strict_add(self.0 - (idx % self.0));
                Ok(&self.1[idx..core::cmp::min(self.1.len(), end)])
            } else {
                Ok(&[])
            }
        }
    }

    // A trivial chunking implementation that takes a buffer and chunk-size and
    // then pretends the buffer was only available in chunks of the given size.
    unsafe impl Read for (usize, alloc::boxed::Box<[u8]>) {
        type Break = osi::never::Never;

        fn len(&self) -> usize {
            self.1.len()
        }

        fn map(&self, idx: usize) -> Result<&[u8], Self::Break> {
            if idx < self.1.len() {
                let end = idx.strict_add(self.0 - (idx % self.0));
                Ok(&self.1[idx..core::cmp::min(self.1.len(), end)])
            } else {
                Ok(&[])
            }
        }
    }

    // Helper function that takes a `Read` implementation and its expected
    // linearized data, and then verifies that the implementation. It also
    // verifies that `Slice` works as expected for the implementation.
    //
    // This runs O(n^2) with `n = expect.len()`. Use it only with short
    // buffers.
    fn test_read<T>(read: &T, expect: &[u8])
    where
        T: ?Sized + Read<Break = osi::never::Never>,
    {
        // Total length must match.
        assert_eq!(read.len(), expect.len());

        // Any mapping at a valid offset must match expected data.
        for i in 0..expect.len() {
            let v = read.map(i).unwrap();
            assert_ne!(v.len(), 0);
            assert_eq!(v, &expect[i..i+v.len()]);
        }

        // Reading past the end must yield empty slices.
        for i in expect.len()..expect.len()+8 {
            assert_eq!(read.map(i).unwrap(), &[]);
        }

        // Create all possible slices
        for i in 0..=expect.len() {
            for j in i..=expect.len() {
                let v = Slice::new(read, i..j);
                assert_eq!(&*v.read(), &expect[i..j]);
            }
        }
    }

    // Verify that `Read` is dyn-compatible.
    #[test]
    fn read_dyn() {
        let data = [0u8; 8];
        let r: &dyn Read<Break = osi::never::Never> = &data;

        assert_eq!(r.map(0).unwrap(), &data);
    }

    // Verify the `Read` implementation on `[u8; N]`.
    #[test]
    fn impl_read_u8_array() {
        let v: [u8; _] = [];
        test_read(&v, b"");
        let v: [u8; _] = [b'f', b'o', b'o', b'b', b'a', b'r'];
        test_read(&v, b"foobar");
    }

    // Verify the `Read` implementation on `&[u8]`.
    #[test]
    fn impl_read_u8_slice() {
        let v: &[u8] = &[];
        test_read(v, b"");
        let v: &[u8] = &[b'f', b'o', b'o', b'b', b'a', b'r'];
        test_read(v, b"foobar");
    }

    // Verify the test-only chunked `Read` implementation.
    #[test]
    fn impl_read_chunk_test() {
        let v: (usize, &[u8]) = (2, &[]);
        test_read(&v, b"");
        let v: (usize, &[u8]) = (1, &[b'f', b'o', b'o', b'b', b'a', b'r']);
        test_read(&v, b"foobar");
        let v: (usize, &[u8]) = (2, &[b'f', b'o', b'o', b'b', b'a', b'r']);
        test_read(&v, b"foobar");
        let v: (usize, &[u8]) = (7, &[b'f', b'o', b'o', b'b', b'a', b'r']);
        test_read(&v, b"foobar");
    }

    // Verify that `Write` is dyn-compatible.
    #[test]
    fn write_dyn() {
        let mut data = alloc::vec::Vec::<u8>::new();
        let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

        assert_eq!(w.map(8, 2).unwrap(), [0u8, 0]);
        assert_eq!(data, [0u8; 10]);
    }

    // Verify the `Write` implementation on `Vec<u8>`.
    #[test]
    fn impl_write_vec() {
        // Verify 0-initialization of empty buffers when mapping.
        {
            let mut data = alloc::vec::Vec::<u8>::new();
            let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

            assert_eq!(w.map(8, 2).unwrap(), [0u8, 0]);
            assert_eq!(data, [0u8; 10]);
        }

        // Verify 0-initialization of partial buffers when mapping.
        {
            let mut data = alloc::vec![0xffu8; 4];
            let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

            assert_eq!(w.map(8, 2).unwrap(), [0u8, 0]);
            assert_eq!(data, [0xffu8, 0xff, 0xff, 0xff, 0, 0, 0, 0, 0, 0]);
        }

        // Verify data-retention of buffers when mapping.
        {
            let mut data = alloc::vec![0xffu8; 4];
            let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

            assert_eq!(w.map(1, 2).unwrap()[0..2], [0xffu8; 2]);
            assert_eq!(data, [0xffu8, 0xff, 0xff, 0xff]);
        }

        // Verify overwriting partial data.
        {
            let mut data = alloc::vec![0xffu8; 4];
            let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

            w.copy(1, &[0; 2]).unwrap();
            assert_eq!(data, [0xffu8, 0, 0, 0xff]);
        }

        // Verify appending data overlapping the end.
        {
            let mut data = alloc::vec![0xffu8; 4];
            let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

            w.copy(2, &[0; 4]).unwrap();
            assert_eq!(data, [0xffu8, 0xff, 0, 0, 0, 0]);
        }

        // Verify appending data past the end.
        {
            let mut data = alloc::vec![0xffu8; 4];
            let w: &mut dyn Write<Break = osi::never::Never> = &mut data;

            w.copy(6, &[0; 2]).unwrap();
            assert_eq!(data, [0xffu8, 0xff, 0xff, 0xff, 0, 0, 0, 0]);
        }
    }
}
