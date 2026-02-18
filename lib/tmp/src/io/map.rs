/// # Mapped I/O Utilities
///
/// This module provides utilities to work with fully mapped data buffers. The
/// [`Read`] and [`Write`] abstractions allow consumers and producers to be
/// written independently without requiring knowledge about each other.

pub unsafe trait Read {
    fn len(&self) -> usize;
    fn map(&self, idx: usize) -> &[u8];
}

unsafe impl<const N: usize> Read for [u8; N] {
    fn len(&self) -> usize {
        N
    }

    fn map(&self, idx: usize) -> &[u8] {
        if idx < N {
            &self[idx..]
        } else {
            &[]
        }
    }
}

unsafe impl Read for [u8] {
    fn len(&self) -> usize {
        <[u8]>::len(self)
    }

    fn map(&self, idx: usize) -> &[u8] {
        if idx < self.len() {
            &self[idx..]
        } else {
            &[]
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// A trivial chunking implementation to test non-linear behavior.
    ///
    /// This simply uses a slice of statically sized chunks of data. It still
    /// has O(1) access, so can be used in benchmarks just fine. However, it
    /// has the downside that total length is always a multiple of the chunk
    /// size. That should be fine for testing, though.
    // SAFETY: Backing memory uses plain refs so it has no interior mutability.
    unsafe impl<const N: usize> Read for [[u8; N]] {
        fn len(&self) -> usize {
            <[[u8; N]]>::len(self).strict_mul(N)
        }

        fn map(&self, idx: usize) -> &[u8] {
            let vec_idx = idx / N;
            let vec_off = idx % N;

            if vec_idx < <[[u8; N]]>::len(self) {
                &self[vec_idx][vec_off..]
            } else {
                &[]
            }
        }
    }

    fn read_tests<T: ?Sized + Read>(read: &T, expect: &[u8]) {
        // Total length must always match.
        assert_eq!(read.len(), expect.len());

        // A mapping at every possible offset must match the expected
        // value, depending on the length of each mapping.
        for i in 0..read.len() {
            let v = read.map(i);
            assert_ne!(v.len(), 0);
            assert_eq!(v, &expect[i..i+v.len()]);
        }
    }

    #[test]
    fn read_dyn() {
        let data: [u8; _] = *b"foobar";
        let data_dyn: &dyn Read = &data;
        read_tests(data_dyn, b"foobar");
    }

    #[test]
    fn read_linear() {
        let data: &[u8] = b"foobar";
        read_tests(data, b"foobar");
    }

    #[test]
    fn read_vectored() {
        let data: &[[u8; 1]] = &[[b'f'], [b'o'], [b'o'], [b'b'], [b'a'], [b'r']];
        read_tests(data, b"foobar");
    }
}
