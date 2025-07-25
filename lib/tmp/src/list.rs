//! Intrusive Circular Double Linked Lists
//!
//! XXX

use core::pin;

mod generic;

pub use generic::Link;

pub struct List<T, const OFFSET: usize> {
    list: generic::List,
    _t: core::marker::PhantomData<T>,
    _offset: core::marker::PhantomData<[(); OFFSET]>,
}

pub unsafe trait PinField<const OFFSET: usize, T>: Sized {
    fn pin_field_of_ptr(container: *const Self) -> *const T {
        // SAFETY:
        unsafe { container.byte_offset(OFFSET as isize).cast() }
    }

    fn pin_container_of_ptr(field: *const T) -> *const Self {
        // SAFETY:
        unsafe { field.byte_offset(-(OFFSET as isize)).cast() }
    }

    fn pin_field_of(container: pin::Pin<&Self>) -> pin::Pin<&T> {
        // SAFETY:
        unsafe { osi::pin::map_unchecked(container, |v| &*Self::pin_field_of_ptr(v)) }
    }

    unsafe fn pin_container_of(field: pin::Pin<&T>) -> pin::Pin<&Self> {
        // SAFETY:
        unsafe { osi::pin::map_unchecked(field, |v| &*Self::pin_container_of_ptr(v)) }
    }
}

pub trait IntoDeref: Sized + core::ops::Deref {
    fn into_deref<'a>(self) -> &'a Self::Target where Self: 'a;

    fn pin_into_deref<'a>(v: pin::Pin<Self>) -> pin::Pin<&'a Self::Target>
    where
        pin::Pin<Self>: 'a,
    {
        unsafe { osi::pin::map_unchecked(v, Self::into_deref) }
    }

    fn into_deref_ptr(self) -> *const Self::Target {
        &raw const *Self::into_deref(self)
    }

    fn pin_into_deref_ptr(v: pin::Pin<Self>) -> *const Self::Target {
        &raw const *Self::pin_into_deref(v)
    }
}

pub trait FromDeref: Sized + core::ops::Deref {
    unsafe fn from_deref(v: &Self::Target) -> Self;

    unsafe fn pin_from_deref(v: pin::Pin<&Self::Target>) -> pin::Pin<Self> {
        unsafe { osi::pin::map_unchecked(v, |v| Self::from_deref(v)) }
    }

    unsafe fn from_deref_ptr(v: *const Self::Target) -> Self {
        Self::from_deref(unsafe { &*v })
    }

    unsafe fn pin_from_deref_ptr(v: *const Self::Target) -> pin::Pin<Self> {
        Self::pin_from_deref(unsafe { pin::Pin::new_unchecked(&*v) })
    }
}

macro_rules! impl_into_from_deref {
    ($generic:ident, $base:ty, $fn_into:tt, $fn_from:tt $(,)?) => {
        impl<$generic> IntoDeref for $base {
            fn into_deref<'a>(self) -> &'a Self::Target where Self: 'a {
                $fn_into(self)
            }
        }

        impl<$generic> FromDeref for $base {
            unsafe fn from_deref(v: &Self::Target) -> Self {
                $fn_from(v)
            }
        }
    }
}

impl_into_from_deref!(
    T,
    &T,
    (core::convert::identity),
    (|v| unsafe { &*(v as *const _) }),
);
impl_into_from_deref!(
    T,
    alloc::boxed::Box<T>,
    (|v| unsafe { &*alloc::boxed::Box::into_raw(v) }),
    (|v| unsafe { alloc::boxed::Box::from_raw(v as *const _ as _) }),
);
impl_into_from_deref!(
    T,
    alloc::rc::Rc<T>,
    (|v| unsafe { &*alloc::rc::Rc::into_raw(v) }),
    (|v| unsafe { alloc::rc::Rc::from_raw(v) }),
);
impl_into_from_deref!(
    T,
    alloc::sync::Arc<T>,
    (|v| unsafe { &*alloc::sync::Arc::into_raw(v) }),
    (|v| unsafe { alloc::sync::Arc::from_raw(v) }),
);

impl<T, const OFFSET: usize> List<T, OFFSET>
where
    T: IntoDeref + FromDeref,
    T::Target: PinField<OFFSET, Link>,
{
    pub const fn new() -> Self {
        Self {
            list: generic::List::new(),
            _t: core::marker::PhantomData,
            _offset: core::marker::PhantomData,
        }
    }

    fn generic(self: pin::Pin<&Self>) -> pin::Pin<&generic::List> {
        unsafe { self.map_unchecked(|v| &v.list) }
    }

    fn generic_mut(self: pin::Pin<&mut Self>) -> pin::Pin<&mut generic::List> {
        unsafe { self.map_unchecked_mut(|v| &mut v.list) }
    }

    fn acquire<'a>(node: pin::Pin<T>) -> pin::Pin<&'a T::Target>
    where
        T: 'a,
    {
        IntoDeref::pin_into_deref(node)
    }

    unsafe fn release(node: pin::Pin<&T::Target>) -> pin::Pin<T> {
        unsafe { FromDeref::pin_from_deref(node) }
    }

    fn map(node: pin::Pin<&T::Target>) -> pin::Pin<&Link> {
        PinField::pin_field_of(node)
    }

    unsafe fn unmap(link: pin::Pin<&Link>) -> pin::Pin<&T::Target> {
        unsafe { PinField::pin_container_of(link) }
    }

    pub fn first(self: pin::Pin<&Self>) -> Option<pin::Pin<&T::Target>> {
        self.generic().first().map(|v| unsafe { Self::unmap(v) })
    }

    pub fn rotate(
        self: pin::Pin<&mut Self>,
        first: pin::Pin<&T::Target>,
    ) {
        self.generic_mut().rotate(Self::map(first)).unwrap();
    }

    pub fn link_before(
        self: pin::Pin<&mut Self>,
        anchor: pin::Pin<&T::Target>,
        v: pin::Pin<T>,
    ) {
        let v_node = Self::acquire(v);
        self.generic_mut().link_before(Self::map(anchor), Self::map(v_node)).unwrap();
    }

    pub fn link_after(
        self: pin::Pin<&mut Self>,
        anchor: pin::Pin<&T::Target>,
        v: pin::Pin<T>,
    ) {
        let v_node = Self::acquire(v);
        self.generic_mut().link_after(Self::map(anchor), Self::map(v_node)).unwrap();
    }

    pub fn link_first(
        self: pin::Pin<&mut Self>,
        v: pin::Pin<T>,
    ) {
        let v_node = Self::acquire(v);
        self.generic_mut().link_first(Self::map(v_node)).unwrap();
    }

    pub fn link_last(
        self: pin::Pin<&mut Self>,
        v: pin::Pin<T>,
    ) {
        let v_node = Self::acquire(v);
        self.generic_mut().link_last(Self::map(v_node)).unwrap();
    }

    pub fn unlink(
        self: pin::Pin<&mut Self>,
        v: pin::Pin<&T::Target>,
    ) -> pin::Pin<T> {
        let v_link = Self::map(v);
        let r_link = self.generic_mut().unlink(v_link).unwrap();
        unsafe { Self::release(Self::unmap(r_link)) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Node {
        v: u32,
        link: Link,
    }

    unsafe impl PinField<{osi::mem::typed_offset_of!(Node, link, Link)}, Link> for Node {
    }

    #[test]
    fn list_basic() {
        let list = List::new();
        let mut list = pin::pin!(list);

        {
            let n = Node { v: 71, link: Default::default() };
            let n = pin::pin!(n);
            let n = n.into_ref();

            list.as_mut().link_first(n);
            assert_eq!(list.as_ref().first().unwrap().v, 71);
            list.as_mut().unlink(n);

            assert_eq!(n.v, 71);
        }
    }
}
