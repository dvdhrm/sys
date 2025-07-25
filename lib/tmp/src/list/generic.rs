//! Generic Intrusive Circular Double Linked Lists
//!
//! XXX

use core::{pin, ptr, sync::atomic};

#[derive(Clone, Copy, Debug)]
pub enum Error {
    LinkForeign,
}

#[derive(Debug, Default)]
pub struct Link {
    owner: atomic::AtomicPtr<()>,
    next: core::cell::Cell<Option<ptr::NonNull<Link>>>,
    prev: core::cell::Cell<Option<ptr::NonNull<Link>>>,
    _pin: core::marker::PhantomPinned,
}

pub struct List {
    first: Option<ptr::NonNull<Link>>,
    _pin: core::marker::PhantomPinned,
}

/// A virtual guard for a link associated with a list.
///
/// A `LinkGuard` is simply a wrapper around a `pin::Pin<&Link>`, but it states
/// that this link is part of a ring with branding `'ring`. This association is
/// encoded in the type-system and thus allows for safe operations on this
/// link.
///
/// As long as a `LinkGuard` exists for a link, this link will not be
/// disassociated from its list. This implies that the link will stay allocated
/// and cannot be associated with any other list.
///
/// Note that a `LinkGuard` does not mark a link as immutable, nor does it
/// provide mutable access. The `ListGuard` is what determines mutability of
/// all its associated links.
pub struct LinkGuard<'this, 'ring> {
    link: pin::Pin<&'this Link>,
    _ring: osi::marker::PhantomInvariantLifetime<'ring>,
}

/// A virtual guard for a ring of links associated with a list.
///
/// A `ListGuard` (here referred to as simply _guard_) is a meta-construct
/// associated with a list. It can be created for any list and allows encoding
/// the mutability and relationship between links and lists in the type system.
///
/// A guard is tied to a specific list, captures the mutability of this list,
/// and allows association links with the list. While a guard exists, any link
/// associated with it is guaranteed to stay associated with it (rather than
/// linked into another list).
///
/// Guards are used to safely operate on lists, and prevent erroneously mixing
/// links associated with different lists of the same type.
///
/// ## Mutability
///
/// Mutable access to a guard allows modifying the associated list and all its
/// links, while immutable access to a guard prevents any concurrent
/// modifications. To ensure this invariant, the creation of a guard captures
/// a matching reference to the list, and only yields a matching reference to
/// the caller (i.e., requesting a mutable guard captures a mutable list
/// reference, while an immutable guard captures an immutable list reference).
///
/// Furthermore, all guards are temporary. A constructor never returns a guard,
/// but instead takes a closure which it invokes. This closure then gets
/// exclusive access to the guard via a matching reference (mutable or
/// immutable, as requested).
///
/// ## Ring Branding
///
/// Every guard gets assigned a unique ring branding (which is represented by
/// the `'ring` lifetime), which cannot be forged or modified. The ring
/// branding represents a virtual ring of objects. Initially, this ring is
/// empty. But any link can be added to it by either claiming that it is
/// associated with the corresponding list, or by adding an unlinked one to the
/// ring. For each link that is part of the ring a `LinkGuard` with a matching
/// ring branding is provided.
///
/// Note that the virtual ring is not equivalent to the circular list of links.
/// Instead, the ring just represents a virtual collection of objects that this
/// code established association with. Most importantly, it encodes in the type
/// system that a given `LinkGuard` is associated with a specific `ListGuard`,
/// rather than with any other list of the same type.
pub struct ListGuard<'this, 'ring> {
    list: ptr::NonNull<List>,
    _list: core::marker::PhantomData<&'this mut List>,
    _ring: osi::marker::PhantomInvariantLifetime<'ring>,
}

impl Link {
    pub const fn new() -> Self {
        Self {
            owner: atomic::AtomicPtr::new(core::ptr::null_mut()),
            next: core::cell::Cell::new(None),
            prev: core::cell::Cell::new(None),
            _pin: core::marker::PhantomPinned,
        }
    }

    fn as_nonnull(&self) -> ptr::NonNull<Link> {
        osi::ptr::nonnull_from_ref(self)
    }

    pub fn is_unlinked(&self) -> bool {
        self.owner.load(atomic::Ordering::Relaxed).is_null()
    }

    pub fn is_linked(&self) -> bool {
        !self.is_unlinked()
    }
}

impl core::ops::Drop for Link {
    fn drop(&mut self) {
        if self.is_linked() {
            core::panic!("dropping a link that is still linked: {:?}", self);
        }
    }
}

impl List {
    pub const fn new() -> Self {
        Self {
            first: None,
            _pin: core::marker::PhantomPinned,
        }
    }

    fn guard<'this, Op, R>(
        self: pin::Pin<&'this Self>,
        op: Op,
    ) -> R
    where
        for<'ring> Op: FnOnce(&ListGuard<'this, 'ring>) -> R,
    {
        let ring = unsafe { ListGuard::new(self) };
        op(&ring)
    }

    fn guard_mut<'this, Op, R>(
        self: pin::Pin<&'this mut Self>,
        op: Op,
    ) -> R
    where
        for<'ring> Op: FnOnce(&mut ListGuard<'this, 'ring>) -> R,
    {
        let mut ring = unsafe { ListGuard::new_mut(self) };
        op(&mut ring)
    }

    pub fn first(
        self: pin::Pin<&Self>,
    ) -> Option<pin::Pin<&Link>> {
        self.guard(|ring| {
            ring.first().map(LinkGuard::into_inner)
        })
    }

    pub fn rotate(
        self: pin::Pin<&mut Self>,
        first: pin::Pin<&Link>,
    ) -> Result<(), Error> {
        self.guard_mut(|ring| {
            let r_first = ring.claim_linked(first)?;
            ring.set_first(Some(&r_first));
            Ok(())
        })
    }

    pub fn link_before(
        self: pin::Pin<&mut Self>,
        anchor: pin::Pin<&Link>,
        link: pin::Pin<&Link>,
    ) -> Result<(), Error> {
        self.guard_mut(|ring| {
            let r_anchor = ring.claim_linked(anchor)?;
            let r_link = ring.claim_unlinked(link)?;
            ring.link_after(&r_anchor.prev(), &r_link);
            ring.set_first_if(Some(&r_anchor), Some(&r_link));
            Ok(())
        })
    }

    pub fn link_after(
        self: pin::Pin<&mut Self>,
        anchor: pin::Pin<&Link>,
        link: pin::Pin<&Link>,
    ) -> Result<(), Error> {
        self.guard_mut(|ring| {
            ring.link_after(
                &ring.claim_linked(anchor)?,
                &ring.claim_unlinked(link)?,
            );
            Ok(())
        })
    }

    pub fn link_first(
        self: pin::Pin<&mut Self>,
        link: pin::Pin<&Link>,
    ) -> Result<(), Error> {
        self.guard_mut(|ring| {
            let r_link = ring.claim_unlinked(link)?;
            match ring.first() {
                None => ring.link_single(&r_link),
                Some(r_first) => {
                    ring.link_after(&r_first.prev(), &r_link);
                    ring.set_first(Some(&r_link));
                },
            }
            Ok(())
        })
    }

    pub fn link_last(
        self: pin::Pin<&mut Self>,
        link: pin::Pin<&Link>,
    ) -> Result<(), Error> {
        self.guard_mut(|ring| {
            let r_link = ring.claim_unlinked(link)?;
            match ring.first() {
                None => ring.link_single(&r_link),
                Some(r_first) => {
                    ring.link_after(&r_first.prev(), &r_link);
                },
            }
            Ok(())
        })
    }

    pub fn unlink<'a, 'b>(
        self: pin::Pin<&'a mut Self>,
        link: pin::Pin<&'b Link>,
    ) -> Result<pin::Pin<&'a Link>, Error>
    where
        'b: 'a,
    {
        self.guard_mut(|ring| {
            let r_link: LinkGuard<'a, '_> = ring.claim_linked(link)?;
            ring.unlink(&r_link);
            Ok(r_link.into_inner())
        })
    }
}

impl<'this, 'ring> LinkGuard<'this, 'ring> {
    unsafe fn new(link: ptr::NonNull<Link>) -> Self {
        Self {
            link: unsafe { pin::Pin::new_unchecked(link.as_ref()) },
            _ring: osi::marker::PhantomInvariantLifetime::new(),
        }
    }

    fn into_inner(self) -> pin::Pin<&'this Link> {
        self.link
    }

    fn as_nonnull(&self) -> ptr::NonNull<Link> {
        self.link.as_nonnull()
    }

    fn is_single(&self) -> bool {
        self.link.next.get() == Some(self.as_nonnull())
    }

    fn next(&self) -> LinkGuard<'this, 'ring> {
        unsafe { Self::new(self.next.get().unwrap()) }
    }

    fn prev(&self) -> LinkGuard<'this, 'ring> {
        unsafe { Self::new(self.prev.get().unwrap()) }
    }
}

impl<'this, 'ring> core::ops::Deref for LinkGuard<'this, 'ring> {
    type Target = Link;

    fn deref(&self) -> &Self::Target {
        &*self.link
    }
}

impl<'this, 'ring> ListGuard<'this, 'ring> {
    /// Create a new list guard for the given list.
    ///
    /// ## Safety
    ///
    /// 1) `list` must be a valid reference `&'this List` or `&'this mut List`.
    /// 2) If `list` is immutable, the caller must only grant immutable access
    ///    to this guard.
    /// 3) No two guards with the same ring branding must be created.
    unsafe fn with(list: ptr::NonNull<List>) -> Self {
        Self {
            list: list,
            _list: core::marker::PhantomData,
            _ring: osi::marker::PhantomInvariantLifetime::new(),
        }
    }

    /// Create a new immmutable list guard for the given list.
    ///
    /// ## Safety
    ///
    /// See [`Self::with`].
    unsafe fn new(list: pin::Pin<&'this List>) -> Self {
        // SAFETY: A pointer acquired from a reference cannot be null.
        let ptr = unsafe { ptr::NonNull::new_unchecked(osi::pin::as_ptr(list) as *mut _) };
        Self::with(ptr)
    }

    /// Create a new mutable list guard for the given list.
    ///
    /// ## Safety
    ///
    /// See [`Self::with`].
    unsafe fn new_mut(list: pin::Pin<&'this mut List>) -> Self {
        // SAFETY: A pointer acquired from a reference cannot be null.
        let ptr = unsafe { ptr::NonNull::new_unchecked(osi::pin::as_mut_ptr(list)) };
        Self::with(ptr)
    }

    fn owner(&self) -> *mut () {
        self.list.cast().as_ptr()
    }

    /// Claim a node as part of this ring.
    ///
    /// Provide a safe wrapper for the given node, claiming that it is part of
    /// this ring.
    ///
    /// ## Safety
    ///
    /// The caller must guarantee that `link` points to a link object that is
    /// valid for the duration of this call. Furthermore, `link` must be linked
    /// into the list protected by this `ListGuard`. In particular, it must
    /// have an owner-field matching `owner`.
    unsafe fn claim_unchecked(
        &self,
        link: ptr::NonNull<Link>,
    ) -> LinkGuard<'this, 'ring> {
        unsafe { LinkGuard::new(link) }
    }

    /// Claim a node as part of this ring.
    ///
    /// Provide a safe wrapper for the given node, claiming that it is part of
    /// this ring. If `link` is not linked into the list protected by this
    /// `ListGuard`, this will return an error.
    fn claim_linked<'link>(
        &self,
        link: pin::Pin<&'link Link>,
    ) -> Result<LinkGuard<'this, 'ring>, Error>
    where
        'link: 'this,
    {
        let owner = link.owner.load(atomic::Ordering::Relaxed);

        if owner != self.owner() {
            return Err(Error::LinkForeign);
        }

        debug_assert!(link.next.get().is_some());
        debug_assert!(link.prev.get().is_some());

        unsafe { Ok(self.claim_unchecked(link.as_nonnull())) }
    }

    fn claim_unlinked<'link>(
        &self,
        link: pin::Pin<&'link Link>,
    ) -> Result<LinkGuard<'this, 'ring>, Error>
    where
        'link: 'this,
    {
        link.owner.compare_exchange(
            core::ptr::null_mut(),
            self.owner(),
            atomic::Ordering::Relaxed,
            atomic::Ordering::Relaxed,
        ).map_err(|_| Error::LinkForeign {})?;

        debug_assert!(link.next.get().is_none());
        debug_assert!(link.prev.get().is_none());

        unsafe { Ok(self.claim_unchecked(link.as_nonnull())) }
    }

    fn first(&self) -> Option<LinkGuard<'this, 'ring>> {
        let f = unsafe { &self.list.as_ref().first };
        f.map(|v| unsafe { self.claim_unchecked(v) })
    }

    fn set_first(&mut self, link: Option<&LinkGuard<'this, 'ring>>) {
        let f = unsafe { &mut self.list.as_mut().first };
        *f = link.map(LinkGuard::as_nonnull);
    }

    fn set_first_if(
        &mut self,
        cmp: Option<&LinkGuard<'this, 'ring>>,
        link: Option<&LinkGuard<'this, 'ring>>,
    ) {
        let f = unsafe { &mut self.list.as_mut().first };
        if *f == cmp.map(LinkGuard::as_nonnull) {
            *f = link.map(LinkGuard::as_nonnull);
        }
    }

    fn link_single(
        &mut self,
        link: &LinkGuard<'this, 'ring>,
    ) {
        link.next.set(Some(link.as_nonnull()));
        link.prev.set(Some(link.as_nonnull()));
        self.set_first(Some(link));
    }

    fn link_after(
        &mut self,
        anchor: &LinkGuard<'this, 'ring>,
        link: &LinkGuard<'this, 'ring>,
    ) {
        let anchor_next = anchor.next();
        link.next.set(Some(anchor_next.as_nonnull()));
        link.prev.set(Some(anchor.as_nonnull()));
        anchor_next.prev.set(Some(link.as_nonnull()));
        anchor.next.set(Some(link.as_nonnull()));
    }

    fn unlink(
        &mut self,
        link: &LinkGuard<'this, 'ring>,
    ) {
        let next = link.next();
        let prev = link.prev();

        if link.is_single() {
            self.set_first(None);
        } else {
            prev.next.set(Some(next.as_nonnull()));
            next.prev.set(Some(prev.as_nonnull()));
            self.set_first_if(Some(&link), Some(&next));
        }

        link.next.set(None);
        link.prev.set(None);
        link.owner.store(
            ptr::null_mut(),
            atomic::Ordering::Relaxed,
        );
    }
}
