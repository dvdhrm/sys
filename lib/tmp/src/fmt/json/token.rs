//! # JSON Tokens
//!
//! The main type of this module is [`Dec`], a JSON tokenizer. It takes a
//! data-stream and turns it into a token-stream. [`Dec`] operates on arbitrary
//! data buffers via [`io::buffer::Read`](crate::io::buffer::Read) and borrows
//! token data from the buffers rather than copying it (if possible).
//!
//! [`Dec`] is a scoped decoder which either operates on fully mapped data or
//! on streamed data. [`BufferDec`] is the top-level decoder for fully mapped
//! data, [`StreamDec`] is the top-level decoder for streamed data. Both of
//! them provide access to a scoped [`Dec`] to perform the actual tokenization.
//!
//! ## Examples
//!
//! The following example takes a data buffer and decodes a single JSON token
//! returning it back to the caller as a report.
//!
//! ```rust
//! use tmp::fmt::json::token;
//! use tmp::io;
//!
//! fn decode(
//!     buffer: &dyn io::buffer::Read<Break = osi::never::Never>,
//! ) -> Option<token::Report<'_, dyn io::buffer::Read<Break = osi::never::Never> + '_>> {
//!     let mut dec = token::BufferDec::with(buffer);
//!     let mut scope = dec.dec();
//!     match scope.next() {
//!         Ok(v) => v,
//!     }
//! }
//! ```
//!
//! ## Deviations
//!
//! This implementation deviates from standards in the following ways:
//! - Mandatory Utf-8: While the JSON standard does not enforce any transport
//!   level encoding, this implementation requires Utf-8. Other encodings
//!   are not supported, yet gracefully handled via the error tokens.
//! - Unicode NonCharacters: I-JSON rejects NonCharacters in data
//!   encodings and string escapes (RFC 7493:2.1). This contradicts the
//!   Unicode standard (Corrigendum #9: Clarification About Noncharacters).
//!   This implementation explicitly allows NonCharacters and follows the
//!   Unicode standard.
//!   This restriction is specific to I-JSON, not JSON.

// XXX: The following improvements are planned for this implementation:
//   - Provide span-information for items, especially errors.
//   - Add better handling of non-JSON syntax to improve error reporting.

use core::mem::MaybeUninit as Uninit;

use crate::io;

/// Enumeration of all errors that can be raised by the tokenizer.
///
/// Errors are raised alongside tokens, rather than separately. The tokenizer
/// can always recover from errors and continue parsing. However, the resulting
/// data will likely be corrupted and should be used only for diagnostics.
///
/// Reports always include the raw data alongside an error. The raw data is
/// provided up until the point where the error occurred.
///
/// Errors are generally raised inline while parsing a token. All errors can be
/// recovered from, and the erroneous data is fixed up suitably by any ongoing
/// token that is parsed. The only exception is [`Error::TokenUnknown`], which
/// represents data that could not be fixed by an ongoing token, and is thus a
/// token by itself.
///
/// Thus, if the raw data-stream needs to be recreated from a token stream, all
/// errors except [`Error::TokenUnknown`] can be ignored. The concatenation of
/// the raw data of all tokens and unknown tokens fully represents the original
/// data.
#[derive(Clone, Copy, Debug, Hash)]
#[derive(Eq, PartialEq)]
#[non_exhaustive]
pub enum Error {
    /// An unknown token was found in the token stream. The raw report data
    /// represents the token and the data will be skipped.
    TokenUnknown,
    /// Integer part of a number has a leading 0.
    NumberIntegerLeadingZero,
    /// Fraction part of a number is empty.
    NumberFractionEmpty,
    /// A string was left unterminated.
    StringIncomplete,
    /// A control code was left unescaped in a string.
    StringControlCode,
    /// String with non-UTF8 data.
    StringUtf8Invalid,
    /// Syntactically invalid escape sequence (eq., unicode escape not followed
    /// by 4 hex-characters.
    StringEscapeInvalid,
    /// Unknown single-character escape sequence
    StringEscapeUnknown,
    /// Unicode lead surrogate escape sequence without following trail
    /// surrogate escape sequence.
    StringEscapeUnpairedLeadSurrogate,
    /// Unicode trail surrogate escape sequence without leading lead surrogate
    /// escape sequence.
    StringEscapeUnpairedTrailSurrogate,
}

/// Encodes the sign of a number.
#[derive(Clone, Copy, Debug, Default, Hash)]
#[derive(Eq, Ord, PartialEq, PartialOrd)]
pub enum Sign {
    /// The plus sign (`+`)
    #[default]
    Plus,
    /// The minus sign (`-`)
    Minus,
}

#[derive(Clone, Debug, Hash)]
#[derive(Eq, PartialEq)]
enum NumberInner {
    Any {
        integer: (Sign, core::ops::Range<usize>),
        fraction: Option<core::ops::Range<usize>>,
        exponent: Option<(Sign, core::ops::Range<usize>)>,
    },
    Int64 {
        value: i64,
    },
}

/// A token representing a number.
#[derive(Clone, Debug, Hash)]
#[derive(Eq, PartialEq)]
pub struct Number {
    inner: NumberInner,
}

/// Enumeration of all possible tokens that can be raised by the tokenizer.
///
/// This type does not provide metadata for each token, but is a mere
/// enumeration of the possible tokens. Metadata is provided by a [`Report`].
#[derive(Clone, Debug, Hash)]
#[derive(Eq, PartialEq)]
pub enum Token {
    /// Non-significant whitespace
    Whitespace,
    /// A colon character (`:`)
    Colon,
    /// A comma character (`,`)
    Comma,
    /// An opening array character (`[`)
    ArrayOpen,
    /// A closing array character (`]`)
    ArrayClose,
    /// An opening object character (`{`)
    ObjectOpen,
    /// A closing object character (`}`)
    ObjectClose,
    /// Null token (`null`)
    Null,
    /// False token (`false`)
    False,
    /// True token (`true`)
    True,
    /// Number token
    Number(Number),
    /// String token
    String,
}

/// XXX
#[derive(Clone, Debug)]
#[derive(Eq, PartialEq)]
pub struct Report<'data, Stream: ?Sized> {
    pub raw: io::buffer::Slice<'data, Stream>,
    pub acc: Option<alloc::boxed::Box<[u8]>>,
    pub token: Result<Token, Error>,
}

#[derive(Clone, Debug)]
enum State {
    None,
    Whitespace,
    Keyword {
        id: &'static [u8],
    },
    Item,
    IntegerSign {
        sign: Sign,
    },
    IntegerZero {
        sign: Sign,
        start: usize,
    },
    IntegerRange {
        sign: Sign,
        range: core::ops::Range<usize>,
        value: Option<i64>,
    },
    FractionSymbol {
        integer: (Sign, core::ops::Range<usize>),
    },
    FractionRange {
        integer: (Sign, core::ops::Range<usize>),
        fraction: core::ops::Range<usize>,
    },
    ExponentSymbol {
        integer: (Sign, core::ops::Range<usize>),
        fraction: Option<core::ops::Range<usize>>,
    },
    ExponentSign {
        integer: (Sign, core::ops::Range<usize>),
        fraction: Option<core::ops::Range<usize>>,
        sign: Sign,
    },
    ExponentRange {
        integer: (Sign, core::ops::Range<usize>),
        fraction: Option<core::ops::Range<usize>>,
        exponent: (Sign, core::ops::Range<usize>),
    },
    String,
    StringUtf8 {
        start: usize,
        len: usize,
        data: [u8; 4],
    },
    StringEscape,
    StringEscapeUnicode {
        start: usize,
        code: u32,
    },
    StringEscapeUnicodeTrail {
        start: usize,
        code: u32,
        lead: u32,
    },
}

#[derive(Clone, Debug)]
struct DecEngine {
    state: State,
    offset: usize,
    idx: usize,
    acc_str: alloc::vec::Vec<Uninit<u8>>,
    acc_off: usize,
    acc_idx: usize,
}

/// Scoped JSON tokenizer.
///
/// This is the actual implementation of the tokenizer. It can be obtained
/// from [`BufferDec`] or [`StreamDec`] and borrows their buffers for a
/// specific scope, so token reports have a fixed lifetime.
///
/// A scoped tokenizer mutably borrows its origin, where the tokenizer engine
/// and all state is located. Advancing the scoped tokenizer will affect its
/// origin. If branching is desired, a copy of [`BufferDec`] can be created
/// to get two independent engines at any point in time.
#[derive(Debug)]
pub struct Dec<'engine, 'read, Buffer: ?Sized> {
    engine: &'engine mut DecEngine,
    read: &'read Buffer,
}

/// JSON tokenizer for fully mapped buffers.
///
/// This tokenizer takes a fully mapped data buffer and tokenizes its full
/// content. The tokens will borrow the data from the data buffer, rather
/// than copying it, unless decoding is necessary (e.g., JSON strings with
/// escape sequences are decoded to their respective raw representation).
///
/// This tokenizer can be cloned, in which case the cloned tokenizer will
/// retain the position and metadata of the original at the time of clone,
/// but otherwise both are fully independent.
#[derive(Clone, Debug)]
pub struct BufferDec<'read, Buffer: ?Sized> {
    engine: DecEngine,
    read: &'read Buffer,
}

/// JSON tokenizer for streaming buffers.
///
/// Unlike [`BufferDec`], this tokenizer takes a streaming buffer and can
/// discard data from the buffer once it has been tokenized. This allows
/// tokenizing large streams of data without holding everything in memory
/// at once.
///
/// Note that an individual token must always be fully held in memory to
/// be tokenized. Thus, if a JSON stream contains strings of great length,
/// even the streaming tokenizer will hold the full string in memory. A
/// validation-only tokenizer could avoid this, but that is outside the
/// scope of this implementation.
#[derive(Debug)]
pub struct StreamDec<'read, Stream: ?Sized> {
    engine: DecEngine,
    read: &'read mut Stream,
}

fn a2u(ascii: u8, radix: u32) -> u32 {
    // SAFETY: 7-bit ASCII is always a valid char.
    let v = unsafe { char::from_u32_unchecked((ascii & 0x7f) as u32) };
    v.to_digit(radix).unwrap()
}

impl Sign {
    // Apply a sign to a 32-bit integer. In case of integers greater than
    // `i32::MAX+1`, this can overflow an `i32` into an `i64`.
    fn mul32(self, v: u32) -> i64 {
        match self {
            Self::Plus => v.into(),
            Self::Minus => -(i64::from(v)),
        }
    }
}

impl Number {
    fn new_any(
        integer: (Sign, core::ops::Range<usize>),
        fraction: Option<core::ops::Range<usize>>,
        exponent: Option<(Sign, core::ops::Range<usize>)>,
    ) -> Self {
        Self {
            inner: NumberInner::Any {
                integer: integer,
                fraction: fraction,
                exponent: exponent,
            },
        }
    }

    fn new_int64(v: i64) -> Self {
        Self {
            inner: NumberInner::Int64 {
                value: v,
            },
        }
    }
}

impl<'data, Stream> Report<'data, Stream>
where
    Stream: ?Sized + io::buffer::Read,
{
    pub fn is_token(&self) -> bool {
        self.token.is_ok()
    }

    pub fn is_error(&self) -> bool {
        self.token.is_err()
    }

    pub fn get(&self) -> Result<&Token, Error> {
        match self.token.as_ref() {
            Ok(v) => Ok(v),
            Err(e) => Err(*e),
        }
    }

    pub fn token(&self) -> Option<&Token> {
        match self.token.as_ref() {
            Ok(v) => Some(v),
            Err(_) => None,
        }
    }

    pub fn error(&self) -> Option<Error> {
        match self.token {
            Ok(_) => None,
            Err(e) => Some(e),
        }
    }

    #[cfg(test)]
    fn assert_eq(
        &self,
        raw: &[u8],
        acc: Option<&str>,
        token: Result<Token, Error>,
    ) {
        assert_eq!(self.token, token);
        assert_eq!(&*self.raw.read(), raw);
        assert_eq!(
            self.acc.as_ref().map(|v| &**v),
            acc.map(|v| v.as_bytes()),
        );
    }
}

impl State {
    fn is_none(&self) -> bool {
        matches!(self, &State::None)
    }
}

impl DecEngine {
    fn new() -> Self {
        Self {
            state: State::None,
            offset: 0,
            idx: 0,
            acc_str: alloc::vec::Vec::new(),
            acc_off: 0,
            acc_idx: 0,
        }
    }

    /// Initialize the string accumulator, if not done so already.
    ///
    /// This will ensure the string accumulator is sufficiently sized. It will
    /// also reserve the first `len` elements of the accumulator to be
    /// initialized by the report handler.
    ///
    /// If the accumulator is already initialized, this is a no-op.
    fn str_maybe_init(&mut self, len: usize) {
        if self.acc_idx == 0 {
            self.acc_str.reserve(len);
            // SAFETY: Space was sufficiently allocated, and the vector stores
            //     `MaybeUninit<u8>`, which is always considered initialized.
            unsafe { self.acc_str.set_len(len) };
            self.acc_off = len;
            self.acc_idx = len;
        }
    }

    /// Push a slice into the string accumulator and advance the accumulator
    /// position to the current position.
    fn str_push(&mut self, v: &[u8]) {
        self.acc_str.extend_from_slice(osi::mem::slice_as_uninit(v));
        self.acc_idx = self.idx;
    }

    /// Push a slice into the string accumulator and advance the accumulator
    /// position to the current position, but only if the accumulator is in
    /// use.
    fn str_sync(&mut self, v: &[u8]) {
        if self.acc_idx != 0 {
            self.str_push(v);
        }
    }

    /// Push a character into the string accumulator and advance the
    /// accumulator position to the current position.
    ///
    /// Works like [`Self::str_push()`] but takes a Unicode Scalar Value.
    fn str_push_char(&mut self, v: char) {
        self.str_push(v.encode_utf8(&mut [0; 4]).as_bytes())
    }

    /// Push a dummy character into the string accumulator and advance the
    /// accumulator position to the current position.
    ///
    /// Works like [`Self::str_push()`] but inserts the Unicode Replacement
    /// Character.
    fn str_push_dummy(&mut self) {
        self.str_push_char(char::REPLACEMENT_CHARACTER)
    }

    /// Advance the state machine by a single step.
    ///
    /// This takes the next character at position `self.idx` as argument, or
    /// `None` if at the end of the stream. The state machine is updated and
    /// one of the following is returned:
    ///
    /// - `Ok(None)`: The state machine stepped successfully but there is
    ///   nothing to report.
    /// - `Ok(Some(v))`: The state machine stepped successfully and completed
    ///   a token. The token is specified as `v`.
    /// - `Err((e, off))`: The state machine stepped successfully but reported
    ///   a tokenization error. The error is specified as `e` and it occurred
    ///   `off` bytes before `self.idx`.
    ///
    /// The stepper usually advances `self.idx` by 1 or leaves it unmodified if
    /// a state change occurred and the character needs to be handled by the
    /// next state. However, the stepper can backtrack arbitrarily by
    /// decreasing `self.idx` by any amount.
    ///
    /// The caller must re-evaluate `self.idx` after each step and correctly
    /// pass the requested character in the next step.
    fn step(
        &mut self,
        next: Option<u8>,
    ) -> Result<Option<Token>, (Error, usize)> {
        match (&mut self.state, next) {
            /*
             * None
             */

            (State::None, Some(b' ' | b'\n' | b'\r' | b'\t')) => {
                self.idx += 1;
                self.state = State::Whitespace;
                Ok(None)
            },

            (State::None, Some(b':')) => {
                self.idx += 1;
                Ok(Some(Token::Colon))
            },

            (State::None, Some(b',')) => {
                self.idx += 1;
                Ok(Some(Token::Comma))
            },

            (State::None, Some(b'[')) => {
                self.idx += 1;
                Ok(Some(Token::ArrayOpen))
            },

            (State::None, Some(b']')) => {
                self.idx += 1;
                Ok(Some(Token::ArrayClose))
            },

            (State::None, Some(b'{')) => {
                self.idx += 1;
                Ok(Some(Token::ObjectOpen))
            },

            (State::None, Some(b'}')) => {
                self.idx += 1;
                Ok(Some(Token::ObjectClose))
            },

            (State::None, Some(b'n')) => {
                self.idx += 1;
                self.state = State::Keyword { id: b"null" };
                Ok(None)
            },

            (State::None, Some(b'f')) => {
                self.idx += 1;
                self.state = State::Keyword { id: b"false" };
                Ok(None)
            },

            (State::None, Some(b't')) => {
                self.idx += 1;
                self.state = State::Keyword { id: b"true" };
                Ok(None)
            },

            (State::None, Some(b'_' | b'a'..=b'z' | b'A'..=b'Z')) => {
                self.idx += 1;
                self.state = State::Item;
                Ok(None)
            },

            (State::None, Some(b'-')) => {
                self.idx += 1;
                self.state = State::IntegerSign { sign: Sign::Minus };
                Ok(None)
            },

            (State::None, Some(b'0')) => {
                self.idx += 1;
                self.state = State::IntegerZero {
                    sign: Sign::Plus,
                    start: self.idx - 1,
                };
                Ok(None)
            },

            (State::None, Some(ch @ b'1'..=b'9')) => {
                self.idx += 1;
                self.state = State::IntegerRange {
                    sign: Sign::Plus,
                    range: self.idx-1..self.idx,
                    value: Some(a2u(ch, 10).into()),
                };
                Ok(None)
            },

            (State::None, Some(b'"')) => {
                self.idx += 1;
                self.state = State::String;
                Ok(None)
            },

            (State::None, Some(_)) => {
                self.idx += 1;
                Err((Error::TokenUnknown, 0))
            },

            (State::None, None) => {
                Ok(None)
            },

            /*
             * Whitespace
             */

            (State::Whitespace, Some(b' ' | b'\n' | b'\r' | b'\t')) => {
                self.idx += 1;
                Ok(None)
            },

            (State::Whitespace, Some(_)) => {
                Ok(Some(Token::Whitespace))
            },

            (State::Whitespace, None) => {
                Ok(Some(Token::Whitespace))
            },

            /*
             * Keyword
             */

            (&mut State::Keyword { id }, ch @ _) => {
                if self.idx < id.len() && ch == Some(id[self.idx]) {
                    self.idx += 1;
                    Ok(None)
                } else if
                    self.idx == id.len()
                    && !matches!(ch, Some(b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z'))
                {
                    match id {
                        b"null" => Ok(Some(Token::Null)),
                        b"false" => Ok(Some(Token::False)),
                        b"true" => Ok(Some(Token::True)),
                        _ => core::unreachable!(),
                    }
                } else if ch.is_some() {
                    self.idx += 1;
                    self.state = State::Item;
                    Ok(None)
                } else {
                    self.state = State::None;
                    Err((Error::TokenUnknown, 0))
                }
            },

            /*
             * Item
             */

            (State::Item, Some(b'_' | b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z')) => {
                self.idx += 1;
                Ok(None)
            },

            (State::Item, _) => {
                self.state = State::None;
                Err((Error::TokenUnknown, 0))
            },

            /*
             * IntegerSign
             */

            (State::IntegerSign { sign }, Some(b'0')) => {
                self.idx += 1;
                self.state = State::IntegerZero {
                    sign: *sign,
                    start: self.idx - 1,
                };
                Ok(None)
            },

            (State::IntegerSign { sign }, Some(ch @ b'1'..=b'9')) => {
                self.idx += 1;
                self.state = State::IntegerRange {
                    sign: *sign,
                    range: self.idx-1..self.idx,
                    value: Some(sign.mul32(a2u(ch, 10))),
                };
                Ok(None)
            },

            (State::IntegerSign { .. }, _) => {
                self.state = State::None;
                Err((Error::TokenUnknown, 0))
            },

            /*
             * IntegerZero
             */

            (State::IntegerZero { start, .. }, Some(b'0')) => {
                self.idx += 1;
                if self.idx > *start + 2 {
                    Ok(None)
                } else {
                    Err((Error::NumberIntegerLeadingZero, 0))
                }
            },

            (&mut State::IntegerZero { sign, start }, Some(ch @ b'1'..=b'9')) => {
                self.idx += 1;
                self.state = State::IntegerRange {
                    sign: sign,
                    range: self.idx-1..self.idx,
                    value: Some(sign.mul32(a2u(ch, 10))),
                };
                if self.idx > start + 2 {
                    Ok(None)
                } else {
                    Err((Error::NumberIntegerLeadingZero, 0))
                }
            },

            (State::IntegerZero { sign, .. }, Some(b'.')) => {
                self.idx += 1;
                self.state = State::FractionSymbol {
                    integer: (*sign, self.idx-2..self.idx-1),
                };
                Ok(None)
            },

            (State::IntegerZero { sign, .. }, Some(b'e' | b'E')) => {
                self.idx += 1;
                self.state = State::ExponentSymbol {
                    integer: (*sign, self.idx-2..self.idx-1),
                    fraction: None,
                };
                Ok(None)
            },

            (State::IntegerZero { .. }, _) => {
                Ok(Some(Token::Number(Number::new_int64(0))))
            },

            /*
             * IntegerRange
             */

            (&mut State::IntegerRange { sign, ref mut range, ref mut value }, Some(ch @ b'0'..=b'9')) => {
                self.idx += 1;
                *range = range.start..range.end+1;
                *value = match *value {
                    None => None,
                    Some(v) => match v.checked_mul(10) {
                        None => None,
                        Some(v) => v.checked_add(sign.mul32(a2u(ch, 10))),
                    },
                };
                Ok(None)
            },

            (State::IntegerRange { sign, range, .. }, Some(b'.')) => {
                self.idx += 1;
                self.state = State::FractionSymbol {
                    integer: (*sign, range.clone()),
                };
                Ok(None)
            },

            (State::IntegerRange { sign, range, .. }, Some(b'e' | b'E')) => {
                self.idx += 1;
                self.state = State::ExponentSymbol {
                    integer: (*sign, range.clone()),
                    fraction: None,
                };
                Ok(None)
            },

            (&mut State::IntegerRange { value: Some(v), .. }, _) => {
                Ok(Some(Token::Number(Number::new_int64(v))))
            },

            (State::IntegerRange { sign, range, value: None }, _) => {
                Ok(Some(Token::Number(
                    Number::new_any((*sign, range.clone()), None, None),
                )))
            },

            /*
             * FractionSymbol
             */

            (State::FractionSymbol { integer }, Some(b'0'..=b'9')) => {
                self.idx += 1;
                self.state = State::FractionRange {
                    integer: integer.clone(),
                    fraction: self.idx-1..self.idx,
                };
                Ok(None)
            },

            (State::FractionSymbol { integer }, Some(b'e' | b'E')) => {
                self.idx += 1;
                self.state = State::ExponentSymbol {
                    integer: integer.clone(),
                    fraction: Some(self.idx-1..self.idx-1),
                };
                Err((Error::NumberFractionEmpty, 0))
            },

            (State::FractionSymbol { integer }, _) => {
                self.state = State::FractionRange {
                    integer: integer.clone(),
                    fraction: self.idx-1..self.idx-1,
                };
                Err((Error::NumberFractionEmpty, 0))
            },

            /*
             * FractionRange
             */

            (&mut State::FractionRange { ref mut fraction, .. }, Some(b'0'..=b'9')) => {
                self.idx += 1;
                *fraction = fraction.start..fraction.end+1;
                Ok(None)
            },

            (State::FractionRange { integer, fraction }, Some(b'e' | b'E')) => {
                self.idx += 1;
                self.state = State::ExponentSymbol {
                    integer: integer.clone(),
                    fraction: Some(fraction.clone()),
                };
                Ok(None)
            },

            (State::FractionRange { integer, fraction }, _) => {
                Ok(Some(Token::Number(Number::new_any(
                    integer.clone(),
                    Some(fraction.clone()),
                    None,
                ))))
            },

            /*
             * ExponentSymbol
             */

            (State::ExponentSymbol { integer, fraction }, Some(b'+')) => {
                self.idx += 1;
                self.state = State::ExponentSign {
                    integer: integer.clone(),
                    fraction: fraction.clone(),
                    sign: Sign::Plus,
                };
                Ok(None)
            },

            (State::ExponentSymbol { integer, fraction }, Some(b'-')) => {
                self.idx += 1;
                self.state = State::ExponentSign {
                    integer: integer.clone(),
                    fraction: fraction.clone(),
                    sign: Sign::Minus,
                };
                Ok(None)
            },

            (State::ExponentSymbol { integer, fraction }, Some(b'0'..=b'9')) => {
                self.idx += 1;
                self.state = State::ExponentRange {
                    integer: integer.clone(),
                    fraction: fraction.clone(),
                    exponent: (Sign::Plus, self.idx-1..self.idx),
                };
                Ok(None)
            },

            (State::ExponentSymbol { integer, fraction }, _) => {
                // Revert handling of the exponent symbol.
                self.idx = self.idx.strict_sub(1);
                Ok(Some(Token::Number(Number::new_any(
                    integer.clone(),
                    fraction.clone(),
                    None,
                ))))
            },

            /*
             * ExponentSign
             */

            (State::ExponentSign { integer, fraction, sign }, Some(b'0'..=b'9')) => {
                self.idx += 1;
                self.state = State::ExponentRange {
                    integer: integer.clone(),
                    fraction: fraction.clone(),
                    exponent: (*sign, self.idx-1..self.idx),
                };
                Ok(None)
            },

            (State::ExponentSign { integer, fraction, .. }, _) => {
                // Revert handling of the exponent symbol and sign.
                self.idx = self.idx.strict_sub(2);
                Ok(Some(Token::Number(Number::new_any(
                    integer.clone(),
                    fraction.clone(),
                    None,
                ))))
            },

            /*
             * ExponentRange
             */

            (State::ExponentRange { exponent: (_sign, ref mut range), .. }, Some(b'0'..=b'9')) => {
                self.idx += 1;
                *range = range.start..range.end+1;
                Ok(None)
            },

            (State::ExponentRange { integer, fraction, exponent }, _) => {
                Ok(Some(Token::Number(Number::new_any(
                    integer.clone(),
                    fraction.clone(),
                    Some(exponent.clone()),
                ))))
            },

            /*
             * String
             */

            (State::String, Some(ch @ 0x00..=0x1f)) => {
                self.idx += 1;
                self.str_sync(&[ch]);
                Err((Error::StringControlCode, 0))
            },

            (State::String, Some(0x80..=0xbf)) => {
                // Stray UTF-8 continuation character
                self.idx += 1;
                self.str_maybe_init(self.idx - 1);
                self.str_push_dummy();
                Err((Error::StringUtf8Invalid, 0))
            },

            (State::String, Some(ch @ 0xc0..=0xf7)) => {
                // UTF-8 lead character
                self.idx += 1;
                self.state = State::StringUtf8 {
                    start: self.idx - 1,
                    len: match ch {
                        0xc0..=0xdf => 2,
                        0xe0..=0xef => 3,
                        0xf0..=0xf7 => 4,
                        _ => core::unreachable!(),
                    },
                    data: [ch, 0, 0, 0],
                };
                Ok(None)
            },

            (State::String, Some(0xf8..=0xff)) => {
                // High UTF-8 lead character
                self.idx += 1;
                self.str_maybe_init(self.idx - 1);
                self.str_push_dummy();
                Err((Error::StringUtf8Invalid, 0))
            },

            (State::String, Some(b'\\')) => {
                self.idx += 1;
                self.str_maybe_init(self.idx - 1);
                self.state = State::StringEscape;
                Ok(None)
            },

            (State::String, Some(b'"')) => {
                self.idx += 1;
                self.str_sync(&[b'"']);
                Ok(Some(Token::String))
            },

            (State::String, Some(ch @ _)) => {
                self.idx += 1;
                self.str_sync(&[ch]);
                Ok(None)
            },

            (State::String, None) => {
                self.state = State::None;
                Err((Error::StringIncomplete, 0))
            },

            /*
             * StringUtf8
             */

            (&mut State::StringUtf8 { start, len, ref mut data }, Some(ch @ _)) => {
                // Collect the expected number of UTF-8 continuation bytes. Any
                // other byte terminates the sequence and is handled in the
                // next step. The actual verification is performed by
                // [`str::from_utf8()`].
                let verify = {
                    if ch & 0xc0 == 0x80 {
                        data[self.idx - start] = ch;
                        self.idx += 1;
                        self.idx - start >= len
                    } else {
                        true
                    }
                };

                if !verify {
                    Ok(None)
                } else {
                    // Release the mutable borrow by copying out of the state.
                    let data = *data;
                    let slice = &data[..self.idx-start];

                    if let Err(e) = str::from_utf8(slice) {
                        assert_eq!(e.valid_up_to(), 0);
                        self.str_maybe_init(self.idx - slice.len());
                        self.str_push_dummy();
                        self.state = State::String;
                        Err((Error::StringUtf8Invalid, 0))
                    } else {
                        self.str_sync(slice);
                        self.state = State::String;
                        Ok(None)
                    }
                }
            },

            (State::StringUtf8 { .. }, None) => {
                self.state = State::None;
                Err((Error::StringIncomplete, 0))
            },

            /*
             * StringEscape
             */

            (
                State::StringEscape,
                Some(ch @ b'"' | ch @ b'\\' | ch @ b'/' | ch @ b'b' | ch @ b'f' | ch @ b'n' | ch @ b'r' | ch @ b't'),
            ) => {
                let v = match ch {
                    b'b' => b'\x08',
                    b'f' => b'\x0c',
                    b'n' => b'\x0a',
                    b'r' => b'\x0d',
                    b't' => b'\x09',
                    v => v,
                };
                self.idx += 1;
                self.str_push(&[v]);
                self.state = State::String;
                Ok(None)
            },

            (State::StringEscape, Some(b'u')) => {
                self.idx += 1;
                self.state = State::StringEscapeUnicode {
                    start: self.idx,
                    code: 0,
                };
                Ok(None)
            },

            (State::StringEscape, Some(_)) => {
                self.idx += 1;
                self.str_push_dummy();
                self.state = State::String;
                Err((Error::StringEscapeUnknown, 0))
            },

            (State::StringEscape, None) => {
                self.state = State::None;
                Err((Error::StringIncomplete, 0))
            },

            /*
             * StringEscapeUnicode
             */

            (&mut State::StringEscapeUnicode { start, ref mut code }, Some(ch @ _)) => {
                let valid = match (self.idx - start, ch) {
                    (0..=3, b'0'..=b'9' | b'a'..=b'f' | b'A'..b'F') => {
                        self.idx += 1;
                        *code = (*code << 4) | a2u(ch, 16);
                        true
                    },
                    _ => false,
                };

                // Release the mutable borrow by copying the value, needed
                // for a possible state change below.
                let code = *code;

                if !valid {
                    self.str_push_dummy();
                    self.state = State::String;
                    Err((Error::StringEscapeInvalid, 0))
                } else if self.idx - start < 4 {
                    Ok(None)
                } else if code >= 0xd800 && code <= 0xdbff {
                    // Lead Surrogate
                    self.state = State::StringEscapeUnicodeTrail {
                        start: self.idx,
                        code: 0,
                        lead: code,
                    };
                    Ok(None)
                } else if code >= 0xdc00 && code <= 0xdfff {
                    // Trail Surrogate
                    self.str_push_dummy();
                    self.state = State::String;
                    Err((Error::StringEscapeUnpairedTrailSurrogate, 0))
                } else {
                    self.str_push_char(char::from_u32(code).unwrap());
                    self.state = State::String;
                    Ok(None)
                }
            },

            (State::StringEscapeUnicode { .. }, None) => {
                self.state = State::None;
                Err((Error::StringIncomplete, 0))
            },

            /*
             * StringEscapeUnicodeTrail
             */

            (&mut State::StringEscapeUnicodeTrail { start, ref mut code, lead }, Some(ch @ _)) => {
                let valid = match (self.idx - start, ch) {
                    (0, _) => ch == b'\\',
                    (1, _) => ch == b'u',
                    (2..=5, b'0'..=b'9' | b'a'..=b'f' | b'A'..b'F') => {
                        *code = (*code << 4) | a2u(ch, 16);
                        true
                    },
                    _ => false,
                };
                self.idx += 1;

                // Release the mutable borrow by copying the value, needed
                // for a possible state change below.
                let code = *code;

                if !valid {
                    // Backtrack all characters of a possible surrogate-trail,
                    // then raise an unpaired-lead-surrogate error and resume
                    // as normal string after the lead surrogate.
                    self.idx = start;
                    self.str_push_dummy();
                    self.state = State::String;
                    Err((Error::StringEscapeUnpairedLeadSurrogate, 0))
                } else if self.idx - start < 6 {
                    Ok(None)
                } else if code >= 0xd800 && code <= 0xdbff {
                    // This is a lead surrogate following a lead surrogate.
                    // Reject the previous lead surrogate as unpaired and
                    // start over with this lead surrogate.
                    self.str_push_dummy();
                    self.state = State::StringEscapeUnicodeTrail {
                        start: self.idx,
                        code: 0,
                        lead: code,
                    };
                    Err((Error::StringEscapeUnpairedLeadSurrogate, self.idx - start))
                } else if code >= 0xdc00 && code <= 0xdfff {
                    // This is a trail surrogate following a lead surrogate,
                    // thus a valid surrogate pair.
                    let full = 0x10000 + ((lead - 0xd800) << 10) + (code - 0xdc00);
                    self.str_push_char(char::from_u32(full).unwrap());
                    self.state = State::String;
                    Ok(None)
                } else {
                    // This is not a surrogate, so reject the previous lead
                    // surrogate but keep this codepoint.
                    self.str_push_dummy();
                    self.str_push_char(char::from_u32(code).unwrap());
                    self.state = State::String;
                    Err((Error::StringEscapeUnpairedLeadSurrogate, self.idx - start))
                }
            },

            (State::StringEscapeUnicodeTrail { .. }, None) => {
                self.state = State::None;
                Err((Error::StringIncomplete, 0))
            },
        }
    }

    /// Report a tokenization result.
    ///
    /// This will prepare a report for the given token on the current state
    /// of the engine. The raw slice must represent the full area that makes
    /// up the token and was previously stepped through.
    ///
    /// This function can report any token at any state. However, usually it
    /// only makes sense to call this immediately after [`Self::step()`]
    /// returned a token.
    ///
    /// This prepares the engine for the next token, advances the state machine
    /// if necessary, and yields the finalized accumulator content to the
    /// caller, if any.
    ///
    /// ## Safety
    ///
    /// The raw buffer `raw` must represent exactly the range that was
    /// previously stepped through via [`Self::step()`]. Any previous data
    /// validation is *not* reperformed here, but relied upon.
    unsafe fn report_token<Buffer>(
        &mut self,
        raw: &io::buffer::Slice<Buffer>,
        _token: &Token,
    ) -> Option<alloc::boxed::Box<[u8]>>
    where
        Buffer: ?Sized + io::buffer::Read,
    {
        let acc = match self.state {
            State::String => {
                if self.acc_idx > 0 {
                    raw.copy_uninit(&mut self.acc_str[0..self.acc_off]);
                    let v = core::mem::take(&mut self.acc_str)
                        .into_boxed_slice();
                    self.acc_off = 0;
                    self.acc_idx = 0;

                    // SAFETY: The accumulator is guaranteed to be
                    //     initialized from `acc_off` onwards. We
                    //     just filled in the prefix, so the entire
                    //     vector is initialized.
                    unsafe { Some(v.assume_init()) }
                } else {
                    None
                }
            },
            _ => None,
        };

        self.state = State::None;
        self.offset = self.offset.strict_add(self.idx);
        self.idx = 0;

        acc
    }

    fn report_error(&mut self, _error: (Error, usize)) {
        // Errors raised on a clear state machine consume their data. All other
        // errors are expected to be recoverable and thus leave their data for
        // the respective state to handle.
        if self.state.is_none() {
            self.offset = self.offset.strict_add(self.idx);
            self.state = State::None;
            self.idx = 0;
        }
    }
}

impl<'engine, 'read, Buffer> Dec<'engine, 'read, Buffer>
where
    Buffer: ?Sized + io::buffer::Read,
{
    fn with(
        engine: &'engine mut DecEngine,
        read: &'read Buffer,
    ) -> Self {
        Self {
            engine: engine,
            read: read,
        }
    }

    fn report(
        &mut self,
        token: Result<Token, (Error, usize)>,
    ) -> Report<'read, Buffer> {
        match token {
            Ok(v) => {
                let from = self.engine.offset;
                let to = self.engine.offset.strict_add(self.engine.idx);
                let raw = io::buffer::Slice::new(self.read, from..to);
                // SAFETY: `raw` represents exactly the data that was
                //     previously stepped through.
                let acc = unsafe { self.engine.report_token(&raw, &v) };

                Report {
                    raw: raw,
                    acc: acc,
                    token: Ok(v),
                }
            },
            Err((e, off)) => {
                let from = self.engine.offset;
                let to = self.engine.offset.strict_add(self.engine.idx).strict_sub(off);
                let raw = io::buffer::Slice::new(self.read, from..to);
                self.engine.report_error((e, off));

                Report {
                    raw: raw,
                    acc: None,
                    token: Err(e),
                }
            },
        }
    }

    /// Retrieve the next token from the stream.
    ///
    /// This will advance the tokenizer and report the next token, if there is
    /// one. Tokenization errors are reported alongside the tokens. That is, a
    /// successful call to this function will return a report that contains
    /// tokenization errors, the next token, or both. Errors are to be
    /// interpreted as preceding the token.
    ///
    /// On successful return, further tokens might be available. Hence, this
    /// function should be called repeatedly.
    ///
    /// Tokenization errors invalidate a token stream. However, the tokenizer
    /// can recover from such errors and will proceed tokenization. In most
    /// cases, further tokenization should be performed for diagnostics only.
    /// Any further tokens must be considered corrupted, unless the error was
    /// recovered by the caller.
    ///
    /// If the underlying stream has insufficient data available to continue
    /// tokenization, [`Flow::Break`] with a value of `Ok(More { .. }` is
    /// returned. It is outside the scope of this tokenizer, how this
    /// information is forwarded to the transport layer of the respective
    /// stream.
    ///
    /// If the underlying stream raised an error, [`Flow::Break`] with a value
    /// of `Err(io::stream::Error)` is returned. Those errors usually indicate
    /// that the stream buffers exceeded the reserved address space. It is an
    /// indication that the ongoing token is excessively long and no buffer
    /// limits have been applied by the transport layer. See the error
    /// definitions for details, but usually those errors should abort the
    /// operation.
    pub fn next(
        &mut self,
    ) -> Result<Option<Report<'read, Buffer>>, Buffer::Break> {
        loop {
            let map_idx = self.engine.idx;
            let map_data = self.read.map(self.engine.offset.strict_add(map_idx))?;

            if map_data.len() == 0 {
                // The end of the stream was reached. The stepper might still
                // make forward progress, or backtrack via `State::None`.
                // So we continue looping, but once we are back at
                // `State::None` *and* at the end of the stream, we can
                // finally signal `None` to the caller.
                while self.engine.idx == map_idx {
                    if self.engine.state.is_none() {
                        return Ok(None);
                    }

                    if let Some(v) = self.engine.step(None).transpose() {
                        return Ok(Some(self.report(v)));
                    }
                }
            } else {
                // In case the engine backtracks across mappings, break the
                // loop and re-map the requested range.
                while self.engine.idx >= map_idx {
                    let next = self.engine.idx.strict_sub(map_idx);

                    // If the engine stepped or jumped past the mapping, break
                    // the loop and re-map the requested range.
                    if next >= map_data.len() {
                        break;
                    }

                    if let Some(v) = self.engine.step(Some(map_data[next])).transpose() {
                        return Ok(Some(self.report(v)));
                    }
                }
            }
        }
    }
}

impl<'read, Buffer> BufferDec<'read, Buffer>
where
    Buffer: ?Sized + io::buffer::Read,
{
    /// Create a new buffer tokenizer with the given values.
    pub fn with(read: &'read Buffer) -> Self {
        Self {
            engine: DecEngine::new(),
            read: read,
        }
    }

    /// Create a scoped tokenizer for this buffer tokenizer.
    pub fn dec(&mut self) -> Dec<'_, 'read, Buffer> {
        Dec::with(&mut self.engine, self.read)
    }
}

impl<'read, Stream> StreamDec<'read, Stream>
where
    Stream: ?Sized + io::buffer::StreamRead,
{
    /// Create a new stream tokenizer with the given values.
    pub fn with(read: &'read mut Stream) -> Self {
        Self {
            engine: DecEngine::new(),
            read: read,
        }
    }

    /// Create a scoped tokenizer for this stream tokenizer.
    pub fn dec(&mut self) -> Dec<'_, '_, Stream> {
        Dec::with(&mut self.engine, self.read)
    }

    /// Advance the stream buffer.
    ///
    /// This will consume all data from the stream buffer that was already
    /// tokenized.
    pub fn consume(&mut self) {
        self.read.consume(self.engine.offset);
        self.engine.offset = 0;
    }
}


#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_next_eq {
        ($dec:expr, $raw:expr, $acc:expr, $token:expr $(,)?) => {
            {
                let dec: &mut Dec<_> = $dec;
                let raw: &[u8] = $raw;
                let acc: Option<&str> = $acc;
                let token: Result<Token, Error> = $token;

                let v = dec.next().unwrap().unwrap();
                assert_eq!(v.token, token);
                assert_eq!(&*v.raw.read(), raw);
                assert_eq!(
                    v.acc.as_ref().map(|v| &**v),
                    acc.map(|v| v.as_bytes()),
                );
            }
        };
    }

    fn test_tokens_basic<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(br#" [ "foobar", null ]"#);
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b" ", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b"[", None, Ok(Token::ArrayOpen));
        assert_next_eq!(&mut dec, b" ", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b"\"foobar\"", None, Ok(Token::String));
        assert_next_eq!(&mut dec, b",", None, Ok(Token::Comma));
        assert_next_eq!(&mut dec, b" ", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b"null", None, Ok(Token::Null));
        assert_next_eq!(&mut dec, b" ", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b"]", None, Ok(Token::ArrayClose));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_tokens_all<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(br#" null:false,true[]{}0"""#);
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b" ", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b"null", None, Ok(Token::Null));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"false", None, Ok(Token::False));
        assert_next_eq!(&mut dec, b",", None, Ok(Token::Comma));
        assert_next_eq!(&mut dec, b"true", None, Ok(Token::True));
        assert_next_eq!(&mut dec, b"[", None, Ok(Token::ArrayOpen));
        assert_next_eq!(&mut dec, b"]", None, Ok(Token::ArrayClose));
        assert_next_eq!(&mut dec, b"{", None, Ok(Token::ObjectOpen));
        assert_next_eq!(&mut dec, b"}", None, Ok(Token::ObjectClose));
        assert_next_eq!(&mut dec, b"0", None, Ok(Token::Number(Number::new_int64(0))));
        assert_next_eq!(&mut dec, b"\"\"", None, Ok(Token::String));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_whitespace<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"\n \r \t:\n:\r:\t: ");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"\n \r \t", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\n", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\r", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\t", None, Ok(Token::Whitespace));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b" ", None, Ok(Token::Whitespace));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_keyword<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> Fn(&'a [u8]) -> Buf,
    {
        {
            let buffer = f(b"null:false:true");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"null", None, Ok(Token::Null));
            assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
            assert_next_eq!(&mut dec, b"false", None, Ok(Token::False));
            assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
            assert_next_eq!(&mut dec, b"true", None, Ok(Token::True));
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"null0:nulltrue:nul");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"null0", None, Err(Error::TokenUnknown));
            assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
            assert_next_eq!(&mut dec, b"nulltrue", None, Err(Error::TokenUnknown));
            assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
            assert_next_eq!(&mut dec, b"nul", None, Err(Error::TokenUnknown));
            assert!(dec.next().unwrap().is_none());
        }
    }

    fn test_state_item<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"_123AaBb:a1_B1:0a:B1_a1");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"_123AaBb", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"a1_B1", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0", None, Ok(Token::Number(Number::new_int64(0))));
        assert_next_eq!(&mut dec, b"a", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"B1_a1", None, Err(Error::TokenUnknown));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_integer_sign<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"-0:-1:-:-");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"-0", None, Ok(Token::Number(Number::new_int64(0))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-1", None, Ok(Token::Number(Number::new_int64(-1))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-", None, Err(Error::TokenUnknown));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_integer_zero<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"0:-0:00:0000.1:01:0.1:0e1:-0");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"0", None, Ok(Token::Number(Number::new_int64(0))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-0", None, Ok(Token::Number(Number::new_int64(0))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"00", None, Err(Error::NumberIntegerLeadingZero));
        assert_next_eq!(&mut dec, b"00", None, Ok(Token::Number(Number::new_int64(0))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"00", None, Err(Error::NumberIntegerLeadingZero));
        assert_next_eq!(&mut dec, b"0000.1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 3..4),
            Some(5..6),
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"01", None, Err(Error::NumberIntegerLeadingZero));
        assert_next_eq!(&mut dec, b"01", None, Ok(Token::Number(Number::new_int64(1))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0.1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            Some(2..3),
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0e1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            Some((Sign::Plus, 2..3)),
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-0", None, Ok(Token::Number(Number::new_int64(0))));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_integer_range<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"71:01:-71:123456781234567812345678:71.0:71e0:71");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"71", None, Ok(Token::Number(Number::new_int64(71))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"01", None, Err(Error::NumberIntegerLeadingZero));
        assert_next_eq!(&mut dec, b"01", None, Ok(Token::Number(Number::new_int64(1))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-71", None, Ok(Token::Number(Number::new_int64(-71))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"123456781234567812345678", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..24),
            None,
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"71.0", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..2),
            Some(3..4),
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"71e0", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..2),
            None,
            Some((Sign::Plus, 3..4)),
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"71", None, Ok(Token::Number(Number::new_int64(71))));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_fraction_symbol<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"0.5:-0.1:.1:0.1e1");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"0.5", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            Some(2..3),
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"-0.1", None, Ok(Token::Number(Number::new_any(
            (Sign::Minus, 1..2),
            Some(3..4),
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b".", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b"1", None, Ok(Token::Number(Number::new_int64(1))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0.1e1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            Some(2..3),
            Some((Sign::Plus, 4..5)),
        ))));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_fraction_range<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"0.112233:112233.445566e5");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"0.112233", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            Some(2..8),
            None,
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"112233.445566e5", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..6),
            Some(7..13),
            Some((Sign::Plus, 14..15)),
        ))));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_exponent_symbol<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"0e:0e0:0efoobar");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"0", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            None,
        ))));
        assert_next_eq!(&mut dec, b"e", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0e0", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            Some((Sign::Plus, 2..3)),
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            None,
        ))));
        assert_next_eq!(&mut dec, b"efoobar", None, Err(Error::TokenUnknown));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_exponent_sign<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"0e+1:0e-1:0e+foobar");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"0e+1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            Some((Sign::Plus, 3..4)),
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0e-1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            Some((Sign::Minus, 3..4)),
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"0", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            None,
        ))));
        assert_next_eq!(&mut dec, b"e", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b"+", None, Err(Error::TokenUnknown));
        assert_next_eq!(&mut dec, b"foobar", None, Err(Error::TokenUnknown));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_exponent_range<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"0e1:11e-51");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"0e1", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..1),
            None,
            Some((Sign::Plus, 2..3)),
        ))));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"11e-51", None, Ok(Token::Number(Number::new_any(
            (Sign::Plus, 0..2),
            None,
            Some((Sign::Minus, 4..6)),
        ))));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_string<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> FnOnce(&'a [u8]) -> Buf,
    {
        let buffer = f(b"\"\":\" \":\"\x00\":\"foobar\":\"foo");
        let mut bufdec = BufferDec::with(&buffer);
        let mut dec = bufdec.dec();

        assert_next_eq!(&mut dec, b"\"\"", None, Ok(Token::String));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\" \"", None, Ok(Token::String));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\"\x00", None, Err(Error::StringControlCode));
        assert_next_eq!(&mut dec, b"\"\x00\"", None, Ok(Token::String));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\"foobar\"", None, Ok(Token::String));
        assert_next_eq!(&mut dec, b":", None, Ok(Token::Colon));
        assert_next_eq!(&mut dec, b"\"foo", None, Err(Error::StringIncomplete));
        assert!(dec.next().unwrap().is_none());
    }

    fn test_state_string_utf8<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> Fn(&'a [u8]) -> Buf,
    {
        {
            let buffer = f(b"\"foo\\qbar\"\"f\xc3\xa4b\"\"f\x80b\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"foo\\q", None, Err(Error::StringEscapeUnknown));
            assert_next_eq!(
                &mut dec,
                b"\"foo\\qbar\"",
                Some("\"foo\u{fffd}bar\""),
                Ok(Token::String),
            );
            assert_next_eq!(
                &mut dec,
                b"\"f\xc3\xa4b\"",
                None,
                Ok(Token::String),
            );
            assert_next_eq!(&mut dec, b"\"f\x80", None, Err(Error::StringUtf8Invalid));
            assert_next_eq!(
                &mut dec,
                b"\"f\x80b\"",
                Some("\"f\u{fffd}b\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"foo\\nbar\xc3\xa4\x80\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(
                &mut dec,
                b"\"foo\\nbar\xc3\xa4\x80",
                None,
                Err(Error::StringUtf8Invalid),
            );
            assert_next_eq!(
                &mut dec,
                b"\"foo\\nbar\xc3\xa4\x80\"",
                Some("\"foo\nbarä\u{fffd}\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"foo\\n\xc3\xa4\xef\xbf\xbdbar\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(
                &mut dec,
                b"\"foo\\n\xc3\xa4\xef\xbf\xbdbar\"",
                Some("\"foo\nä\u{fffd}bar\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }
    }

    fn test_state_string_escape<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> Fn(&'a [u8]) -> Buf,
    {
        {
            let buffer = f(b"\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(
                &mut dec,
                b"\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"",
                Some("\"\"\\/\x08\x0c\n\r\t\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"\\Q\"\"\\");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\Q", None, Err(Error::StringEscapeUnknown));
            assert_next_eq!(
                &mut dec,
                b"\"\\Q\"",
                Some("\"\u{fffd}\""),
                Ok(Token::String),
            );
            assert_next_eq!(&mut dec, b"\"\\", None, Err(Error::StringIncomplete));
            assert!(dec.next().unwrap().is_none());
        }
    }

    fn test_state_string_escape_unicode<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> Fn(&'a [u8]) -> Buf,
    {
        {
            let buffer = f(b"\"\\u");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\u", None, Err(Error::StringIncomplete));
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"\\u\\uf\\uffff\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\u", None, Err(Error::StringEscapeInvalid));
            assert_next_eq!(&mut dec, b"\"\\u\\uf", None, Err(Error::StringEscapeInvalid));
            assert_next_eq!(
                &mut dec,
                b"\"\\u\\uf\\uffff\"",
                Some("\"\u{fffd}\u{fffd}\u{ffff}\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"\\udc00\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\udc00", None, Err(Error::StringEscapeUnpairedTrailSurrogate));
            assert_next_eq!(
                &mut dec,
                b"\"\\udc00\"",
                Some("\"\u{fffd}\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }
    }

    fn test_state_string_escape_unicode_trail<Buf, BufFn>(f: BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> Fn(&'a [u8]) -> Buf,
    {
        {
            let buffer = f(b"\"\\ud800\\u");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\ud800\\u", None, Err(Error::StringIncomplete));
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"\\ud800A\\ud800\\ufffQ\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\ud800", None, Err(Error::StringEscapeUnpairedLeadSurrogate));
            assert_next_eq!(&mut dec, b"\"\\ud800A\\ud800", None, Err(Error::StringEscapeUnpairedLeadSurrogate));
            assert_next_eq!(&mut dec, b"\"\\ud800A\\ud800\\ufff", None, Err(Error::StringEscapeInvalid));
            assert_next_eq!(
                &mut dec,
                b"\"\\ud800A\\ud800\\ufffQ\"",
                Some("\"\u{fffd}A\u{fffd}\u{fffd}Q\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"\\ud812\\udc34\\ud800\\u1234\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\ud812\\udc34\\ud800", None, Err(Error::StringEscapeUnpairedLeadSurrogate));
            assert_next_eq!(
                &mut dec,
                b"\"\\ud812\\udc34\\ud800\\u1234\"",
                Some("\"\u{14834}\u{fffd}\u{1234}\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }

        {
            let buffer = f(b"\"\\ud812\\ud812\\udc34\"");
            let mut bufdec = BufferDec::with(&buffer);
            let mut dec = bufdec.dec();

            assert_next_eq!(&mut dec, b"\"\\ud812", None, Err(Error::StringEscapeUnpairedLeadSurrogate));
            assert_next_eq!(
                &mut dec,
                b"\"\\ud812\\ud812\\udc34\"",
                Some("\"\u{fffd}\u{14834}\""),
                Ok(Token::String),
            );
            assert!(dec.next().unwrap().is_none());
        }
    }

    fn test_all<Buf, BufFn>(buffn: &BufFn)
    where
        Buf: io::buffer::Read<Break = osi::never::Never>,
        BufFn: for<'a> Fn(&'a [u8]) -> Buf,
    {
        test_tokens_basic(buffn);
        test_tokens_all(buffn);
        test_state_whitespace(buffn);
        test_state_keyword(buffn);
        test_state_item(buffn);
        test_state_integer_sign(buffn);
        test_state_integer_zero(buffn);
        test_state_integer_range(buffn);
        test_state_fraction_symbol(buffn);
        test_state_fraction_range(buffn);
        test_state_exponent_symbol(buffn);
        test_state_exponent_sign(buffn);
        test_state_exponent_range(buffn);
        test_state_string(buffn);
        test_state_string_utf8(buffn);
        test_state_string_escape(buffn);
        test_state_string_escape_unicode(buffn);
        test_state_string_escape_unicode_trail(buffn);
    }

    #[test]
    fn tokens_linear() {
        fn buffn(v: &[u8]) -> alloc::boxed::Box<[u8]> {
            alloc::boxed::Box::from(v)
        }

        test_all(&buffn);
    }

    #[test]
    fn tokens_chunked_2() {
        fn buffn(v: &[u8]) -> (usize, alloc::boxed::Box<[u8]>) {
            (2, alloc::boxed::Box::from(v))
        }

        test_all(&buffn);
    }

    #[test]
    fn tokens_chunked_7() {
        fn buffn(v: &[u8]) -> (usize, alloc::boxed::Box<[u8]>) {
            (7, alloc::boxed::Box::from(v))
        }

        test_all(&buffn);
    }
}
