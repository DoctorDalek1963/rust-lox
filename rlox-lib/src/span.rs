//! This module provides the [`Span`], [`WithSpan`], and [`LineOffsets`] types.

use std::{cmp, fmt, hash::Hash, ops::Deref};

/// A section of source code, measured as indices into source code.
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Span {
    /// The index of the start of the span.
    pub start: usize,

    /// The index of the end of the span (inclusive).
    pub end: usize,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if cfg!(debug_assertions) {
            f.debug_struct("Span")
                .field("start", &self.start)
                .field("end", &self.end)
                .finish()
        } else {
            write!(f, "")
        }
    }
}

impl Span {
    /// Join two spans.
    pub fn union(&self, other: &Self) -> Self {
        Self {
            start: cmp::min(self.start, other.start),
            end: cmp::max(self.end, other.end),
        }
    }

    /// Join this span with another one in-place.
    pub fn mut_union(&mut self, other: &Self) {
        *self = self.union(other);
    }

    pub fn between(left: &Self, right: &Self) -> Self {
        assert!(
            left.start <= right.end,
            "It doesn't make sense to get the span between left ({left:?}) and right ({right:?})"
        );

        Self {
            start: left.start,
            end: right.end,
        }
    }
}

/// Wrap a value with a [`Span`].
pub struct WithSpan<T> {
    /// The span of the value.
    pub span: Span,

    /// The value itself.
    pub value: T,
}

impl<T: Clone> Clone for WithSpan<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            span: self.span,
        }
    }
}

impl<T: Copy> Copy for WithSpan<T> {}

impl<T: fmt::Debug> fmt::Debug for WithSpan<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if cfg!(debug_assertions) {
            f.debug_struct("WithSpan")
                .field("value", &self.value)
                .field("span", &self.span)
                .finish()
        } else {
            write!(f, "{:?}", self.value)
        }
    }
}

impl<T: PartialEq> PartialEq for WithSpan<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value && self.span == other.span
    }
}

impl<T: Eq> Eq for WithSpan<T> {}

impl<T: Hash> Hash for WithSpan<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.value.hash(state);
    }
}

impl<T> From<WithSpan<T>> for Span {
    fn from(value: WithSpan<T>) -> Self {
        value.span
    }
}

impl<T> From<&WithSpan<T>> for Span {
    fn from(value: &WithSpan<T>) -> Self {
        value.span
    }
}

impl<T> Deref for WithSpan<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

/// A set of line offsets for getting line and column numbers from [`Span`]s.
pub struct LineOffsets {
    /// The indices of newline characters.
    offsets: Box<[usize]>,

    /// The total length of the string.
    len: usize,
}

impl LineOffsets {
    /// Create a new set of line offsets.
    pub fn new(data: &str) -> Self {
        let mut offsets = vec![0];
        let len = data.len();

        for (i, val) in data.chars().enumerate() {
            if val == '\n' {
                offsets.push(i + 1);
            }
        }

        Self {
            offsets: offsets.into(),
            len,
        }
    }

    /// Get the line number and index of the previous newline for the given offset.
    pub fn line_and_newline_offset(&self, offset: usize) -> (usize, usize) {
        assert!(
            offset <= self.len,
            "Span offset must be within length of source code: offset={offset}"
        );

        match self.offsets.binary_search(&offset) {
            Ok(line_idx) => (line_idx + 1, self.offsets[line_idx]),
            Err(line_idx) => (line_idx, self.offsets[line_idx.saturating_sub(1)]),
        }
    }
}
