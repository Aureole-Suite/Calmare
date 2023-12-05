use std::ops::Range;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl std::fmt::Debug for Span {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}+{}", self.start, self.end - self.start)
	}
}

impl Span {
	pub const fn new(pos: usize) -> Self {
		Span {
			start: pos,
			end: pos,
		}
	}

	pub const fn at_start(&self) -> Self {
		Span::new(self.start)
	}

	pub const fn at_end(&self) -> Self {
		Span::new(self.end)
	}

	pub fn join(self, b: Span) -> Self {
		Span {
			start: self.start.min(b.start),
			end: self.end.max(b.end),
		}
	}

	pub const fn connects(self, b: Span) -> bool {
		let a = self;
		a.end == b.start
	}

	pub const fn as_range(self) -> Range<usize> {
		self.start..self.end
	}

	pub const fn on<T>(self, value: T) -> Spanned<T> {
		Spanned {
			span: self,
			spanned_value: value,
		}
	}
}

impl std::ops::BitOr for Span {
	type Output = Self;

	fn bitor(self, rhs: Self) -> Self::Output {
		self.join(rhs)
	}
}

impl std::ops::BitOrAssign for Span {
	fn bitor_assign(&mut self, rhs: Self) {
		*self = *self | rhs;
	}
}

#[derive(Clone, Copy, Eq)]
pub struct Spanned<T> {
	pub span: Span,
	pub spanned_value: T,
}

impl<T> std::ops::Deref for Spanned<T> {
	type Target = T;

	fn deref(&self) -> &T {
		&self.spanned_value
	}
}

impl<T: PartialEq> PartialEq for Spanned<T> {
	fn eq(&self, other: &Self) -> bool {
		self.spanned_value == other.spanned_value
	}
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if f.alternate() {
			self.span.fmt(f)?;
			f.write_str("@")?;
		}
		self.spanned_value.fmt(f)
	}
}
