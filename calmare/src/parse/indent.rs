#[derive(Debug, Clone, Copy, Eq)]
pub struct Indent<'a>(pub &'a str);

impl<'a, 'b> PartialEq<Indent<'b>> for Indent<'a> {
	fn eq(&self, other: &Indent<'b>) -> bool {
		self.0 == other.0
	}
}

impl<'a, 'b> PartialOrd<Indent<'b>> for Indent<'a> {
	fn partial_cmp(&self, other: &Indent<'b>) -> Option<std::cmp::Ordering> {
		use std::cmp::Ordering::*;
		let a = self.0;
		let b = other.0;
		if a == b {
			Some(Equal)
		} else if a.starts_with(b) {
			Some(Greater)
		} else if b.starts_with(a) {
			Some(Less)
		} else {
			None
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum Space<'a> {
	End,
	Indent(Indent<'a>),
	Inline,
}

impl<'a> Space<'a> {
	pub fn indent(&self) -> Option<Indent<'a>> {
		match self {
			Space::Indent(i) => Some(*i),
			_ => None,
		}
	}
}

impl<'a, 'b> PartialEq<Indent<'b>> for Space<'a> {
	fn eq(&self, other: &Indent<'b>) -> bool {
		*self == Space::Indent(*other)
	}
}

impl<'a, 'b> PartialEq<Space<'b>> for Indent<'a> {
	fn eq(&self, other: &Space<'b>) -> bool {
		Space::Indent(*self) == *other
	}
}

impl<'a, 'b> PartialOrd<Indent<'b>> for Space<'a> {
	fn partial_cmp(&self, other: &Indent<'b>) -> Option<std::cmp::Ordering> {
		self.partial_cmp(&Space::Indent(*other))
	}
}

impl<'a, 'b> PartialOrd<Space<'b>> for Indent<'a> {
	fn partial_cmp(&self, other: &Space<'b>) -> Option<std::cmp::Ordering> {
		Space::Indent(*self).partial_cmp(other)
	}
}
