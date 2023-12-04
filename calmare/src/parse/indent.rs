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

#[derive(Debug, Clone, Copy, Eq)]
pub enum Space<'a> {
	Space { space: &'a str },
	Indent { space: &'a str, indent: Indent<'a> },
}

impl<'a> Space<'a> {
	pub fn space(&self) -> &'a str {
		match self {
			Space::Space { space } | Space::Indent { space, indent: _ } => space,
		}
	}

	pub fn indent(&self) -> Option<Indent<'a>> {
		match self {
			Space::Space { space: _ } => None,
			Space::Indent { space: _, indent } => Some(*indent),
		}
	}
}

impl<'a> From<Indent<'a>> for Space<'a> {
	fn from(indent: Indent<'a>) -> Self {
		Space::Indent {
			space: &indent.0[..0],
			indent,
		}
	}
}

impl<'a, 'b> PartialEq<Space<'b>> for Space<'a> {
	fn eq(&self, other: &Space<'b>) -> bool {
		self.indent() == other.indent()
	}
}

impl<'a, 'b> PartialOrd<Space<'b>> for Space<'a> {
	fn partial_cmp(&self, other: &Space<'b>) -> Option<std::cmp::Ordering> {
		use std::cmp::Ordering::*;
		match (self.indent(), other.indent()) {
			(None, None) => Some(Equal),
			(None, Some(_)) => Some(Greater),
			(Some(_), None) => Some(Less),
			(Some(a), Some(b)) => a.partial_cmp(&b),
		}
	}
}

impl<'a, 'b> PartialEq<Indent<'b>> for Space<'a> {
	fn eq(&self, other: &Indent<'b>) -> bool {
		*self == Space::from(*other)
	}
}

impl<'a, 'b> PartialEq<Space<'b>> for Indent<'a> {
	fn eq(&self, other: &Space<'b>) -> bool {
		Space::from(*self) == *other
	}
}

impl<'a, 'b> PartialOrd<Indent<'b>> for Space<'a> {
	fn partial_cmp(&self, other: &Indent<'b>) -> Option<std::cmp::Ordering> {
		self.partial_cmp(&Space::from(*other))
	}
}

impl<'a, 'b> PartialOrd<Space<'b>> for Indent<'a> {
	fn partial_cmp(&self, other: &Space<'b>) -> Option<std::cmp::Ordering> {
		Space::from(*self).partial_cmp(other)
	}
}
