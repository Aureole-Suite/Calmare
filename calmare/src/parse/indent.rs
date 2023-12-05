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
pub struct Space<'a> {
	pub(crate) space: &'a str,
	pub(crate) kind: SpaceKind<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub(crate) enum SpaceKind<'a> {
	End,
	Indent(Indent<'a>),
	Space,
}

impl<'a> Space<'a> {
	pub fn space(&self) -> &'a str {
		self.space
	}

	pub fn indent(&self) -> Option<Indent<'a>> {
		match self.kind {
			SpaceKind::Indent(i) => Some(i),
			_ => None,
		}
	}
}

impl<'a, 'b> PartialEq<Space<'b>> for Space<'a> {
	fn eq(&self, other: &Space<'b>) -> bool {
		self.kind == other.kind
	}
}

impl<'a, 'b> PartialOrd<Space<'b>> for Space<'a> {
	fn partial_cmp(&self, other: &Space<'b>) -> Option<std::cmp::Ordering> {
		self.kind.partial_cmp(&other.kind)
	}
}

impl<'a, 'b> PartialEq<Indent<'b>> for Space<'a> {
	fn eq(&self, other: &Indent<'b>) -> bool {
		self.kind == SpaceKind::Indent(*other)
	}
}

impl<'a, 'b> PartialEq<Space<'b>> for Indent<'a> {
	fn eq(&self, other: &Space<'b>) -> bool {
		SpaceKind::Indent(*self) == other.kind
	}
}

impl<'a, 'b> PartialOrd<Indent<'b>> for Space<'a> {
	fn partial_cmp(&self, other: &Indent<'b>) -> Option<std::cmp::Ordering> {
		self.kind.partial_cmp(&SpaceKind::Indent(*other))
	}
}

impl<'a, 'b> PartialOrd<Space<'b>> for Indent<'a> {
	fn partial_cmp(&self, other: &Space<'b>) -> Option<std::cmp::Ordering> {
		SpaceKind::Indent(*self).partial_cmp(&other.kind)
	}
}
