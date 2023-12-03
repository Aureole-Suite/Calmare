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

impl<'a, 'b> PartialEq<Indent<'b>> for Option<Indent<'a>> {
	fn eq(&self, other: &Indent<'b>) -> bool {
		self.as_ref().map_or(false, |a| a == other)
	}
}

impl<'a, 'b> PartialOrd<Indent<'b>> for Option<Indent<'a>> {
	fn partial_cmp(&self, other: &Indent<'b>) -> Option<std::cmp::Ordering> {
		use std::cmp::Ordering::*;
		self.as_ref()
			.map_or(Some(Greater), |a| a.partial_cmp(other))
	}
}
