use crate::{Print, PrintContext, Printer};

pub macro number($($type:ty),*) {
	$(impl Print for $type {
		fn print(&self, _ctx: &mut PrintContext, f: &mut Printer) {
			write!(f, "{:?}", self);
		}
	})*
}

pub macro newtype_term($type:ty, $term:literal) {
	impl Print for $type {
		fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
			let Self(v) = self;
			f.term($term).field().val(v, ctx);
		}
	}
}

pub macro newtype_unit($type:ty, $suf:literal) {
	impl Print for $type {
		fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
			let Self(v) = self;
			f.val(v, ctx).suf($suf);
		}
	}
}

#[doc(hidden)]
pub(crate) trait Hex {
	fn hex_width(&self) -> usize;
}

impl Hex for u8 {
	fn hex_width(&self) -> usize {
		2
	}
}

impl Hex for u16 {
	fn hex_width(&self) -> usize {
		4
	}
}

impl Hex for u32 {
	fn hex_width(&self) -> usize {
		8
	}
}

pub macro newtype_hex($type:ty) {
	impl Print for $type {
		fn print(&self, _ctx: &mut PrintContext, f: &mut Printer) {
			let Self(v) = self;
			write!(f, "0x{:0width$X}", v, width = v.hex_width());
		}
	}
}
