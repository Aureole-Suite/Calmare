use crate::{Print, PrintContext, Printer};

pub macro number($($type:ty),*) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer, _ctx: &mut PrintContext) {
			write!(f, "{:?}", self);
		}
	})*
}

pub macro newtype_term($type:ty, $term:literal) {
	impl Print for $type {
		fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
			let Self(v) = self;
			f.term($term).field().val(v, ctx);
		}
	}
}

pub macro newtype_unit($type:ty, $suf:literal) {
	impl Print for $type {
		fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
			let Self(v) = self;
			f.val(v, ctx).suf($suf);
		}
	}
}

#[doc(hidden)]
pub(crate) trait Hex {
	fn print_hex(&self, f: &mut Printer);
}

impl Hex for u8 {
	fn print_hex(&self, f: &mut Printer) {
		write!(f, "0x{self:02X}");
	}
}

impl Hex for u16 {
	fn print_hex(&self, f: &mut Printer) {
		write!(f, "0x{self:04X}");
	}
}

impl Hex for u32 {
	fn print_hex(&self, f: &mut Printer) {
		write!(f, "0x{self:08X}");
	}
}

pub macro newtype_hex($type:ty) {
	impl Print for $type {
		fn print(&self, f: &mut Printer, _ctx: &mut PrintContext) {
			let Self(v) = self;
			v.print_hex(f);
		}
	}
}

pub macro strukt($(struct $type:ty { $($field:ident),* $(,)? })+) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
			let Self { $($field),* } = &self;
			$(f.kv_line(stringify!($field), $field, ctx);)*
		}
	})+
}
