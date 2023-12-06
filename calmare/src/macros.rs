use crate::{parse, Parse, Parser};
use crate::{Print, Printer};

pub mod strukt;

pub macro newtype_term($type:ty, $term:literal) {
	impl Print for $type {
		fn print(&self, f: &mut Printer) {
			let Self(v) = self;
			f.term($term).field().val(v);
		}
	}

	impl Parse for $type {
		fn parse(f: &mut Parser) -> parse::Result<Self> {
			f.check_word($term)?;
			f.check("[")?;
			let v = f.val()?;
			f.check("]")?;
			Ok(Self(v))
		}
	}
}

pub macro newtype_unit($type:ty, $suf:literal) {
	impl Print for $type {
		fn print(&self, f: &mut Printer) {
			let Self(v) = self;
			f.val(v).no_space().word($suf);
		}
	}
	impl Parse for $type {
		fn parse(f: &mut Parser) -> parse::Result<Self> {
			let v = f.val()?;
			f.no_space().check_word($suf)?;
			Ok(Self(v))
		}
	}
}

pub macro newtype_hex($type:ty) {
	impl Print for $type {
		fn print(&self, f: &mut Printer) {
			let Self(v) = self;
			f.hex(v);
		}
	}

	impl Parse for $type {
		fn parse(f: &mut Parser) -> parse::Result<Self> {
			f.val().map(Self)
		}
	}
}
