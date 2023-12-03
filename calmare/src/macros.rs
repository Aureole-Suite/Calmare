use crate::{parse, Parse, ParseContext, Parser};
use crate::{Print, PrintContext, Printer};

pub mod strukt;

pub macro int($($type:ty),*) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer, _ctx: &mut PrintContext) {
			write!(f, "{:?}", self);
		}
	})*

	$(impl Parse for $type {
		fn parse(f: &mut Parser, _ctx: &mut ParseContext) -> parse::Result<Self> {
			let parse::Spanned(span, s) = f.try_spanned(|f| f.number())?;
			let res = if let Some(s) = s.strip_prefix("0x") {
				Self::from_str_radix(s, 16)
			} else {
				s.parse()
			};
			res.map_err(|e| {
				parse::Diagnostic::error(span, format!("could not parse {}: {}", stringify!($type), e))
			})
		}
	})*
}

pub macro float($($type:ty),*) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer, _ctx: &mut PrintContext) {
			write!(f, "{:?}", self);
		}
	})*

	$(impl Parse for $type {
		fn parse(f: &mut Parser, _ctx: &mut ParseContext) -> parse::Result<Self> {
			let parse::Spanned(span, s) = f.try_spanned(|f| f.number())?;
			let res = s.parse();
			res.map_err(|e| {
				parse::Diagnostic::error(span, format!("could not parse {}: {}", stringify!($type), e))
			})
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

	impl Parse for $type {
		fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
			let mut tup = f.check_term($term)?;
			let v = Parse::parse(tup.field()?, ctx)?;
			tup.finish()?;
			Ok(Self(v))
		}
	}
}

pub macro newtype_unit($type:ty, $suf:literal) {
	impl Print for $type {
		fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
			let Self(v) = self;
			f.val(v, ctx).no_space().word($suf);
		}
	}
	impl Parse for $type {
		fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
			let v = Parse::parse(f, ctx)?;
			f.check_word($suf)?;
			Ok(Self(v))
		}
	}
}

pub macro newtype_hex($type:ty) {
	impl Print for $type {
		fn print(&self, f: &mut Printer, _ctx: &mut PrintContext) {
			let Self(v) = self;
			f.hex(v);
		}
	}

	impl Parse for $type {
		fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
			Parse::parse(f, ctx).map(Self)
		}
	}
}
