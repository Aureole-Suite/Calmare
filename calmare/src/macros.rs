use crate::{parse, Parse, Parser};
use crate::{Print, Printer};

pub mod strukt;


pub fn number<'src>(f: &mut Parser<'src>) -> parse::Result<&'src str> {
	fn digits<'src>(
		f: &mut Parser<'src>,
		pred: fn(&char) -> bool,
		what: &str,
	) -> parse::Result<&'src str> {
		match f.pat_mul(|c| pred(&c)) {
			"" => Err(parse::Diagnostic::error(
				f.raw_pos().as_span(),
				format!("expected {what}"),
			)),
			v => Ok(v),
		}
	}

	let pos = f.pos()?;
	if f.pat("0x").is_some() {
		digits(f, char::is_ascii_hexdigit, "hex digits")?;
	} else {
		let _ = f.pat('-');
		digits(f, char::is_ascii_digit, "digits")?;
		if f.pat('.').is_some() {
			digits(f, char::is_ascii_digit, "digits")?;
		}
	}
	Ok(f.text_since(pos))
}

pub macro int($($type:ty),*) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer) {
			write!(f, "{:?}", self);
		}
	})*

	$(impl Parse for $type {
		fn parse(f: &mut Parser) -> parse::Result<Self> {
			let s = number(f)?;
			let res = if let Some(s) = s.strip_prefix("0x") {
				Self::from_str_radix(s, 16)
			} else {
				s.parse()
			};
			res.map_err(|e| {
				parse::Diagnostic::error(f.span_of(s), format!("could not parse {}: {}", stringify!($type), e))
			})
		}
	})*
}

pub macro float($($type:ty),*) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer) {
			write!(f, "{:?}", self);
		}
	})*

	$(impl Parse for $type {
		fn parse(f: &mut Parser) -> parse::Result<Self> {
			let s = number(f)?;
			let res = s.parse();
			res.map_err(|e| {
				parse::Diagnostic::error(f.span_of(s), format!("could not parse {}: {}", stringify!($type), e))
			})
		}
	})*
}

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
			f.term(|f| f.val()).map(Self)
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
