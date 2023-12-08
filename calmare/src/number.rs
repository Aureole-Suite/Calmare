use crate::{parse, Parse, Parser, Print, Printer};

fn number<'src>(f: &mut Parser<'src>) -> parse::Result<&'src str> {
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

macro int($($type:ty),*) {
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

macro float($($type:ty),*) {
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

int!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);
float!(f32, f64);
