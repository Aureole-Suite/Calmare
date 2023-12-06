use themelios::types::{TString, Text};

use crate::parse::{self, Diagnostic};
use crate::{Parse, Parser, Print, Printer};

impl Print for str {
	fn print(&self, f: &mut Printer) {
		write!(f, "{self:?}"); // TODO
	}
}

impl Print for String {
	fn print(&self, f: &mut Printer) {
		self.as_str().print(f)
	}
}

impl Parse for String {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		let pos = f.pos()?;
		f.pat('"')
			.ok_or_else(|| Diagnostic::error(pos.as_span(), "expected string"))?;
		let mut out = String::new();
		loop {
			if f.rest().starts_with('\n') || f.rest().is_empty() {
				return Err(Diagnostic::error(
					pos.as_span() | f.raw_pos().as_span(),
					"unterminated string",
				));
			}
			match f.any_char().unwrap() {
				'"' => break,
				'\\' => unimplemented!(), // will be ♯ later
				char => out.push(char),
			}
		}
		Ok(out)
	}
}

impl Print for TString {
	fn print(&self, f: &mut Printer) {
		self.0.print(f)
	}
}

impl Parse for TString {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.val().map(Self)
	}
}

impl Print for Text {
	fn print(&self, f: &mut Printer) {
		f.word("{").line();
		f.indent(|f| {
			let str = self.0.as_str();
			let has_wait = str.ends_with('\t');
			let str = str.strip_suffix('\t').unwrap_or(str);
			let mut iter = str.chars().peekable();
			while let Some(ch) = iter.next() {
				match ch {
					'\n' => f.line(),
					'\t' => write!(f, "♯W"),
					'\r' if iter.peek() == Some(&'\n') => write!(f, "♯r").line(),
					'\r' => write!(f, "♯r♯").line(),
					' ' | '　' if f.is_line() => write!(f, "♯{ch}"),
					'{' | '}' => write!(f, "♯{ch}"),
					ch => write!(f, "{ch}"),
				};
			}
			if !has_wait {
				write!(f, "♯A");
			}
		});
		f.line().word("}");
	}
}

impl Parse for Text {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		Err(Diagnostic::info(f.pos()?.as_span(), "TODO"))
	}
}
