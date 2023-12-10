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
			match f.any_char() {
				'"' => break,
				'\n' => {
					return Err(Diagnostic::error(
						pos.as_span() | f.raw_pos().as_span(),
						"unterminated string",
					))
				}
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
		// Hack because .lines() uses .space(), which skips consecutive newlines
		fn newlines(text_since: &str, skip: usize, string: &mut String) {
			for _ in text_since.chars().filter(|c| *c == '\n').skip(skip) {
				string.push('\n');
			}
		}

		f.check("{")?;

		let mut string = String::new();
		let mut pos = f.raw_pos();
		let mut auto = None;
		f.lines(|f| {
			newlines(f.text_since(pos), string.is_empty().into(), &mut string);
			loop {
				let pos = f.raw_pos();
				match f.any_char() {
					'\n' => break,
					'♯' => {
						let n = f.pat_mul(|c: char| char::is_ascii_digit(&c));
						if n.is_empty() {
							match f.any_char() {
								'A' => auto = Some((string.len(), f.raw_span(pos))),
								'W' => string.push('\t'),
								'r' => string.push('\r'),
								ch @ (' ' | '　' | '{' | '}') => string.push(ch),
								_ => Diagnostic::error(f.raw_span(pos), "invalid escape sequence")
									.emit(f),
							}
						} else {
							match f.any_char() {
								'C' | 'i' | 'x' => string.push_str(f.text_since(pos)),
								_ => Diagnostic::error(f.raw_span(pos), "invalid escape sequence")
									.emit(f),
							}
						}
					}
					ch => string.push(ch),
				}
			}
			pos = f.raw_pos();
			Ok(())
		});
		newlines(f.text_since(pos), 1, &mut string);

		match auto {
			Some((len, _)) if len == string.len() => {}
			Some((_, span)) => Diagnostic::error(span, "`♯A` can only be at end").emit(f),
			None => string.push('\t'),
		}

		f.allow_unindented(|f| f.check("}"))?;

		Ok(Text(TString(string)))
	}
}
