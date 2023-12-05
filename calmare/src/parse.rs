mod diagnostic;
mod indent;
mod span;

use std::str::pattern::Pattern;

pub use diagnostic::{Diagnostic, Emit, Level, Result};
use indent::{Indent, Space};
pub use span::{Span, Spanned};

pub struct Parser<'a> {
	source: &'a str,
	pos: usize,
	indent: Indent<'a>,
	last_space: Option<Spanned<Space<'a>>>,
	diagnostics: Vec<Diagnostic>,
}

impl<'src> Parser<'src> {
	pub fn new(source: &'src str) -> Self {
		Parser {
			source,
			pos: 0,
			indent: Indent(&source[..0]),
			last_space: None,
			diagnostics: Vec::new(),
		}
	}

	pub fn pos(&self) -> Span {
		Span::new(self.pos)
	}

	pub fn diagnostics(&self) -> &[Diagnostic] {
		self.diagnostics.as_ref()
	}

	pub fn span_text(&self, s: Span) -> &'src str {
		&self.source[s.as_range()]
	}

	pub fn span_of(&self, text: &'src str) -> Span {
		// from https://github.com/rust-lang/rfcs/pull/2796
		let range = self.source.as_bytes().as_ptr_range();
		let subrange = text.as_bytes().as_ptr_range();
		assert!(subrange.start >= range.start);
		assert!(subrange.end <= range.end);
		unsafe {
			let start = subrange.start.offset_from(range.start) as usize;
			let end = subrange.end.offset_from(range.start) as usize;
			Span::new(start) | Span::new(end)
		}
	}

	fn string(&self) -> &'src str {
		&self.source[self.pos..]
	}

	fn eat(&mut self, suffix: &'src str) -> &'src str {
		assert_eq!(
			self.source.as_bytes().as_ptr_range().end,
			suffix.as_bytes().as_ptr_range().end,
		);
		let len = self.string().len() - suffix.len();
		let s = &self.string()[..len];
		self.pos += len;
		self.last_space = None;
		s
	}

	fn pat(&mut self, pat: impl Pattern<'src>) -> Option<&'src str> {
		self.string().strip_prefix(pat).map(|suf| self.eat(suf))
	}

	fn pat_mul(&mut self, pat: impl Pattern<'src>) -> &'src str {
		self.eat(self.string().trim_start_matches(pat))
	}

	fn pat_mul_nonempty(&mut self, pat: impl Pattern<'src>, what: &str) -> Result<&'src str> {
		match self.pat_mul(pat) {
			"" => Err(Diagnostic::error(self.pos(), format!("expected {what}"))),
			v => Ok(v),
		}
	}

	pub fn check(&mut self, c: &str) -> Result<&mut Self> {
		self.check_space()?;
		match self.pat(c) {
			Some(_) => Ok(self),
			None => Err(Diagnostic::error(self.pos(), format!("expected `{c}`"))),
		}
	}

	pub fn space(&mut self) -> Result<&mut Self> {
		self.space2();
		Ok(self)
	}

	fn space2(&mut self) -> Spanned<Space<'src>> {
		if self.last_space.is_none() {
			let startpos = self.pos;
			let space = self.inline_space();
			let mut kind = Space::Inline;

			// Intentionally short-circuiting
			while self.pat('\r').is_some() | self.pat('\n').is_some() {
				kind = Space::Indent(Indent(self.inline_space()));
			}

			if startpos == 0 {
				kind = Space::Indent(Indent(&self.source[..0]));
			}
			if self.pos == self.source.len() {
				kind = Space::End
			}

			self.last_space = Some(self.span_of(space).on(kind))
		}
		self.last_space.unwrap()
	}

	fn inline_space(&mut self) -> &'src str {
		let start = self.pos();
		self.pat_mul([' ', '\t']);
		// TODO don't include comments in return value
		if self.pat("//").is_some() {
			self.pat_mul(|c| c != '\n');
		}
		self.span_text(start | self.pos())
	}

	pub fn lines(&mut self, mut f: impl FnMut(&mut Self) -> Result<()>) {
		let Space::Indent(target_indent) = *self.space2() else {
			self.one_line(&mut f);
			return;
		};
		if target_indent <= self.indent && self.pos != 0 {
			return;
		}
		let prev_indent = std::mem::replace(&mut self.indent, target_indent);

		loop {
			let space = self.space2();

			if *space == self.indent {
				self.indent = space.indent().unwrap();
			} else if *space <= prev_indent {
				break;
			} else {
				let span = self.span_of(space.indent().unwrap().0);
				let mut error = Diagnostic::error(span, "unexpected indent");
				error = error.note(self.span_of(prev_indent.0), "it's more than here...");
				if *space < self.indent {
					error = error.note(self.span_of(self.indent.0), "...but less than here");
				} else {
					error = error.note(self.span_of(self.indent.0), "...but uncomparable to here");
					error = error.note(self.span_of(self.indent.0), "did you mix tabs and spaces?");
				}
				while *self.space2() > prev_indent {
					self.pat(|_| true);
				}
				error = error.note(self.pos(), "skipping to here");
				error.emit(self);
				break;
			}

			self.one_line(&mut f);
		}

		self.indent = prev_indent;
	}

	fn check_space(&mut self) -> Result<()> {
		let space = self.space2();
		if *space > self.indent
			|| matches!(*space, Space::Indent(indent) if std::ptr::eq(indent.0, self.indent.0))
		{
			Ok(())
		} else {
			Err(Diagnostic::error(
				space.span,
				"unexpected end of line",
			))
		}
	}

	fn one_line(&mut self, f: &mut impl FnMut(&mut Self) -> Result<()>) {
		let pos1 = self.pos();
		let ok = f(self).emit(self).is_some();
		if self.pos() == pos1 {
			if ok {
				Diagnostic::error(pos1, "line parsed as empty — this is a bug").emit(self);
			}
			self.pat(|_| true);
		}

		let space = self.space2();
		if *space > self.indent {
			let mut error = Diagnostic::error(space.span.at_end(), "expected end of line");
			while *self.space2() > self.indent {
				self.pat(|_| true);
			}
			error = error.note(self.pos(), "skipping to here");
			if ok {
				error.emit(self);
			}
		}
	}

	pub fn word(&mut self) -> Result<&'src str> {
		self.check_space()?;
		let start = self.pos();
		if self
			.pat(|c| unicode_ident::is_xid_start(c) || c == '_')
			.is_none()
		{
			return Err(Diagnostic::error(self.pos(), "expected word"));
		}
		self.pat_mul(unicode_ident::is_xid_continue);
		Ok(self.span_text(start | self.pos()))
	}

	pub fn check_word(&mut self, word: &str) -> Result<&mut Self> {
		self.check_space()?;
		let pos = self.pos();
		let aword = self.word();
		if aword != Ok(word) {
			return Err(Diagnostic::error(
				pos | self.pos(),
				format!("expected `{word}`"),
			));
		}
		Ok(self)
	}

	pub fn term<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
		self.check("[")?.space()?;
		let v = f(self)?;
		self.space()?.check("]")?;
		Ok(v)
	}

	pub fn tuple<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
		self.check("(")?.space()?;
		let v = f(self)?;
		self.space()?.check(")")?;
		Ok(v)
	}

	pub fn number(&mut self) -> Result<&'src str> {
		let pos = self.pos();
		if self.pat("0x").is_some() {
			self.pat_mul_nonempty(|c: char| c.is_ascii_hexdigit(), "hex digits")?;
		} else {
			let _ = self.pat('-');
			self.pat_mul_nonempty(|c: char| c.is_ascii_digit(), "digits")?;
			if self.pat('.').is_some() {
				self.pat_mul_nonempty(|c: char| c.is_ascii_digit(), "digits")?;
			}
		}
		Ok(self.span_text(pos | self.pos()))
	}
}

#[test]
fn test() {
	let text = "\
word
    word
      word
     \tword // error: mixed tabs and spaces
    word
  word // error: not an indentation level
word
 word
   word
   word
  \tword // error: mixed
";
	let mut parser = Parser::new(text);
	let mut n = 0;
	parser.lines(|f| {
		n += 1;
		f.check_word("word").unwrap().lines(|f| {
			n += 1;
			f.check_word("word").unwrap().lines(|f| {
				n += 1;
				f.check_word("word").unwrap();
				Ok(())
			});
			Ok(())
		});
		Ok(())
	});
	println!("{:#?}", parser.diagnostics());
	assert!(parser.string().is_empty());
	assert_eq!(parser.diagnostics().len(), 3);
	assert_eq!(n, 8);
}
