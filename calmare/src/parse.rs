mod diagnostic;
mod indent;
mod span;

use std::str::pattern::Pattern;

pub use diagnostic::{Diagnostic, Emit, Level, Result};
use indent::{Indent, Space};
pub use span::Span;
use themelios::scena::insn_set::InsnSet;

pub struct Parser<'src> {
	source: &'src str,
	pos: usize,
	indent: Indent<'src>,
	last_space: Option<(Span, Space<'src>)>,
	diagnostics: Vec<Diagnostic>,
	iset: &'src InsnSet<'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourcePos(usize);

impl SourcePos {
	pub fn as_span(self) -> Span {
		Span::new(self.0)
	}
}

impl<'src> Parser<'src> {
	pub fn new(source: &'src str, iset: &'src InsnSet<'src>) -> Self {
		Parser {
			source,
			pos: 0,
			indent: Indent(&source[..0]),
			last_space: None,
			diagnostics: Vec::new(),
			iset,
		}
	}

	pub fn raw_pos(&self) -> SourcePos {
		SourcePos(self.pos)
	}

	pub fn pos(&mut self) -> Result<SourcePos> {
		let space = self.space();
		let is_at_indent =
			matches!(space.1, Space::Indent(indent) if std::ptr::eq(indent.0, self.indent.0));
		if space.1 > self.indent || is_at_indent {
			Ok(SourcePos(self.pos))
		} else {
			Err(Diagnostic::error(space.0, "unexpected end of line"))
		}
	}

	pub fn span(&self, pos: SourcePos) -> Span {
		let end = match self.last_space {
			Some(t) => t.0.at_start(),
			None => self.raw_pos().as_span(),
		};
		pos.as_span() | end
	}

	pub fn diagnostics(&self) -> &[Diagnostic] {
		self.diagnostics.as_ref()
	}

	pub fn insn_set(&self) -> &'src InsnSet<'src> {
		self.iset
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

	pub fn rest(&self) -> &'src str {
		&self.source[self.pos..]
	}

	fn eat(&mut self, suffix: &'src str) -> &'src str {
		assert_eq!(
			self.source.as_bytes().as_ptr_range().end,
			suffix.as_bytes().as_ptr_range().end,
		);
		let len = self.rest().len() - suffix.len();
		let s = &self.rest()[..len];
		self.pos += len;
		self.last_space = None;
		s
	}

	pub fn pat(&mut self, pat: impl Pattern<'src>) -> Option<&'src str> {
		self.rest().strip_prefix(pat).map(|suf| self.eat(suf))
	}

	pub fn any_char(&mut self) -> Option<char> {
		self.pat(|_| true).map(|a| a.chars().next().unwrap())
	}

	pub fn pat_mul(&mut self, pat: impl Pattern<'src>) -> &'src str {
		self.eat(self.rest().trim_start_matches(pat))
	}

	pub fn text_since(&self, pos: SourcePos) -> &'src str {
		&self.source[pos.0..self.pos]
	}

	pub fn check(&mut self, c: &str) -> Result<&mut Self> {
		let pos = self.pos()?;
		match self.pat(c) {
			Some(_) => Ok(self),
			None => Err(Diagnostic::error(pos.as_span(), format!("expected `{c}`"))),
		}
	}

	pub fn no_space(&mut self) -> &mut Self {
		assert!(self.last_space.is_none());
		self.last_space = Some((self.raw_pos().as_span(), Space::Inline));
		self
	}

	fn space(&mut self) -> (Span, Space<'src>) {
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

			self.last_space = Some((self.span_of(space), kind))
		}
		self.last_space.unwrap()
	}

	fn inline_space(&mut self) -> &'src str {
		let start = self.raw_pos();
		self.pat_mul([' ', '\t']);
		// TODO don't include comments in return value
		if self.pat("//").is_some() {
			self.pat_mul(|c| c != '\n');
		}
		self.text_since(start)
	}

	pub fn lines(&mut self, mut f: impl FnMut(&mut Self) -> Result<()>) {
		let Space::Indent(target_indent) = self.space().1 else {
			self.one_line(&mut f);
			return;
		};
		if target_indent <= self.indent && self.pos != 0 {
			return;
		}
		let prev_indent = std::mem::replace(&mut self.indent, target_indent);

		loop {
			let space = self.space();

			if space.1 == self.indent {
				self.indent = space.1.indent().unwrap();
			} else if space.1 <= prev_indent {
				break;
			} else {
				let span = self.span_of(space.1.indent().unwrap().0);
				let mut error = Diagnostic::error(span, "unexpected indent");
				error.note(self.span_of(prev_indent.0), "it's more than here...");
				if space.1 < self.indent {
					error.note(self.span_of(self.indent.0), "...but less than here");
				} else {
					error.note(self.span_of(self.indent.0), "...but uncomparable to here");
					error.note(self.span_of(self.indent.0), "did you mix tabs and spaces?");
				}
				self.skip_until_indent(prev_indent, error);
				break;
			}

			self.one_line(&mut f);
		}

		self.indent = prev_indent;
	}

	fn one_line(&mut self, f: &mut impl FnMut(&mut Self) -> Result<()>) {
		let pos1 = self.raw_pos();
		let ok = f(self).emit(self).is_some();
		if self.raw_pos() == pos1 {
			Diagnostic::error(pos1.as_span(), "line parsed as empty — this is a bug")
				.filter(ok)
				.emit(self);
			self.pat(|_| true);
		}

		let space = self.space();
		if space.1 > self.indent {
			let error = Diagnostic::error(space.0.at_end(), "expected end of line").filter(ok);
			self.skip_until_indent(self.indent, error);
		}
	}

	fn skip_until_indent(&mut self, indent: Indent<'src>, error: Diagnostic) {
		while self.space().1 > indent {
			self.pat(|_| true);
		}
		error
			.with_note(self.space().0.at_end(), "skipping to here")
			.emit(self);
	}

	pub fn try_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<Option<T>> {
		self.space();
		let pos = self.raw_pos();
		let diag = self.diagnostics().len();
		match f(self) {
			Ok(v) => Ok(Some(v)),
			Err(_) if self.raw_pos() == pos && self.diagnostics().len() == diag => Ok(None),
			Err(e) => Err(e),
		}
	}

	pub fn word(&mut self) -> Result<&'src str> {
		let pos = self.pos()?;
		if self
			.pat(|c| unicode_ident::is_xid_start(c) || c == '_')
			.is_none()
		{
			return Err(Diagnostic::error(pos.as_span(), "expected word"));
		}
		self.pat_mul(unicode_ident::is_xid_continue);
		Ok(self.text_since(pos))
	}

	pub fn check_word(&mut self, word: &str) -> Result<&mut Self> {
		let pos = self.pos()?;
		let aword = self.word();
		if aword != Ok(word) {
			return Err(Diagnostic::error(
				self.span(pos),
				format!("expected `{word}`"),
			));
		}
		Ok(self)
	}

	pub fn eof(&self) -> Result<()> {
		if self.rest().is_empty() {
			Ok(())
		} else {
			Err(Diagnostic::error(
				self.raw_pos().as_span(),
				"expected end of file",
			))
		}
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
	use themelios::scena::insn_set;
	let iset = insn_set::get(insn_set::Game::Fc, insn_set::Variant::Base);
	let mut parser = Parser::new(text, &iset);
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
	parser.eof().emit(&mut parser);
	println!("{:#?}", parser.diagnostics());
	assert_eq!(parser.diagnostics().len(), 3);
	assert_eq!(n, 8);
}
