mod diagnostic;
mod indent;
mod span;

use bimap::BiBTreeMap;
use std::collections::BTreeSet;
use std::str::pattern::Pattern;

use themelios::scena::insn_set::InsnSet;
use themelios::types::Label;

pub use diagnostic::{Diagnostic, Emit, Level, Result};
use indent::{Indent, Space};
pub use span::Span;

pub struct Parser<'src> {
	source: &'src str,
	pos: usize,
	indent: Indent<'src>,
	allow_unindented: bool,
	last_space: Option<(Span, Space<'src>)>,
	diagnostics: Vec<Diagnostic>,
	iset: &'src InsnSet<'src>,
	labels: BiBTreeMap<&'src str, Label>,
	defined_labels: BTreeSet<Label>,
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
			allow_unindented: false,
			last_space: None,
			diagnostics: Vec::new(),
			iset,
			labels: BiBTreeMap::new(),
			defined_labels: BTreeSet::new(),
		}
	}

	pub fn raw_pos(&self) -> SourcePos {
		SourcePos(self.pos)
	}

	pub fn raw_span(&self, pos: SourcePos) -> Span {
		pos.as_span() | self.raw_pos().as_span()
	}

	pub fn pos(&mut self) -> Result<SourcePos> {
		let space = self.space();
		if space.1 > self.indent || space.1 == self.indent && self.allow_unindented {
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

	pub(crate) fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
		std::mem::take(&mut self.diagnostics)
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

	pub fn advance(&mut self, len: usize) -> &'src str {
		let s = &self.rest()[..len];
		if len > 0 {
			self.pos += len;
			self.last_space = None;
			self.allow_unindented = false;
		}
		s
	}

	pub fn pat(&mut self, pat: impl Pattern<'src>) -> Option<&'src str> {
		self.rest()
			.strip_prefix(pat)
			.map(|suffix| self.advance(self.rest().len() - suffix.len()))
	}

	pub fn any_char(&mut self) -> char {
		if self.rest().starts_with('\n') || self.rest().is_empty() {
			'\n'
		} else {
			self.pat(|_| true).unwrap().chars().next().unwrap()
		}
	}

	pub fn pat_mul(&mut self, pat: impl Pattern<'src>) -> &'src str {
		let suffix = self.rest().trim_start_matches(pat);
		self.advance(self.rest().len() - suffix.len())
	}

	pub fn peek_word(&mut self) -> Result<&'src str> {
		self.pos()?;
		let s = self.rest();
		let start = |c| unicode_ident::is_xid_start(c) || c == '_';
		let cont = |c| unicode_ident::is_xid_continue(c) || c == '/';
		let suffix = s
			.strip_prefix(start)
			.map(|suffix| suffix.trim_start_matches(cont))
			.unwrap_or(s);
		Ok(&s[..s.len() - suffix.len()])
	}

	pub fn text_since(&self, pos: SourcePos) -> &'src str {
		&self.source[pos.0..self.pos]
	}

	pub fn check(&mut self, c: &str) -> Result<()> {
		let pos = self.pos()?;
		match self.pat(c) {
			Some(_) => Ok(()),
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
			let this_indent = self.space().1;

			if this_indent == self.indent {
				self.indent = this_indent.indent().unwrap();
			} else if this_indent <= prev_indent {
				break;
			} else {
				let span = self.span_of(this_indent.indent().unwrap().0);
				let mut error = Diagnostic::error(span, "unexpected indent");
				error.note(self.span_of(prev_indent.0), "it's more than here...");
				if this_indent < self.indent {
					error.note(self.span_of(self.indent.0), "...but less than here");
				} else {
					error.note(self.span_of(self.indent.0), "...but uncomparable to here");
					error.note(self.span_of(self.indent.0), "did you mix tabs and spaces?");
				}
				self.indent = prev_indent;
				self.skip_until_indent(error);
				break;
			}

			self.allow_unindented = true;
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

		if self.pos().is_ok() {
			let error = Diagnostic::error(self.last_nonspace(), "expected end of line").filter(ok);
			self.skip_until_indent(error);
		}
	}

	fn skip_until_indent(&mut self, error: Diagnostic) {
		while self.pos().is_ok() {
			self.pat(|_| true);
		}
		error
			.with_note(self.last_nonspace(), "skipping to here")
			.emit(self);
	}

	pub fn last_nonspace(&mut self) -> Span {
		self.space().0.at_end()
	}

	pub fn allow_unindented<T: 'src>(&mut self, func: impl FnOnce(&mut Self) -> T) -> T {
		let prev = std::mem::replace(&mut self.allow_unindented, true);
		let val = func(self);
		self.allow_unindented = prev;
		val
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
		match self.peek_word()? {
			s @ "" => Err(Diagnostic::error(self.span_of(s), "expected word")),
			word => {
				self.advance(word.len());
				Ok(word)
			}
		}
	}

	pub fn check_word(&mut self, word: &str) -> Result<()> {
		let aword = self.peek_word()?;
		if aword == word {
			self.advance(word.len());
			Ok(())
		} else {
			Err(Diagnostic::error(
				self.span_of(aword),
				format!("expected `{word}`"),
			))
		}
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

	pub fn label(&mut self, label: &'src str) -> Label {
		if let Some(s) = self.labels.get_by_left(label) {
			*s
		} else {
			let l = Label(self.labels.len());
			self.labels.insert(label, l);
			l
		}
	}

	pub fn define_label(&mut self, label: Label, span: Span) {
		if !self.defined_labels.insert(label) {
			let prev = self.labels.get_by_right(&label).unwrap();
			Diagnostic::error(span, "duplicate label definition")
				.with_note(self.span_of(prev), "previous defined here")
				.emit(self)
		}
	}

	pub(crate) fn check_labels(&mut self) {
		for (span, label) in &self.labels {
			if !self.defined_labels.contains(label) {
				self.diagnostics
					.push(Diagnostic::error(self.span_of(span), "undefined label"))
			}
		}
	}

	pub fn tuple(&mut self) -> Result<TupleParser<'_, 'src>> {
		self.check("(")?;
		Ok(TupleParser::new(self))
	}
}

pub struct TupleParser<'a, 'src> {
	parser: &'a mut Parser<'src>,
	count: usize,
}

impl<'a, 'src> TupleParser<'a, 'src> {
	pub fn new(parser: &'a mut Parser<'src>) -> Self {
		Self { parser, count: 0 }
	}

	pub fn field(&mut self) -> Result<&mut Parser<'src>> {
		let pos = self.parser.pos()?;
		if self.parser.check(")").is_ok() {
			return Err(Diagnostic::error(
				self.parser.span(pos),
				"unexpected end of tuple",
			));
		}
		if self.count != 0 {
			self.parser.check(",")?;
		}
		self.count += 1;
		Ok(self.parser)
	}

	pub fn try_field(&mut self) -> Result<Option<&mut Parser<'src>>> {
		self.parser.pos()?;
		if self.parser.check(")").is_ok() {
			return Ok(None);
		}
		if self.count != 0 {
			self.parser.check(",")?;
		}
		self.count += 1;
		Ok(Some(self.parser))
	}

	pub fn finish(self) -> Result<()> {
		self.parser.check(")")
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
		f.check_word("word").unwrap();
		f.lines(|f| {
			n += 1;
			f.check_word("word").unwrap();
			f.lines(|f| {
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
