mod diagnostic;
mod indent;
mod span;

use std::str::pattern::Pattern;

pub use diagnostic::{Diagnostic, Emit, Level, Result};
use indent::Indent;
pub use span::{Span, Spanned};

pub struct Parser<'a> {
	source: &'a str,
	pos: usize,
	// current indentation level
	indent: Indent<'a>,
	// position after any inline space at the last .space() call, at the first newline if any
	after_space: usize,
	// indentation at last .space() call
	last_indent: Option<Indent<'a>>,
	diagnostics: Vec<Diagnostic>,
}

impl<'src> Parser<'src> {
	pub fn new(source: &'src str) -> Self {
		Parser {
			source,
			pos: 0,
			indent: Indent(""),
			after_space: 0,
			last_indent: None,
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
		self.last_indent = None;
		s
	}

	pub fn pat(&mut self, pat: impl Pattern<'src>) -> Option<&'src str> {
		self.string().strip_prefix(pat).map(|suf| self.eat(suf))
	}

	pub fn check(&mut self, c: &str) -> Result<&mut Self> {
		match self.pat(c) {
			Some(_) => Ok(self),
			None => Err(Diagnostic::error(self.pos(), format!("expected `{c}`"))),
		}
	}

	pub fn pat_mul(&mut self, pat: impl Pattern<'src>) -> &'src str {
		self.eat(self.string().trim_start_matches(pat))
	}

	pub fn pat_mul_nonempty(&mut self, pat: impl Pattern<'src>, what: &str) -> Result<&'src str> {
		match self.pat_mul(pat) {
			"" => Err(Diagnostic::error(self.pos(), format!("expected {what}"))),
			v => Ok(v),
		}
	}

	pub fn space(&mut self) -> Result<&mut Self> {
		if self.last_indent.is_none() {
			self.inline_space();
			self.after_space = self.pos;
			// Intentionally short-circuiting
			while self.pat('\r').is_some() | self.pat('\n').is_some() {
				self.last_indent = Some(self.inline_space())
			}
			if self.string().is_empty() {
				self.last_indent = Some(Indent(""))
			}
		}

		if self.last_indent <= self.indent {
			Err(Diagnostic::error(
				Span::new(self.after_space),
				"unexpected end of line",
			))
		} else {
			Ok(self)
		}
	}

	fn inline_space(&mut self) -> Indent<'src> {
		let start = self.pos();
		self.pat_mul([' ', '\t']);
		if self.pat("//").is_some() {
			self.pat_mul(|c| c != '\n');
		}
		Indent(self.span_text(start | self.pos()))
	}

	pub fn lines(&mut self, mut f: impl FnMut(&mut Self) -> Result<()>) {
		let target_indent = {
			match self.last_indent {
				Some(target_indent) => {
					if target_indent <= self.indent {
						return;
					}
					target_indent
				}
				None => self.indent,
			}
		};
		let prev_indent = std::mem::replace(&mut self.indent, target_indent);

		loop {
			if self.string().is_empty() {
				break;
			}

			let pos1 = self.pos();
			let ok = f(self).emit(self).is_some();
			if ok && self.pos() == pos1 {
				Diagnostic::error(pos1, "line parsed as empty — this is probably a bug").emit(self);
				self.pat(|_| true);
			}

			let _ = self.space();

			if self.last_indent < self.indent {
				break;
			}

			if self.last_indent == self.indent {
				continue;
			}

			if ok {
				Diagnostic::error(Span::new(self.after_space), "expected end of line").emit(self);
			}

			while self.last_indent > self.indent {
				self.pat(|_| true);
				let _ = self.space();
			}
		}

		self.indent = prev_indent;
	}

	pub fn word(&mut self) -> Result<&'src str> {
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
