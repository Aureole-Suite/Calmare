use super::{Parser, Span, Spanned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Level {
	Info,
	Warning,
	Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[must_use]
pub struct Diagnostic {
	pub level: Level,
	pub text: Spanned<String>,
	pub notes: Vec<Spanned<String>>,
}

impl Diagnostic {
	pub fn new(level: Level, span: Span, text: impl ToString) -> Diagnostic {
		Diagnostic {
			level,
			text: Spanned(span, text.to_string()),
			notes: Vec::new(),
		}
	}

	pub fn error(span: Span, text: impl ToString) -> Diagnostic {
		Self::new(Level::Error, span, text)
	}

	pub fn warn(span: Span, text: impl ToString) -> Diagnostic {
		Self::new(Level::Warning, span, text)
	}

	pub fn info(span: Span, text: impl ToString) -> Diagnostic {
		Self::new(Level::Info, span, text)
	}

	pub fn note(mut self, span: Span, text: impl ToString) -> Diagnostic {
		self.notes.push(span.on(text.to_string()));
		self
	}

	pub fn emit(self, parser: &mut Parser) {
		parser.diagnostics.push(self);
	}

	pub fn is_fatal(&self) -> bool {
		self.level >= Level::Error
	}
}

pub trait Emit {
	type Output;
	fn emit(self, parser: &mut Parser) -> Self::Output;
}

impl Emit for Diagnostic {
	type Output = ();

	fn emit(self, parser: &mut Parser) {
		Diagnostic::emit(self, parser)
	}
}

impl<T, E: Emit<Output = ()>> Emit for Result<T, E> {
	type Output = Option<T>;

	fn emit(self, parser: &mut Parser) -> Option<T> {
		match self {
			Ok(v) => Some(v),
			Err(e) => {
				e.emit(parser);
				None
			}
		}
	}
}

impl<E: Emit<Output = ()>> Emit for Vec<E> {
	type Output = ();

	fn emit(self, parser: &mut Parser) {
		for e in self {
			e.emit(parser)
		}
	}
}

impl Emit for () {
	type Output = ();

	fn emit(self, _parser: &mut Parser) {}
}

pub type Result<T, E = Diagnostic> = std::result::Result<T, E>;
