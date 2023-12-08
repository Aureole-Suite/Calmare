use crate::parse::{Diagnostic, Emit as _, Span};
use crate::{parse, ParseBlock, Parser};
use crate::{PrintBlock, Printer};

pub macro strukt($(struct $type:ty { $($field:ident),* $(,)? })+) {
	$(impl PrintBlock for $type {
		fn print_block(&self, f: &mut Printer) {
			let Self { $($field),* } = &self;
			$(f.word(stringify!($field)).val($field).line();)*
		}
	})+

	$(impl ParseBlock for $type {
		fn parse_block(f: &mut Parser) -> parse::Result<Self> {
			let span = f.raw_pos().as_span();
			$(let mut $field = PlainField::new(|f| f.val());)*

			let mut first_error = true;
			f.lines(|f| match f.word()? {
				$(word @ stringify!($field) => $field.parse_field(word, f),)*
				word => {
					let mut diag = parse::Diagnostic::error(f.span_of(word), "unknown field");
					if first_error {
						first_error = false;
						diag.note(f.span_of(word), format!(
							"allowed fields are {}",
							[$(concat!("`", stringify!($field), "`")),*].join(", ")
						));
					}
					Err(diag)
				}
			});

			#[allow(unused_mut)]
			let mut missing: Vec<&str> = Vec::new();

			$(
				if !$field.is_present() {
					missing.push(concat!("`", stringify!($field), "`"));
				}
			)*

			if missing.is_empty() {
				$(
					let Some($field) = $field.get() else {
						return Err(parse::Diagnostic::DUMMY);
					};
				)*
				Ok(Self { $($field,)* })
			} else {
				Err(
					parse::Diagnostic::error(span, "missing fields")
						.with_note(span, missing.join(", "))
				)
			}
		}
	})+
}

#[derive(Debug, Clone)]
pub struct Slot<T>(Option<(Span, Option<T>)>);

impl<T> Slot<T> {
	pub fn new() -> Slot<T> {
		Slot(None)
	}

	pub fn insert(&mut self, f: &mut Parser, span: Span, value: parse::Result<T>) {
		if let Some((prev, _)) = self.0.replace((span, value.emit(f))) {
			Diagnostic::error(span, "duplicate item")
				.with_note(prev, "previous here")
				.emit(f);
		}
	}

	pub fn span(&self) -> Option<Span> {
		self.0.as_ref().map(|v| v.0)
	}

	pub fn get(self) -> Option<T> {
		self.0.and_then(|v| v.1)
	}

	pub fn get_ref(&self) -> Option<&T> {
		self.0.as_ref().and_then(|v| v.1.as_ref())
	}
}

impl<T> Default for Slot<T> {
	fn default() -> Self {
		Self::new()
	}
}

pub trait ParseField {
	type Output;
	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()>;
	fn is_present(&self) -> bool;
	fn get(self) -> Option<Self::Output>;
}

#[derive(Debug, Clone)]
pub struct PlainField<T, F> {
	func: F,
	value: Slot<T>,
}

impl<T, F> PlainField<T, F>
where
	F: FnMut(&mut Parser) -> parse::Result<T>,
{
	pub fn new(func: F) -> Self {
		Self {
			func,
			value: Slot::new(),
		}
	}
}

impl<T, F> ParseField for PlainField<T, F>
where
	F: FnMut(&mut Parser) -> parse::Result<T>,
{
	type Output = T;

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		let value = (self.func)(f);
		self.value.insert(f, f.span_of(word), value);
		Ok(())
	}

	fn is_present(&self) -> bool {
		self.value.span().is_some()
	}

	fn get(self) -> Option<Self::Output> {
		self.value.get()
	}
}
