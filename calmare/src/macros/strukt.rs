use crate::parse::{Diagnostic, Emit as _, Span};
use crate::{parse, Parse, ParseBlock, Parser, Print};
use crate::{PrintBlock, Printer};

macro select {
	($ty:ty) => { $ty },
	() => { PlainField<_> },
}

macro name {
	($name:ident $alias:ident) => { stringify!($alias) },
	($name:ident) => { stringify!($name) },
}

pub macro strukt($(struct $type:ty {
	$($field:ident $(as $alias:ident)? $(: $ty:ty)?),*
	$(,)?
})+) {
	$(impl PrintBlock for $type {
		fn print_block(&self, f: &mut Printer) {
			let Self { $($field),* } = &self;
			$(<select!($($ty)?)>::print_field(name!($field $($alias)?), f, $field);)*
		}
	})+

	$(impl ParseBlock for $type {
		fn parse_block(f: &mut Parser) -> parse::Result<Self> {
			let span = f.raw_pos().as_span();
			$(let mut $field = <select!($($ty)?)>::default();)*

			let mut first_error = true;
			f.lines(|f| match f.word()? {
				$(word @ name!($field $($alias)?) => $field.parse_field(word, f),)*
				word => {
					let mut diag = parse::Diagnostic::error(f.span_of(word), "unknown field");
					if first_error {
						first_error = false;
						diag.note(f.span_of(word), format!(
							"allowed fields are {}",
							[$(concat!("`", name!($field $($alias)?), "`")),*].join(", ")
						));
					}
					Err(diag)
				}
			});

			#[allow(unused_mut)]
			let mut missing: Vec<&str> = Vec::new();

			$(
				if !$field.is_present() {
					missing.push(concat!("`", name!($field $($alias)?), "`"));
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

	pub fn insert(&mut self, span: Span, value: parse::Result<T>) {
		if let Some((prev, _)) = self.0.replace((span, value.emit())) {
			Diagnostic::error(span, "duplicate item")
				.with_note(prev, "previous here")
				.emit();
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

pub trait Field: Default {
	type Value;
	fn print_field(key: &str, f: &mut Printer, value: &Self::Value);
	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()>;
	fn is_present(&self) -> bool;
	fn get(self) -> Option<Self::Value>;
}

#[derive(Debug, Clone)]
pub struct PlainField<T> {
	value: Slot<T>,
}

impl<T> Default for PlainField<T> {
	fn default() -> Self {
		Self { value: Slot::new() }
	}
}

impl<T: Print + Parse> Field for PlainField<T> {
	type Value = T;

	fn print_field(key: &str, f: &mut Printer, value: &T) {
		f.word(key).val(value).line();
	}

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		let value = f.val();
		self.value.insert(f.span_of(word), value);
		Ok(())
	}

	fn is_present(&self) -> bool {
		self.value.span().is_some()
	}

	fn get(self) -> Option<Self::Value> {
		self.value.get()
	}
}
