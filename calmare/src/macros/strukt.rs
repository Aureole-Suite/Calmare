use crate::{parse, Parse, ParseBlock, Parser};
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
			let start = f.pos();
			$(let mut $field = PlainField::default();)*

			let mut first_error = true;
			f.lines(|f| match f.word()? {
				$(word @ stringify!($field) => $field.parse_field(word, f),)*
				word => {
					let mut diag = parse::Diagnostic::error(f.span_of(word), "unknown field");
					if first_error {
						first_error = false;
						diag = diag.note(f.span_of(word), format!(
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
				let $field = $field.get();
				if $field.is_none() {
					missing.push(concat!("`", stringify!($field), "`"));
				}
			)*

			if missing.is_empty() {
				$(
					let Some(Some($field)) = $field else {
						return Err(parse::Diagnostic::DUMMY);
					};
				)*
				Ok(Self { $($field,)* })
			} else {
				Err(parse::Diagnostic::error(start, "missing fields").note(start, missing.join(", ")))
			}
		}
	})+
}

pub trait ParseField: Default {
	type Output;
	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()>;
	fn get(self) -> Option<Option<Self::Output>>;
}

#[derive(Debug, Clone)]
pub struct PlainField<T> {
	head_span: Option<parse::Span>,
	value: Option<T>,
}

impl<T> Default for PlainField<T> {
	fn default() -> Self {
		Self {
			head_span: None,
			value: None,
		}
	}
}

impl<T: Parse> ParseField for PlainField<T> {
	type Output = T;

	fn parse_field<'src>(&mut self, word: &'src str, f: &mut Parser<'src>) -> parse::Result<()> {
		if let Some(prev_span) = self.head_span.replace(f.span_of(word)) {
			parse::Diagnostic::error(f.span_of(word), "duplicate item")
				.note(prev_span, "previous here")
				.emit(f);
		}
		f.space()?;
		self.value = Some(T::parse(f)?);
		Ok(())
	}

	fn get(self) -> Option<Option<Self::Output>> {
		self.head_span.map(|_| self.value)
	}
}
