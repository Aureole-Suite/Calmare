use crate::{Parse, Parser, ParseContext, parse};
use crate::{Print, PrintContext, Printer};

pub macro strukt($(struct $type:ty { $($field:ident),* $(,)? })+) {
	$(impl Print for $type {
		fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
			let Self { $($field),* } = &self;
			$(f.kv_line(stringify!($field), $field, ctx);)*
		}
	})+

	$(impl Parse for $type {
		fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
			$(let mut $field = PlainField::default();)*

			let mut first_error = true;
			let start = f.pos();
			f.lines(|f| {
				let parse::Spanned(span, word) = f.try_spanned(|f| f.word())?;
				match word {
					$(stringify!($field) => $field.parse_field(f, ctx, span),)*
					_ => {
						let mut diag = parse::Diagnostic::error(span, "unknown field");
						if first_error {
							first_error = false;
							diag = diag.note(span, format!(
								"allowed fields are {}",
								[$(concat!("`", stringify!($field), "`")),*].join(", ")
							));
						}
						Err(diag)
					}
				}
			});
			let span = start | f.pos();

			#[allow(unused_mut)]
			let mut failures: Vec<&str> = Vec::new();

			$(
				let $field = $field.get();
				if $field.is_none() {
					failures.push(concat!("`", stringify!($field), "`"));
				}
			)*

			if failures.is_empty() {
				Ok(Self {
					$($field: $field.unwrap(),)*
				})
			} else {
				Err(parse::Diagnostic::error(span, "missing fields").note(span, failures.join(", ")))
			}
		}
	})+
}

pub trait ParseField: Default {
	type Output;
	fn parse_field(
		&mut self,
		f: &mut Parser,
		ctx: &mut ParseContext,
		head_span: parse::Span,
	) -> parse::Result<()>;
	fn get(self) -> Option<Self::Output>;
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

	fn parse_field(
		&mut self,
		f: &mut Parser,
		ctx: &mut ParseContext,
		head_span: parse::Span,
	) -> parse::Result<()> {
		if let Some(prev_span) = self.head_span.replace(head_span) {
			parse::Diagnostic::error(head_span, "duplicate item")
				.note(prev_span, "previous here")
				.emit(f);
		}
		f.space()?;
		self.value = Some(T::parse(f, ctx)?);
		Ok(())
	}

	fn get(self) -> Option<Self::Output> {
		self.value
	}
}
