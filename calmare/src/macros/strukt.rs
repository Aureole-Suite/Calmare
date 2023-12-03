use crate::{parse, Parse, ParseContext, Parser};
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
			let start = f.pos();
			f.check(":")?.space()?;
			$(let mut $field = PlainField::default();)*

			let mut first_error = true;
			f.lines(|f| match f.word()? {
				$(word @ stringify!($field) => $field.parse_field(word, f, ctx),)*
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
				Err(parse::Diagnostic::error(start, "missing fields").note(start, failures.join(", ")))
			}
		}
	})+
}

pub trait ParseField: Default {
	type Output;
	fn parse_field<'src>(
		&mut self,
		word: &'src str,
		f: &mut Parser<'src>,
		ctx: &mut ParseContext,
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

	fn parse_field<'src>(
		&mut self,
		word: &'src str,
		f: &mut Parser<'src>,
		ctx: &mut ParseContext,
	) -> parse::Result<()> {
		if let Some(prev_span) = self.head_span.replace(f.span_of(word)) {
			parse::Diagnostic::error(f.span_of(word), "duplicate item")
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
