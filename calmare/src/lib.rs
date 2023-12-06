#![feature(decl_macro)]
#![feature(pattern)]

pub mod print;
use std::borrow::Cow;

pub use print::Printer;
pub mod parse;
pub use parse::Parser;

mod scena;
mod types;

mod macros;

impl Printer {
	fn val(&mut self, val: impl Print) -> &mut Self {
		val.print(self);
		self.space()
	}

	fn val_block(&mut self, val: impl PrintBlock) -> &mut Self {
		self.block(|f| val.print_block(f));
		self.space()
	}

	fn hex(&mut self, val: impl Hex) -> &mut Self {
		val.print_hex(self);
		self.space()
	}
}

impl Parser<'_> {
	fn val<T: Parse>(&mut self) -> parse::Result<T> {
		T::parse(self)
	}

	fn try_val<T: Parse>(&mut self) -> parse::Result<Option<T>> {
		self.try_parse(T::parse)
	}

	fn val_block<T: ParseBlock>(&mut self) -> parse::Result<T> {
		self.check(":")?;
		T::parse_block(self)
	}
}

pub trait Print {
	fn print(&self, f: &mut Printer);
}

pub trait PrintBlock {
	fn print_block(&self, f: &mut Printer);
}

pub trait Parse: Sized {
	fn parse(f: &mut Parser) -> parse::Result<Self>;
}

pub trait ParseBlock: Sized {
	fn parse_block(f: &mut Parser) -> parse::Result<Self>;
}

macros::int!(u8, u16, u32, u64, i8, i16, i32, i64);
macros::float!(f32, f64);

impl<T: Print + ?Sized> Print for &T {
	fn print(&self, f: &mut Printer) {
		T::print(self, f)
	}
}

impl<T: PrintBlock + ?Sized> PrintBlock for &T {
	fn print_block(&self, f: &mut Printer) {
		T::print_block(self, f)
	}
}

impl<T: Print + ?Sized> Print for Box<T> {
	fn print(&self, f: &mut Printer) {
		T::print(self, f)
	}
}

impl<T: PrintBlock + ?Sized> PrintBlock for Box<T> {
	fn print_block(&self, f: &mut Printer) {
		T::print_block(self, f)
	}
}

impl<T: Parse + ?Sized> Parse for Box<T> {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.val().map(Box::new)
	}
}

impl<T: ParseBlock + ?Sized> ParseBlock for Box<T> {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		T::parse_block(f).map(Box::new)
	}
}

impl<T: Print + ToOwned + ?Sized> Print for Cow<'_, T> {
	fn print(&self, f: &mut Printer) {
		T::print(self, f)
	}
}

impl<'a, T: ToOwned + ?Sized> Parse for Cow<'a, T>
where
	T::Owned: Parse,
{
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.val().map(Cow::Owned)
	}
}

impl<A: Print, B: Print> Print for (A, B) {
	fn print(&self, f: &mut Printer) {
		f.val(&self.0).val(&self.1);
	}
}

impl<A: Parse, B: Parse> Parse for (A, B) {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		Ok((f.val()?, f.val()?))
	}
}

impl<A: Print, B: Print, C: Print> Print for (A, B, C) {
	fn print(&self, f: &mut Printer) {
		f.val(&self.0).val(&self.1).val(&self.2);
	}
}

impl<A: Parse, B: Parse, C: Parse> Parse for (A, B, C) {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		Ok((f.val()?, f.val()?, f.val()?))
	}
}

impl<A: Print> Print for Option<A> {
	fn print(&self, f: &mut Printer) {
		match self {
			Some(v) => f.val(v),
			None => f.word("null"),
		};
	}
}

impl<A: Parse> Parse for Option<A> {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		// This makes Option<Option<T>> priorize the outer None.
		// Priorizing the inner null would give worse "expected" messages.
		if f.check_word("null").is_ok() {
			return Ok(None);
		}

		match f.val() {
			Ok(v) => Ok(Some(v)),
			Err(mut v) => {
				if v.text.starts_with("expected ") {
					v.text.push_str(" or null");
				}
				Err(v)
			}
		}
	}
}

impl Print for str {
	fn print(&self, f: &mut Printer) {
		write!(f, "{self:?}"); // TODO
	}
}

impl Print for String {
	fn print(&self, f: &mut Printer) {
		self.as_str().print(f)
	}
}

impl Parse for String {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.string()
	}
}

trait Hex {
	fn print_hex(&self, f: &mut Printer);
}

impl<T: Hex> Hex for &T {
	fn print_hex(&self, f: &mut Printer) {
		T::print_hex(self, f)
	}
}

impl Hex for u8 {
	fn print_hex(&self, f: &mut Printer) {
		write!(f, "0x{self:02X}");
	}
}

impl Hex for u16 {
	fn print_hex(&self, f: &mut Printer) {
		write!(f, "0x{self:04X}");
	}
}

impl Hex for u32 {
	fn print_hex(&self, f: &mut Printer) {
		write!(f, "0x{self:08X}");
	}
}
