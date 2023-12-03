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

	fn hex(&mut self, val: impl Hex) -> &mut Self {
		val.print_hex(self);
		self.space()
	}
}

pub trait Print {
	fn print(&self, f: &mut Printer);
}

pub trait Parse: Sized {
	fn parse(f: &mut Parser) -> parse::Result<Self>;
}

macros::int!(u8, u16, u32, u64, i8, i16, i32, i64);
macros::float!(f32, f64);

impl<T: Print> Print for &T {
	fn print(&self, f: &mut Printer) {
		T::print(self, f)
	}
}

impl<T: Print> Print for Box<T> {
	fn print(&self, f: &mut Printer) {
		T::print(self, f)
	}
}

impl<T: Parse> Parse for Box<T> {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		T::parse(f).map(Box::new)
	}
}

impl<T: Print + ToOwned> Print for Cow<'_, T> {
	fn print(&self, f: &mut Printer) {
		T::print(self, f)
	}
}

impl<'a, T: ToOwned> Parse for Cow<'a, T>
where
	T::Owned: Parse,
{
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		Parse::parse(f).map(Cow::Owned)
	}
}

impl<A: Print, B: Print> Print for (A, B) {
	fn print(&self, f: &mut Printer) {
		f.val(&self.0).val(&self.1);
	}
}

impl<A: Parse, B: Parse> Parse for (A, B) {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		let a = A::parse(f)?;
		f.space()?;
		let b = B::parse(f)?;
		Ok((a, b))
	}
}

impl<A: Print, B: Print, C: Print> Print for (A, B, C) {
	fn print(&self, f: &mut Printer) {
		f.val(&self.0).val(&self.1).val(&self.2);
	}
}

impl<A: Parse, B: Parse, C: Parse> Parse for (A, B, C) {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		let a = A::parse(f)?;
		f.space()?;
		let b = B::parse(f)?;
		f.space()?;
		let c = C::parse(f)?;
		Ok((a, b, c))
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
		Ok(String::new()) // TODO
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
