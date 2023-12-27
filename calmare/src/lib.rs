#![feature(decl_macro)]
#![feature(pattern)]
#![feature(array_try_from_fn)]
#![feature(try_blocks)]

pub mod print;
use std::borrow::Cow;

use themelios::gamedata as iset;

pub use print::Printer;
pub mod parse;
use parse::Emit as _;
pub use parse::Parser;

mod ed8;
mod scena;
mod types;

mod macros;
mod number;
mod string;

impl Printer<'_> {
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

	fn sqbrack_val<T: Parse>(&mut self) -> parse::Result<T> {
		self.check("[")?;
		let v = self.val()?;
		self.check("]")?;
		Ok(v)
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

macro tuple {
	(@single $($n:ident)*) => {
		#[allow(non_snake_case, unused)]
		impl<$($n: Print,)*> Print for ($($n,)*) {
			fn print(&self, f: &mut Printer) {
				let ($($n,)*) = self;
				$(f.val($n);)*
			}
		}

		#[allow(non_snake_case, unused)]
		impl<$($n: Parse,)*> Parse for ($($n,)*) {
			fn parse(f: &mut Parser) -> parse::Result<Self> {
				$(let $n = f.val()?;)*
				Ok(($($n,)*))
			}
		}
	},
	() => { tuple!(@single); },
	($first:ident $($rest:ident)*) => {
		tuple!(@single $first $($rest)*);
		tuple!($($rest)*);
	}
}

tuple!(A B C D E F G H I J K L);

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

pub fn print<T: PrintBlock>(value: &T, iset: &iset::InsnSet) -> String {
	let mut printer = Printer::new(iset);
	value.print_block(&mut printer);
	printer.finish()
}

pub fn parse<T: ParseBlock>(
	source: &str,
	iset: &iset::InsnSet,
) -> (Option<T>, Vec<parse::Diagnostic>) {
	parse::diagnose(|| {
		let mut parser = Parser::new(source, iset);
		let v = T::parse_block(&mut parser).emit();
		parser.check_labels();
		v
	})
}
