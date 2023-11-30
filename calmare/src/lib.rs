#![feature(decl_macro)]

pub use calmare_syntax::Printer;

mod scena;
mod types;

mod macros;

#[derive(Debug)]
pub struct PrintContext {}

#[extend::ext]
pub(crate) impl Printer {
	fn val(&mut self, val: impl Print, ctx: &mut PrintContext) -> &mut Self {
		val.print(ctx, self);
		self.space()
	}

	fn kv_line(&mut self, key: &str, val: impl Print, ctx: &mut PrintContext) -> &mut Self {
		self.word(key).val(val, ctx).line()
	}
}

pub trait Print {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer);
}

macros::number!(u8, u16, u32, u64, i8, i16, i32, i64, f32, f64);

impl<T: Print> Print for &T {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		T::print(self, ctx, f)
	}
}

impl<A: Print, B: Print> Print for (A, B) {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		f.val(&self.0, ctx).val(&self.1, ctx);
	}
}

impl<A: Print, B: Print, C: Print> Print for (A, B, C) {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		f.val(&self.0, ctx).val(&self.1, ctx).val(&self.2, ctx);
	}
}

impl Print for str {
	fn print(&self, _ctx: &mut PrintContext, f: &mut Printer) {
		write!(f, "{self:?}"); // TODO
	}
}

impl Print for String {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		self.as_str().print(ctx, f)
	}
}
