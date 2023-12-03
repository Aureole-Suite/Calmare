use themelios::scena;

mod code;
mod ed6;

use crate::macros::{newtype_hex, newtype_term};
use crate::{parse, Parse, ParseContext, Parser};
use crate::{Print, PrintContext, Printer};

impl Print for scena::FuncId {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		let mut term = f.term("fn");
		term.field().val(self.0, ctx);
		term.field().val(self.1, ctx);
	}
}

impl Parse for scena::FuncId {
	fn parse(f: &mut Parser, ctx: &mut ParseContext) -> parse::Result<Self> {
		f.check_word("fn")?.space()?.term(|f| {
			let a = Parse::parse(f, ctx)?;
			f.space()?.check(",")?.space()?;
			let b = Parse::parse(f, ctx)?;
			Ok(scena::FuncId(a, b))
		})
	}
}

newtype_term!(scena::ChipId, "chip");
newtype_term!(scena::LocalCharId, "char");
newtype_hex!(scena::CharFlags);
newtype_term!(scena::EventId, "event");
newtype_hex!(scena::EventFlags);
newtype_term!(scena::LookPointId, "look_point");
newtype_hex!(scena::LookPointFlags);
newtype_term!(scena::EntryId, "entry");
newtype_hex!(scena::EntryFlags);

impl Print for scena::CharId {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		use scena::CharId as C;
		match self {
			C::Party(v) => f.term("field_party").field().val(v, ctx),
			C::Local(v) => f.val(v, ctx),
			C::Custom(v) => f.term("custom").field().val(v, ctx),
			C::Party2(v) => f.term("party").field().val(v, ctx),
			C::Self_ => f.word("self"),
			C::Null => f.word("null"),
			C::Name(v) => f.val(v, ctx),
			C::Chest => f.word("chest"),
		};
	}
}
