use themelios::scena;

mod code;
mod ed6;

use crate::macros::{newtype_hex, newtype_term};
use crate::{parse, Parse, Parser};
use crate::{Print, Printer};

impl Print for scena::FuncId {
	fn print(&self, f: &mut Printer) {
		let mut term = f.term("fn");
		term.field().val(self.0);
		term.field().val(self.1);
	}
}

impl Parse for scena::FuncId {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.check_word("fn")?;
		f.check("[")?;
		let a = f.val()?;
		f.check(",")?;
		let b = f.val()?;
		f.check("]")?;
		Ok(scena::FuncId(a, b))
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
	fn print(&self, f: &mut Printer) {
		use scena::CharId as C;
		match self {
			C::Party(v) => f.term("field_party").field().val(v),
			C::Local(v) => f.val(v),
			C::Custom(v) => f.term("custom").field().val(v),
			C::Party2(v) => f.term("party").field().val(v),
			C::Self_ => f.word("self"),
			C::Null => f.word("null"),
			C::Name(v) => f.val(v),
			C::Chest => f.word("chest"),
		};
	}
}

impl Parse for scena::CharId {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		use scena::CharId as C;
		let v = if f.check_word("field_party").is_ok() {
			C::Party(f.sqbrack_val()?)
		} else if let Some(v) = f.try_val()? {
			C::Local(v)
		} else if f.check_word("custom").is_ok() {
			C::Custom(f.sqbrack_val()?)
		} else if f.check_word("party").is_ok() {
			C::Party2(f.sqbrack_val()?)
		} else if f.check_word("self").is_ok() {
			C::Self_
		} else if f.check_word("null").is_ok() {
			C::Null
		} else if let Some(v) = f.try_val()? {
			C::Name(v)
		} else if f.check_word("chest").is_ok() {
			C::Chest
		} else {
			let word = f.peek_word()?;
			return Err(parse::Diagnostic::info(f.span_of(word), "expected charid"));
		};
		Ok(v)
	}
}

newtype_term!(scena::EntranceId, "entrance"); // defined in ._en file
newtype_term!(scena::ObjectId, "object"); // defined in ._op file
newtype_term!(scena::ForkId, "fork");
newtype_term!(scena::MenuId, "menu");
newtype_term!(scena::EffId, "eff");
newtype_term!(scena::EffInstanceId, "eff_instance");
newtype_term!(scena::VisId, "vis");

newtype_term!(scena::Var, "var");
newtype_term!(scena::Global, "global");
newtype_term!(scena::Attr, "system");

impl Print for scena::CharAttr {
	fn print(&self, f: &mut Printer) {
		f.val(self.0).no_space().word(".").no_space().val(self.1);
	}
}

impl Parse for scena::CharAttr {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		let char = f.val()?;
		f.no_space().check(".")?;
		let attr = f.no_space().val()?;
		Ok(scena::CharAttr(char, attr))
	}
}
