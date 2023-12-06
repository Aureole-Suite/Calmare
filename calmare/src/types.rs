use crate::macros::{newtype_hex, newtype_term, newtype_unit};
use crate::{parse, Parse, Parser};
use crate::{Print, Printer};
use themelios::types;

impl Print for types::Label {
	fn print(&self, f: &mut Printer) {
		write!(f, "{:?}", self);
	}
}

impl Parse for types::Label {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		Err(parse::Diagnostic::info(f.pos()?.as_span(), "TODO"))
	}
}

impl Print for types::FileId {
	fn print(&self, f: &mut Printer) {
		write!(f.term("file").field(), "0x{:08X}", self.0);
	}
}

impl Parse for types::FileId {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.check_word("file")?;
		Ok(Self(f.sqbrack_val()?))
	}
}

newtype_hex!(types::Color);

newtype_term!(types::BattleId, "battle");
newtype_term!(types::BgmId, "bgm");
newtype_term!(types::ItemId, "item");
newtype_term!(types::MagicId, "magic");
newtype_term!(types::NameId, "name");
newtype_term!(types::QuestId, "quest");
newtype_term!(types::RecipeId, "recipe");
newtype_term!(types::ShopId, "shop");
newtype_term!(types::SoundId, "sound");
newtype_term!(types::TownId, "town");
newtype_term!(types::Flag, "flag");

newtype_unit!(types::Time, "ms");
newtype_unit!(types::Angle, "deg");
newtype_unit!(types::Angle32, "mdeg");
newtype_unit!(types::Speed, "mm/s");
newtype_unit!(types::AngularSpeed, "deg/s");
newtype_unit!(types::Length, "mm");

impl Print for types::Pos2 {
	fn print(&self, f: &mut Printer) {
		let mut term = f.term("");
		term.field().val(self.x);
		term.field().word("null");
		term.field().val(self.z);
	}
}

impl Parse for types::Pos2 {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.check("(")?;
		let x = f.val()?;
		f.check(",")?;
		f.check_word("null")?;
		f.check(",")?;
		let z = f.val()?;
		f.check(")")?;
		Ok(types::Pos2 { x, z })
	}
}

impl Print for types::Pos3 {
	fn print(&self, f: &mut Printer) {
		let mut term = f.term("");
		term.field().val(self.x);
		term.field().val(self.y);
		term.field().val(self.z);
	}
}

impl Parse for types::Pos3 {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		f.check("(")?;
		let x = f.val()?;
		f.check(",")?;
		let y = f.val()?;
		f.check(",")?;
		let z = f.val()?;
		f.check(")")?;
		Ok(types::Pos3 { x, y, z })
	}
}
