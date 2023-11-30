use crate::macros::{newtype_hex, newtype_term, newtype_unit};
use crate::{Print, PrintContext, Printer, PrinterExt};
use themelios::types;

impl Print for types::Label {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		write!(f, "{:?}", self);
	}
}

impl Print for types::FileId {
	fn print(&self, f: &mut Printer, _ctx: &mut PrintContext) {
		write!(f.term("file").field(), "0x{:08X}", self.0);
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
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		let mut term = f.term("");
		term.field().val(self.x, ctx);
		term.field().word("null");
		term.field().val(self.z, ctx);
	}
}

impl Print for types::Pos3 {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		let mut term = f.term("");
		term.field().val(self.x, ctx);
		term.field().val(self.y, ctx);
		term.field().val(self.z, ctx);
	}
}

impl Print for types::TString {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		self.0.print(f, ctx)
	}
}