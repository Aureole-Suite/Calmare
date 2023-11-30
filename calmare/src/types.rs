use crate::macros::{newtype_hex, newtype_term, newtype_unit};
use crate::{Print, PrintContext, Printer, PrinterExt};
use themelios::types;

// Label(usize)
// newtype!(FileId(u32), "0x{:08X}");

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
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		let mut term = f.term("");
		term.field().val(self.x, ctx);
		term.field().word("null");
		term.field().val(self.z, ctx);
	}
}

impl Print for types::Pos3 {
	fn print(&self, ctx: &mut PrintContext, f: &mut Printer) {
		let mut term = f.term("");
		term.field().val(self.x, ctx);
		term.field().val(self.y, ctx);
		term.field().val(self.z, ctx);
	}
}

// pub struct TString(pub String);
