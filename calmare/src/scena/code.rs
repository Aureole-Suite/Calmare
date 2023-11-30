use themelios::scena::{
	code::Code,
	insn::{Arg, Expr, Insn},
};

use crate::{Print, PrintContext, Printer, PrinterExt};

impl Print for Code {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		f.block(|f| {
			for insn in self.iter() {
				insn.print(f, ctx);
				if !f.is_line() {
					f.line();
				}
			}
		});
	}
}

impl Print for Insn {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		if let ("if", [args @ .., Arg::Code(yes), Arg::Code(no)]) = self.parts() {
			f.word("if");
			for arg in args {
				f.val(arg, ctx);
			}
			f.val(yes, ctx);
			if no.len() == 1 && no[0].name == "if" {
				f.word("else").val(&no[0], ctx);
			} else {
				f.word("else").val(no, ctx);
			}
		} else {
			f.word(&self.name);
			for arg in &self.args {
				f.val(arg, ctx);
			}
		}
	}
}

impl Print for Arg {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		match self {
			Arg::Label(v) => f.val(v, ctx),

			Arg::Int(v) => f.val(v, ctx),
			Arg::String(v) => f.val(v, ctx),
			Arg::Time(v) => f.val(v, ctx),
			Arg::Angle(v) => f.val(v, ctx),
			Arg::Angle32(v) => f.val(v, ctx),
			Arg::Speed(v) => f.val(v, ctx),
			Arg::AngularSpeed(v) => f.val(v, ctx),
			Arg::Length(v) => f.val(v, ctx),
			Arg::Color(v) => f.val(v, ctx),

			Arg::Pos2(v) => f.val(v, ctx),
			Arg::Pos3(v) => f.val(v, ctx),
			Arg::RPos3(v) => f.val(v, ctx),

			Arg::TString(v) => f.val(v, ctx),
			Arg::Text(v) => f.val(v, ctx),
			Arg::Tuple(v) => {
				for v in v {
					f.val(v, ctx);
				}
				f
			}

			Arg::File(v) => f.val(v, ctx),
			Arg::Battle(v) => f.val(v, ctx),
			Arg::Bgm(v) => f.val(v, ctx),
			Arg::Item(v) => f.val(v, ctx),
			Arg::Magic(v) => f.val(v, ctx),
			Arg::Name(v) => f.val(v, ctx),
			Arg::Quest(v) => f.val(v, ctx),
			Arg::Recipe(v) => f.val(v, ctx),
			Arg::Shop(v) => f.val(v, ctx),
			Arg::Sound(v) => f.val(v, ctx),
			Arg::Town(v) => f.val(v, ctx),

			Arg::Func(v) => f.val(v, ctx),
			Arg::LookPoint(v) => f.val(v, ctx),
			Arg::Event(v) => f.val(v, ctx),
			Arg::Entrance(v) => f.val(v, ctx),
			Arg::Object(v) => f.val(v, ctx),
			Arg::ForkId(v) => f.val(v, ctx),
			Arg::MenuId(v) => f.val(v, ctx),
			Arg::EffId(v) => f.val(v, ctx),
			Arg::EffInstanceId(v) => f.val(v, ctx),
			Arg::ChipId(v) => f.val(v, ctx),
			Arg::VisId(v) => f.val(v, ctx),

			Arg::Char(v) => f.val(v, ctx),

			Arg::Flag(v) => f.val(v, ctx),
			Arg::Var(v) => f.val(v, ctx),
			Arg::Global(v) => f.val(v, ctx),
			Arg::Attr(v) => f.val(v, ctx),
			Arg::CharAttr(v, w) => f.val(v, ctx).no_space().word(".").no_space().val(w, ctx),
			Arg::Code(v) => f.val(v, ctx),
			Arg::Expr(v) => f.val(v, ctx),

			Arg::QuestTask(v) => f.val(v, ctx),
			Arg::QuestFlags(v) => f.val(v, ctx),
			Arg::SystemFlags(v) => f.val(v, ctx),
			Arg::LookPointFlags(v) => f.val(v, ctx),
			Arg::ObjectFlags(v) => f.val(v, ctx),
			Arg::EventFlags(v) => f.val(v, ctx),
			Arg::CharFlags(v) => f.val(v, ctx),
			Arg::CharFlags2(v) => f.val(v, ctx),
		};
	}
}

impl Print for Expr {
	fn print(&self, f: &mut Printer, ctx: &mut PrintContext) {
		write!(f, "{:?}", self);
	}
}
