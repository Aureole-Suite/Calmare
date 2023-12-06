use themelios::scena::code::Code;
use themelios::scena::insn::{Arg, Expr, Insn};
use themelios::scena::insn_set as iset;

use crate::parse::{self, Diagnostic, Span};
use crate::{Parse, ParseBlock, Parser};
use crate::{Print, PrintBlock, Printer};

mod expr;

impl PrintBlock for Code {
	fn print_block(&self, f: &mut Printer) {
		for insn in self.iter() {
			insn.print(f);
			if !f.is_line() {
				f.line();
			}
		}
	}
}

impl ParseBlock for Code {
	fn parse_block(f: &mut Parser) -> parse::Result<Self> {
		parse_code(f, false, false)
	}
}

fn parse_code(f: &mut Parser, cont: bool, brk: bool) -> Result<Code, Diagnostic> {
	let mut insns = Vec::new();
	f.lines(|f| {
		insns.push(f.val()?);
		Ok(())
	});
	Ok(Code(insns))
}

impl Print for Insn {
	fn print(&self, f: &mut Printer) {
		match self.parts() {
			("if", [args @ .., Arg::Code(yes), Arg::Code(no)]) => {
				f.word("if");
				for arg in args {
					f.val(arg);
				}
				f.val_block(yes);
				if no.len() == 1 && no[0].name == "if" {
					f.word("else").val(&no[0]);
				} else {
					f.word("else").val_block(no);
				}
			}

			(_, [param, expr @ Arg::Expr(_)]) => {
				f.val(param).val(expr);
			}

			("Menu", args) => {
				f.word("Menu");
				let mut n = 0;
				for arg in args {
					if matches!(arg, Arg::TString(_)) {
						f.line().indent(|f| {
							f.val(arg);
							write!(f, "// {n}");
						});
						n += 1;
					} else {
						f.val(arg);
					}
				}
			}

			_ => {
				f.word(&self.name);
				for arg in &self.args {
					f.val(arg);
				}
			}
		}
	}
}

impl Parse for Insn {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		let pos = f.pos()?;
		let word = f.word()?;
		let Some(args) = f.insn_set().insns_rev.get(word) else {
			return Err(Diagnostic::error(f.span(pos), "unknown instruction"));
		};
		Ok(Insn::new(word, parse_args(f, args)?))
	}
}

impl Print for Arg {
	fn print(&self, f: &mut Printer) {
		match self {
			Arg::Label(v) => f.val(v),

			Arg::Int(v) => f.val(v),
			Arg::String(v) => f.val(v),
			Arg::Time(v) => f.val(v),
			Arg::Angle(v) => f.val(v),
			Arg::Angle32(v) => f.val(v),
			Arg::Speed(v) => f.val(v),
			Arg::AngularSpeed(v) => f.val(v),
			Arg::Length(v) => f.val(v),
			Arg::Color(v) => f.val(v),

			Arg::Pos2(v) => f.val(v),
			Arg::Pos3(v) => f.val(v),
			Arg::RPos3(v) => f.val(v),

			Arg::TString(v) => f.val(v),
			Arg::Text(v) => f.val(v),
			Arg::Tuple(v) => {
				for v in v {
					f.val(v);
				}
				f
			}

			Arg::File(v) => f.val(v),
			Arg::Battle(v) => f.val(v),
			Arg::Bgm(v) => f.val(v),
			Arg::Item(v) => f.val(v),
			Arg::Magic(v) => f.val(v),
			Arg::Name(v) => f.val(v),
			Arg::Quest(v) => f.val(v),
			Arg::Recipe(v) => f.val(v),
			Arg::Shop(v) => f.val(v),
			Arg::Sound(v) => f.val(v),
			Arg::Town(v) => f.val(v),

			Arg::Func(v) => f.val(v),
			Arg::LookPoint(v) => f.val(v),
			Arg::Event(v) => f.val(v),

			Arg::Entrance(v) => f.term("entrance").field().val(v),
			Arg::Object(v) => f.term("object").field().val(v),
			Arg::ForkId(v) => f.term("fork").field().val(v),
			Arg::MenuId(v) => f.term("menu").field().val(v),
			Arg::EffId(v) => f.term("eff").field().val(v),
			Arg::EffInstanceId(v) => f.term("eff_instance").field().val(v),
			Arg::ChipId(v) => f.term("chip").field().val(v),
			Arg::VisId(v) => f.term("vis").field().val(v),

			Arg::Char(v) => f.val(v),

			Arg::Flag(v) => f.val(v),
			Arg::Var(v) => f.term("var").field().val(v),
			Arg::Global(v) => f.term("global").field().val(v),
			Arg::Attr(v) => f.term("system").field().val(v),
			Arg::CharAttr(v, w) => f.val(v).no_space().word(".").no_space().val(w),
			Arg::Code(v) => f.val_block(v),
			Arg::Expr(v) => f.val(v),

			Arg::QuestTask(v) => f.hex(v),
			Arg::QuestFlags(v) => f.hex(v),
			Arg::SystemFlags(v) => f.hex(v),
			Arg::LookPointFlags(v) => f.hex(v),
			Arg::ObjectFlags(v) => f.hex(v),
			Arg::EventFlags(v) => f.hex(v),
			Arg::CharFlags(v) => f.hex(v),
			Arg::CharFlags2(v) => f.hex(v),
		};
	}
}

fn parse_args(f: &mut Parser, iargs: &[iset::Arg]) -> parse::Result<Vec<Arg>> {
	let mut out = Vec::new();
	for iarg in iargs {
		parse_arg(&mut out, f, iarg)?;
	}
	Ok(out)
}

fn parse_arg(out: &mut Vec<Arg>, f: &mut Parser<'_>, iarg: &iset::Arg) -> Result<(), Diagnostic> {
	match iarg {
		iset::Arg::Int(_, iset::IntArg::Const(_)) => {}
		iset::Arg::Int(_, iarg) => {
			out.push(parse_int_arg(f, *iarg)?);
		}
		iset::Arg::Misc(iarg) => parse_misc_arg(out, f, iarg)?,
		iset::Arg::Tuple(iargs) => {
			let mut first = true;
			f.check("(")?;
			let mut tup = Vec::new();
			for iarg in iargs {
				if first {
					first = false;
				} else {
					f.check(",")?;
				}
				parse_arg(&mut tup, f, iarg)?;
			}
			f.check(")")?;
		}
	}
	Ok(())
}

fn parse_int_arg(f: &mut Parser, iarg: iset::IntArg) -> parse::Result<Arg> {
	use iset::IntArg as IA;

	fn term<T: Parse>(f: &mut Parser, word: &str) -> parse::Result<T> {
		f.check_word(word)?.term(|f| f.val())
	}

	Ok(match iarg {
		IA::Const(_) => unreachable!(),
		IA::Int => Arg::Int(f.val()?),
		IA::Address => Arg::Label(f.val()?),

		IA::Time => Arg::Time(f.val()?),
		IA::Length => Arg::Length(f.val()?),
		IA::Speed => Arg::Speed(f.val()?),
		IA::Angle => Arg::Angle(f.val()?),
		IA::AngularSpeed => Arg::AngularSpeed(f.val()?),
		IA::Angle32 => Arg::Angle32(f.val()?),
		IA::Color => Arg::Color(f.val()?),

		IA::FileId => Arg::File(f.val()?),
		IA::BattleId => Arg::Battle(f.val()?),
		IA::BgmId => Arg::Bgm(f.val()?),
		IA::ItemId => Arg::Item(f.val()?),
		IA::MagicId => Arg::Magic(f.val()?),
		IA::NameId => Arg::Name(f.val()?),
		IA::QuestId => Arg::Quest(f.val()?),
		IA::RecipeId => Arg::Recipe(f.val()?),
		IA::ShopId => Arg::Shop(f.val()?),
		IA::SoundId => Arg::Sound(f.val()?),
		IA::TownId => Arg::Town(f.val()?),
		IA::FuncId => Arg::Func(f.val()?),
		IA::LookPointId => Arg::LookPoint(f.val()?),
		IA::EventId => Arg::Event(f.val()?),

		IA::EntranceId => Arg::Entrance(term(f, "entrance")?),
		IA::ObjectId => Arg::Object(term(f, "object")?),
		IA::ForkId => Arg::ForkId(term(f, "fork")?),
		IA::MenuId => Arg::MenuId(term(f, "menu")?),
		IA::EffId => Arg::EffId(term(f, "eff")?),
		IA::EffInstanceId => Arg::EffInstanceId(term(f, "eff_instance")?),
		IA::ChipId => Arg::ChipId(term(f, "chip")?),
		IA::VisId => Arg::VisId(term(f, "vis")?),

		IA::CharId => Arg::Char(f.val()?),

		IA::Flag => Arg::Flag(f.val()?),
		IA::Var => Arg::Var(term(f, "var")?),
		IA::Global => Arg::Global(term(f, "global")?),
		IA::Attr => Arg::Attr(term(f, "system")?),
		IA::CharAttr => {
			let char = f.val()?;
			f.no_space().check(".")?;
			let attr = f.no_space().val()?;
			Arg::CharAttr(char, attr)
		}

		IA::QuestTask => Arg::QuestTask(f.val()?),
		IA::QuestFlags => Arg::SystemFlags(f.val()?),
		IA::SystemFlags => Arg::SystemFlags(f.val()?),
		IA::LookPointFlags => Arg::LookPointFlags(f.val()?),
		IA::ObjectFlags => Arg::ObjectFlags(f.val()?),
		IA::EventFlags => Arg::EventFlags(f.val()?),
		IA::CharFlags => Arg::CharFlags(f.val()?),
		IA::CharFlags2 => Arg::CharFlags2(f.val()?),
	})
}

fn parse_misc_arg(out: &mut Vec<Arg>, f: &mut Parser, iarg: &iset::MiscArg) -> parse::Result<()> {
	Err(Diagnostic::DUMMY)
}

impl Print for Expr {
	fn print(&self, f: &mut Printer) {
		expr::print(self, f)
	}
}
