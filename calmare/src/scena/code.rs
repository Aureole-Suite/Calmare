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
		Ok(Insn::new(word, parse_args(&args)?))
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

fn parse_args(iargs: &[iset::Arg]) -> parse::Result<Vec<Arg>> {
	Ok(vec![])
}

impl Print for Expr {
	fn print(&self, f: &mut Printer) {
		expr::print(self, f)
	}
}
