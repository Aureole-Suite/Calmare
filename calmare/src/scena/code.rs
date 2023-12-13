use std::collections::BTreeMap;

use themelios::scena::code::{Arg, Atom, Code, Expr, Insn};
use themelios::scena::{insn_set as iset, CharId};
use themelios::types::Text;

use crate::parse::{self, Diagnostic, Emit as _, Span};
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
		Ok(parse_code(f, false, false))
	}
}

fn parse_code(f: &mut Parser, cont: bool, brk: bool) -> Code {
	let mut insns = Vec::new();
	f.lines(|f| {
		parse_remainder(f, |f| {
			let insn = parse_code_line(f, cont, brk)?;
			insns.push(insn);
			Ok(())
		})
	});
	Code(insns)
}

fn parse_remainder(
	f: &mut Parser,
	func: impl FnOnce(&mut Parser) -> Result<(), Diagnostic>,
) -> Result<(), Diagnostic> {
	let mut result = func(f);
	if f.pos().is_ok() {
		// Didn't parse whole line. Check if there's any colon,
		// and if so try to parse the inner block.
		let mut error =
			Diagnostic::error(f.raw_pos().as_span(), "expected end of line").filter(result.is_ok());
		match find_tail_on_error(f) {
			Ok(v) => {
				error.note(f.last_nonspace(), "skipping to here");
				error.emit();
				if v {
					parse_code(f, true, true);
				}
			}
			Err(e) => result = result.and(Err(e)),
		}
	}
	result
}

fn parse_code_line(f: &mut Parser<'_>, cont: bool, brk: bool) -> Result<Insn, Diagnostic> {
	let pos = f.pos()?;

	if f.check_word("if").is_ok() {
		let expr = f.val()?;
		f.check(":")?;
		let yes = parse_code(f, cont, brk);
		let mut args = vec![Arg::Expr(expr), Arg::Code(yes)];
		if f.allow_unindented(|f| f.check_word("else")).is_ok() {
			if f.peek_word() != Ok("if") {
				f.check(":")?;
			}
			let no = parse_code(f, cont, brk);
			args.push(Arg::Code(no));
		}
		return Ok(Insn::new("if", args));
	}

	if f.check_word("while").is_ok() {
		let expr = f.val()?;
		f.check(":")?;
		let body = parse_code(f, true, true);
		return Ok(Insn::new("while", vec![Arg::Expr(expr), Arg::Code(body)]));
	}

	if f.check_word("break").is_ok() {
		if !brk {
			Diagnostic::error(f.span(pos), "cannot break here").emit();
		}
		return Ok(Insn::new("break", vec![]));
	}

	if f.check_word("continue").is_ok() {
		if !cont {
			Diagnostic::error(f.span(pos), "cannot break here").emit();
		}
		return Ok(Insn::new("continue", vec![]));
	}

	if f.check_word("switch").is_ok() {
		let expr = f.val()?;
		f.check(":")?;
		let mut cases = Vec::new();
		let mut seen = BTreeMap::<Option<u16>, Span>::default();
		f.lines(|f| {
			parse_remainder(f, |f| {
				let pos = f.pos()?;
				let case = match f.word()? {
					"case" => Some(f.val()?),
					"default" => None,
					_ => {
						return Err(Diagnostic::error(
							f.span(pos),
							"expected `case` or `default`",
						))
					}
				};
				if let Some(prev) = seen.insert(case, f.span(pos)) {
					// I'd have this as an error, but the vanilla scripts do it, so...
					Diagnostic::warn(f.span(pos), "duplicate case")
						.with_note(prev, "previous here")
						.emit();
				}

				f.check(":")?;
				let body = parse_code(f, cont, true);
				cases.push(match case {
					Some(v) => {
						Insn::new("case", vec![Arg::Atom(Atom::Int(v as _)), Arg::Code(body)])
					}
					None => Insn::new("default", vec![Arg::Code(body)]),
				});
				Ok(())
			})
		});
		return Ok(Insn::new(
			"switch",
			vec![Arg::Expr(expr), Arg::Code(Code(cases))],
		));
	}

	if f.check_word("_label").is_ok() {
		let pos = f.pos()?;
		let label = f.val()?;
		f.define_label(label, f.raw_span(pos));
		return Ok(Insn::new("_label", vec![Arg::Label(label)]));
	}

	if let Some(lhs) = f.try_parse(expr::parse_lvalue)? {
		let name = match &lhs {
			Atom::Var(_) => Some("Var"),
			Atom::Global(_) => Some("Global"),
			Atom::Attr(_) => Some("Attr"),
			Atom::CharAttr(_) => Some("CharAttr"),
			_ => None,
		};
		let name = name
			.filter(|name| f.insn_set().insns_rev.contains_key(*name))
			.ok_or_else(|| Diagnostic::error(f.span(pos), "invalid lvalue"))
			.emit();

		let expr = expr::parse_assignment(f)?;
		if let Some(name) = name {
			return Ok(Insn::new(
				name,
				vec![Arg::Atom(lhs), Arg::Expr(Box::new(expr))],
			));
		} else {
			return Err(Diagnostic::DUMMY);
		}
	}

	f.val()
}

fn find_tail_on_error(f: &mut Parser) -> parse::Result<bool> {
	while f.pos().is_ok() {
		if f.check(":").is_ok() {
			return Ok(true);
		}
		if f.try_parse(String::parse)?.is_none() // these can contain colons
			&& f.try_parse(Text::parse)?.is_none()
		{
			f.any_char();
		}
	}
	Ok(false)
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
					if matches!(arg, Arg::Atom(Atom::TString(_))) {
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
		let word = f.peek_word()?;
		let Some(args) = f.insn_set().insns_rev.get(word) else {
			return Err(Diagnostic::error(f.span_of(word), "unknown instruction"));
		};
		Ok(Insn::new(f.word()?, parse_args(f, args)?))
	}
}

impl Print for Arg {
	fn print(&self, f: &mut Printer) {
		match self {
			Arg::Label(v) => f.val(v),
			Arg::Tuple(v) => {
				let mut term = f.term("");
				for v in v {
					term.field().val(v);
				}
				drop(term);
				f
			}
			Arg::Code(v) => f.val_block(v),
			Arg::Expr(v) => f.val(v),
			Arg::Atom(v) => f.val(v),
		};
	}
}

impl Print for Atom {
	fn print(&self, f: &mut Printer) {
		match self {
			Atom::Int(v) => f.val(v),
			Atom::String(v) => f.val(v),
			Atom::Time(v) => f.val(v),
			Atom::Angle(v) => f.val(v),
			Atom::Angle32(v) => f.val(v),
			Atom::Speed(v) => f.val(v),
			Atom::AngularSpeed(v) => f.val(v),
			Atom::Length(v) => f.val(v),
			Atom::Color(v) => f.val(v),

			Atom::Pos2(v) => f.val(v),
			Atom::Pos3(v) => f.val(v),
			Atom::RPos3(v) => f.val(v),

			Atom::TString(v) => f.val(v),
			Atom::Text(v) => f.val(v),

			Atom::FileId(v) => f.val(v),
			Atom::BattleId(v) => f.val(v),
			Atom::BgmId(v) => f.val(v),
			Atom::ItemId(v) => f.val(v),
			Atom::MagicId(v) => f.val(v),
			Atom::NameId(v) => f.val(v),
			Atom::QuestId(v) => f.val(v),
			Atom::RecipeId(v) => f.val(v),
			Atom::ShopId(v) => f.val(v),
			Atom::SoundId(v) => f.val(v),
			Atom::TownId(v) => f.val(v),

			Atom::FuncId(v) => f.val(v),
			Atom::LookPointId(v) => f.val(v),
			Atom::EventId(v) => f.val(v),

			Atom::EntranceId(v) => f.val(v),
			Atom::ObjectId(v) => f.val(v),
			Atom::ForkId(v) => f.val(v),
			Atom::MenuId(v) => f.val(v),
			Atom::EffId(v) => f.val(v),
			Atom::EffInstanceId(v) => f.val(v),
			Atom::ChipId(v) => f.val(v),
			Atom::VisId(v) => f.val(v),

			Atom::CharId(v) => f.val(v),

			Atom::Flag(v) => f.val(v),
			Atom::Var(v) => f.val(v),
			Atom::Global(v) => f.val(v),
			Atom::Attr(v) => f.val(v),
			Atom::CharAttr(v) => f.val(v),

			Atom::QuestTask(v) => f.hex(v),
			Atom::QuestFlags(v) => f.hex(v),
			Atom::SystemFlags(v) => f.hex(v),
			Atom::LookPointFlags(v) => f.hex(v),
			Atom::ObjectFlags(v) => f.hex(v),
			Atom::EventFlags(v) => f.hex(v),
			Atom::CharFlags(v) => f.hex(v),
			Atom::CharFlags2(v) => f.hex(v),
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
			out.push(Arg::Atom(parse_int_arg(f, *iarg)?));
		}
		iset::Arg::Misc(iarg) => parse_misc_arg(out, f, iarg)?,
		iset::Arg::Tuple(iargs) => {
			f.tuple(|tup| {
				let mut values = Vec::new();
				for iarg in iargs {
					parse_arg(&mut values, tup.field()?, iarg)?;
				}
				out.push(Arg::Tuple(values));
				Ok(())
			})?;
		}
	}
	Ok(())
}

fn parse_int_arg(f: &mut Parser, iarg: iset::IntArg) -> parse::Result<Atom> {
	use iset::IntArg as IA;
	match iarg {
		IA::Const(_) => unreachable!(),
		IA::Int => f.atom(Atom::Int),

		IA::Time => f.atom(Atom::Time),
		IA::Length => f.atom(Atom::Length),
		IA::Speed => f.atom(Atom::Speed),
		IA::Angle => f.atom(Atom::Angle),
		IA::AngularSpeed => f.atom(Atom::AngularSpeed),
		IA::Angle32 => f.atom(Atom::Angle32),
		IA::Color => f.atom(Atom::Color),

		IA::FileId => f.atom(Atom::FileId),
		IA::BattleId => f.atom(Atom::BattleId),
		IA::BgmId => f.atom(Atom::BgmId),
		IA::ItemId => f.atom(Atom::ItemId),
		IA::MagicId => f.atom(Atom::MagicId),
		IA::NameId => f.atom(Atom::NameId),
		IA::QuestId => f.atom(Atom::QuestId),
		IA::RecipeId => f.atom(Atom::RecipeId),
		IA::ShopId => f.atom(Atom::ShopId),
		IA::SoundId => f.atom(Atom::SoundId),
		IA::TownId => f.atom(Atom::TownId),
		IA::FuncId => f.atom(Atom::FuncId),
		IA::LookPointId => f.atom(Atom::LookPointId),
		IA::EventId => f.atom(Atom::EventId),

		IA::EntranceId => f.atom(Atom::EntranceId),
		IA::ObjectId => f.atom(Atom::ObjectId),
		IA::ForkId => f.atom(Atom::ForkId),
		IA::MenuId => f.atom(Atom::MenuId),
		IA::EffId => f.atom(Atom::EffId),
		IA::EffInstanceId => f.atom(Atom::EffInstanceId),
		IA::ChipId => f.atom(Atom::ChipId),
		IA::VisId => f.atom(Atom::VisId),

		IA::CharId => f.atom(Atom::CharId),

		IA::Flag => f.atom(Atom::Flag),
		IA::Var => f.atom(Atom::Var),
		IA::Global => f.atom(Atom::Global),
		IA::Attr => f.atom(Atom::Attr),
		IA::CharAttr => f.atom(Atom::CharAttr),

		IA::QuestTask => f.atom(Atom::QuestTask),
		IA::QuestFlags => f.atom(Atom::QuestFlags),
		IA::SystemFlags => f.atom(Atom::SystemFlags),
		IA::LookPointFlags => f.atom(Atom::LookPointFlags),
		IA::ObjectFlags => f.atom(Atom::ObjectFlags),
		IA::EventFlags => f.atom(Atom::EventFlags),
		IA::CharFlags => f.atom(Atom::CharFlags),
		IA::CharFlags2 => f.atom(Atom::CharFlags2),
	}
}

fn parse_misc_arg(out: &mut Vec<Arg>, f: &mut Parser, iarg: &iset::MiscArg) -> parse::Result<()> {
	fn list<F>(out: &mut Vec<Arg>, f: &mut Parser, mut func: F) -> parse::Result<()>
	where
		F: FnMut(&mut Parser) -> parse::Result<Arg>,
	{
		while f.pos().is_ok() {
			out.push(func(f)?);
		}
		Ok(())
	}

	fn tuple<T, F>(f: &mut Parser, mut func: F) -> parse::Result<Vec<T>>
	where
		F: FnMut(&mut Parser) -> parse::Result<T>,
	{
		let mut vals = Vec::new();
		f.tuple(|tup| {
			while let Some(f) = tup.try_field()? {
				vals.push(func(f)?);
			}
			Ok(())
		})?;
		Ok(vals)
	}

	use iset::MiscArg as MA;
	match iarg {
		MA::Label => out.push(Arg::Label(f.val()?)),
		MA::String => out.push(f.arg(Atom::String)?),
		MA::TString => out.push(f.arg(Atom::TString)?),
		MA::Text => list(out, f, |f| f.arg(Atom::Text))?,
		MA::Pos2 => out.push(f.arg(Atom::Pos2)?),
		MA::Pos3 => out.push(f.arg(Atom::Pos3)?),
		MA::RPos3 => out.push(f.arg(Atom::RPos3)?),
		MA::Expr => out.push(Arg::Expr(f.val()?)),
		MA::Fork | MA::ForkLoop(_) => out.push(Arg::Code(f.val_block()?)),
		MA::SwitchTable(_, _) => out.push(Arg::Tuple(tuple(f, |f| {
			f.tuple(|tup| {
				let a = tup.field()?.arg(Atom::Int)?;
				let b = tup.field()?.val()?;
				Ok(Arg::Tuple(vec![a, Arg::Label(b)]))
			})
		})?)),
		MA::QuestList => list(out, f, |f| f.arg(Atom::QuestId))?,
		MA::Menu => list(out, f, |f| f.arg(Atom::TString))?,
		MA::PartySelectMandatory => {
			let pos = f.pos()?;
			let items = tuple(f, |f| {
				if f.check_word("null").is_ok() {
					Ok(Arg::Atom(Atom::CharId(CharId::Null)))
				} else {
					f.arg(Atom::NameId)
				}
			})?;
			if items.len() != 4 {
				Diagnostic::error(f.span(pos), "must be 4 items").emit();
			}
			out.push(Arg::Tuple(items));
		}
		MA::PartySelectOptional => list(out, f, |f| f.arg(Atom::NameId))?,
		MA::TcMembers => out.push(Arg::Tuple(tuple(f, |f| f.arg(Atom::NameId))?)),
		MA::ED7CharAnimation => out.push(Arg::Tuple(tuple(f, |f| f.arg(Atom::Int))?)),
		MA::EvoSave => {
			if f.insn_set().variant == iset::Variant::Evo {
				out.push(f.arg(Atom::Int)?)
			}
		}
		MA::KaiSoundId => out.push(f.arg(Atom::SoundId)?),
		MA::ED7BattlePos => out.push(f.arg(Atom::BattleId)?),
		MA::FcPartyEquip => out.push(f.arg(Atom::Int)?),
		MA::ScPartySetSlot => out.push(f.arg(Atom::Int)?),
		MA::EffPlayPos => {
			if matches!(out[0], Arg::Atom(Atom::CharId(CharId::Null))) {
				out.push(f.arg(Atom::Pos3)?)
			} else {
				out.push(f.arg(Atom::RPos3)?)
			}
		}
	}
	Ok(())
}

impl Print for Expr {
	fn print(&self, f: &mut Printer) {
		expr::print(self, f)
	}
}

impl Parse for Expr {
	fn parse(f: &mut Parser) -> parse::Result<Self> {
		expr::parse(f)
	}
}

#[extend::ext]
impl<'src> Parser<'src> {
	fn atom<T: Parse>(&mut self, f: impl Fn(T) -> Atom) -> parse::Result<Atom> {
		self.val().map(f)
	}

	fn arg<T: Parse>(&mut self, f: impl Fn(T) -> Atom) -> parse::Result<Arg> {
		self.atom(f).map(Arg::Atom)
	}
}
