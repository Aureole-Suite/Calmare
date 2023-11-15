use gospel::read::{Le as _, Reader};
use snafu::prelude::*;

use super::{Arg, Insn};
use crate::scena::code::Code;
use crate::scena::insn::Unit;
use crate::scena::{expr::Expr, insn_set as iset, CharId, EventId, FuncId, LookPointId};
use crate::types::*;
use crate::util::ReaderExt;

#[derive(Debug, Snafu)]
pub enum ReadError {
	#[snafu(context(false))]
	Gospel { source: gospel::read::Error },
	#[snafu(context(false))]
	Decode { source: crate::util::DecodeError },
	#[snafu(context(false))]
	Value { source: crate::util::ValueError },
	#[snafu(context(false))]
	Code {
		source: Box<crate::scena::code::ReadError>,
	},
	#[snafu(context(false))]
	Expr {
		source: Box<crate::scena::expr::ReadError>,
	},
	#[snafu(whatever)]
	Whatever { message: String },
}

type Result<T, E = ReadError> = std::result::Result<T, E>;

pub(crate) fn read(f: &mut Reader, iset: &iset::InsnSet) -> Result<Insn> {
	let mut args = Vec::new();
	let byte = f.u8()? as usize;
	let name = read_insn(f, &mut args, iset, &iset.insns, byte)?;
	Ok(Insn::new(name, args))
}

fn read_insn<'a>(
	f: &mut Reader,
	out: &mut Vec<Arg>,
	iset: &'a iset::InsnSet,
	insns: &'a [iset::Insn],
	n: usize,
) -> Result<&'a str> {
	match insns.get(n) {
		None | Some(iset::Insn::Blank) => whatever!("unknown instruction {n:02X}"),
		Some(iset::Insn::Regular { name, args }) => {
			for arg in args {
				read_arg(f, out, iset, arg)?;
			}
			Ok(name)
		}
		Some(iset::Insn::Match { head, on, cases }) => {
			for arg in head {
				read_arg(f, out, iset, arg)?;
			}
			let n = read_int(f, *on)? as usize;
			read_insn(f, out, iset, cases, n)
		}
	}
}

fn read_int(f: &mut Reader, int: iset::IntArg) -> Result<i64> {
	Ok(match int {
		iset::IntArg::u8 => f.u8()? as i64,
		iset::IntArg::u16 => f.u16()? as i64,
		iset::IntArg::u24 => (f.u16()? as i64) | (f.u8()? as i64) << 8,
		iset::IntArg::u32 => f.u32()? as i64,
		iset::IntArg::i8 => f.i8()? as i64,
		iset::IntArg::i16 => f.i16()? as i64,
		iset::IntArg::i32 => f.i32()? as i64,
		iset::IntArg::Const(v) => v,
	})
}

fn read_arg(
	f: &mut Reader,
	out: &mut Vec<Arg>,
	iset: &iset::InsnSet,
	arg: &iset::Arg,
) -> Result<(), ReadError> {
	match arg {
		iset::Arg::Int(int, ty) => out.push(read_int_arg(f, iset, *int, *ty)?),
		iset::Arg::Misc(ty) => read_misc(f, out, iset, *ty)?,
		iset::Arg::Tuple(args) => {
			let mut values = Vec::new();
			for arg in args {
				read_arg(f, &mut values, iset, arg)?;
			}
			out.push(Arg::Tuple(values))
		}
	}
	Ok(())
}

fn read_misc(
	f: &mut Reader,
	out: &mut Vec<Arg>,
	iset: &iset::InsnSet,
	ty: iset::MiscArg,
) -> Result<()> {
	use iset::MiscArg as T;
	match ty {
		T::String => out.push(Arg::String(f.string()?)),
		T::TString => out.push(Arg::TString(TString(f.string()?))),
		T::Text => parse_text(f, out)?,

		T::Pos2 => out.push(Arg::Pos2(Pos2 {
			x: f.i32()?,
			z: f.i32()?,
		})),
		T::Pos3 => out.push(Arg::Pos3(Pos3 {
			x: f.i32()?,
			y: f.i32()?,
			z: f.i32()?,
		})),
		T::RPos3 => out.push(Arg::RPos3(Pos3 {
			x: f.i32()?,
			y: f.i32()?,
			z: f.i32()?,
		})),

		T::Expr => out.push(Arg::Expr(Expr::read(f, iset).map_err(Box::new)?)),

		T::Fork => {
			let len = f.u8()? as usize;
			let pos = f.pos();
			let code = Code::read(f, iset, Some(pos + len)).map_err(Box::new)?;
			if len > 0 {
				f.check_u8(0)?;
			}
			out.push(Arg::Code(code))
		}

		T::ForkLoop => {
			let len = f.u8()? as usize;
			let pos = f.pos();
			let code = Code::read(f, iset, Some(pos + len)).map_err(Box::new)?;

			let insns = [read(f, iset)?, read(f, iset)?];
			#[rustfmt::skip]
			ensure_whatever!(
				insns == [
					Insn::new(&iset.fork_loop_next, vec![]),
					Insn::new("_goto", vec![Arg::Address(pos)]),
				],
				"invalid ForkLoop: ended with {insns:?}"
			);
			out.push(Arg::Code(code))
		}

		T::SwitchTable => {
			let count = read_int(f, iset.switch_table_size)? as usize;
			let mut cs = Vec::with_capacity(count);
			for _ in 0..count {
				read_arg(f, &mut cs, iset, &iset.switch_table_type)?;
			}
			out.push(Arg::Tuple(cs))
		}

		T::QuestList => loop {
			match f.u16()? {
				0xFFFF => break,
				q => out.push(Arg::Quest(QuestId(q))),
			}
		},

		T::Menu => {
			for line in f.string()?.split_terminator('\x01') {
				out.push(Arg::TString(TString(line.to_owned())))
			}
		}

		T::FcPartyEquip => {
			if matches!(out[0], Arg::Item(ItemId(600..=799))) {
				out.push(Arg::Int(f.u8()? as _))
			} else {
				out.push(Arg::Int(0))
			}
		}

		T::EffPlayPos => {
			let pos = Pos3 {
				x: f.i32()?,
				y: f.i32()?,
				z: f.i32()?,
			};
			if matches!(out[0], Arg::Char(CharId::Null)) {
				out.push(Arg::Pos3(pos))
			} else {
				out.push(Arg::RPos3(pos))
			}
		}
	}
	Ok(())
}

fn parse_text(f: &mut Reader, out: &mut Vec<Arg>) -> Result<()> {
	loop {
		let (page, more) = parse_text_page(f)?;
		out.push(Arg::Text(page));
		if !more {
			break;
		}
	}
	Ok(())
}

fn parse_text_page(f: &mut Reader) -> Result<(TString, bool)> {
	let mut buf = Vec::new();
	let more = loop {
		match f.u8()? {
			0x00 => break false,
			0x01 => buf.extend(b"\\n"), // newline → line feed
			0x02 => buf.extend(b"\\f"), // pause → page break
			0x03 => break true,         // page break
			0x07 => buf.extend(format!("\\{}C", f.u8()?).as_bytes()),
			0x0D => buf.extend(b"\\r"),
			0x1F => buf.extend(format!("\\{}i", f.u16()?).as_bytes()),
			ch @ (0x00..=0x1F) => buf.extend(format!("\\{}x", ch).as_bytes()),
			b'\\' => buf.extend(b"\\\\"),
			ch @ 0x20.. => buf.push(ch),
		}
	};
	let string = crate::util::decode(&buf)?;
	Ok((TString(string), more))
}

fn read_int_arg(
	f: &mut Reader,
	iset: &iset::InsnSet,
	int: iset::IntArg,
	ty: iset::IntType,
) -> Result<Arg> {
	use crate::util::cast;
	use iset::IntType as T;

	let v = read_int(f, int)?;
	Ok(match ty {
		T::Int => Arg::Int(v),

		T::Address => Arg::Address(cast(v)?),

		T::Time => Arg::Scalar(cast(v)?, Unit::Time),
		T::Length => Arg::Scalar(cast(v)?, Unit::Length),
		T::Speed => Arg::Scalar(cast(v)?, Unit::Speed),
		T::Angle => Arg::Scalar(cast(v)?, Unit::Angle),
		T::AngularSpeed => Arg::Scalar(cast(v)?, Unit::AngularSpeed),
		T::Angle32 => Arg::Angle32(cast(v)?),
		T::Color => Arg::Color(cast(v)?),

		T::FileId => Arg::File(FileId(cast(v)?)),

		T::BattleId => Arg::Battle(BattleId(cast(v)?)),
		T::BgmId => Arg::Bgm(BgmId(cast(v)?)),
		T::ItemId => Arg::Item(ItemId(cast(v)?)),
		T::MagicId => Arg::Magic(MagicId(cast(v)?)),
		T::NameId => Arg::Name(NameId(cast(v)?)),
		T::QuestId => Arg::Quest(QuestId(cast(v)?)),
		T::RecipeId => Arg::Recipe(RecipeId(cast(v)?)),
		T::ShopId => Arg::Shop(ShopId(cast(v)?)),
		T::SoundId => Arg::Sound(SoundId(cast(v)?)),
		T::TownId => Arg::Town(TownId(cast(v)?)),

		T::FuncId => {
			let [a, b] = u16::to_le_bytes(cast(v)?);
			Arg::Func(FuncId(a as u16, b as u16))
		}
		T::LookPointId => Arg::LookPoint(LookPointId(cast(v)?)),
		T::EventId => Arg::Event(EventId(cast(v)?)),
		T::EntranceId => Arg::Entrance(cast(v)?),
		T::ObjectId => Arg::Object(cast(v)?),

		T::ForkId => Arg::ForkId(cast(v)?),
		T::MenuId => Arg::MenuId(cast(v)?),
		T::EffId => Arg::EffId(cast(v)?),
		T::EffInstanceId => Arg::EffInstanceId(cast(v)?),
		T::ChipId => Arg::ChipId(cast(v)?),

		T::CharId => Arg::Char(CharId::from_u16(iset.game, cast(v)?)?),

		T::Flag => Arg::Flag(Flag(cast(v)?)),
		T::Var => Arg::Var(cast(v)?),
		T::Attr => Arg::Attr(cast(v)?),
		T::CharAttr => {
			let char = CharId::from_u16(iset.game, cast(v & 0xFFFF)?)?;
			let attr: u8 = cast(v >> 16)?;
			Arg::CharAttr(char, attr)
		}

		T::QuestTask => Arg::QuestTask(cast(v)?),
		T::QuestFlags => Arg::QuestFlags(cast(v)?),
		T::SystemFlags => Arg::SystemFlags(cast(v)?),
		T::LookPointFlags => Arg::LookPointFlags(cast(v)?),
		T::ObjectFlags => Arg::ObjectFlags(cast(v)?),
		T::EventFlags => Arg::EventFlags(cast(v)?),
		T::CharFlags => Arg::CharFlags(cast(v)?),
		T::CharFlags2 => Arg::CharFlags2(cast(v)?),
	})
}
