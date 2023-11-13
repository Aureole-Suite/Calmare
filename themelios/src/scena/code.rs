use gospel::read::Reader;
use gospel::write::Writer;
use snafu::prelude::*;

use crate::types::*;

use self::expr::Expr;

use super::*;

pub mod expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Code;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Game {
	Fc,
	Sc,
	Tc,
	Zero,
	Azure,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InsnTable {
	pub game: Game,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Insn {
	name: String,
	args: Vec<Operand>,
}

impl Insn {
	fn read(f: &mut Reader, iset: &InsnTable) -> Result<Insn, ReadError> {
		todo!()
	}

	fn write(f: &mut Writer, iset: &InsnTable, insn: &Insn) -> Result<(), WriteError> {
		todo!()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operand {
	Address(usize),

	Int(i64), // easier to have a single integer type encompassing everything
	String(String),

	Scalar(i32, Unit),
	Angle32(i32),
	Color(u32),

	Pos2(Pos2),
	Pos3(Pos3),
	RPos3(Pos3),

	TString(TString),
	Text(TString),

	Tuple(Vec<Operand>),
	File(FileId),

	Battle(BattleId), // This one's weird since it's global in Sky but local in CB
	Bgm(BgmId),
	Item(ItemId),
	Magic(MagicId),
	Name(NameId),
	Quest(QuestId),
	Recipe(RecipeId),
	Shop(ShopId),
	Sound(SoundId),
	Town(TownId),

	Func(FuncId),
	LookPoint(LookPointId),
	Event(EventId),
	Entrance(u16), // defined in ._en file
	Object(u16),   // defined in ._op file

	ForkId(u16),
	MenuId(u16),
	EffId(u8),
	EffInstanceId(u8),
	ChipId(u16),

	Char(CharId),

	Flag(Flag),
	Var(u16),
	Attr(u8),
	CharAttr(CharId, u8),

	Code(Code),
	Expr(Expr),

	QuestTask(u16),
	QuestFlags(u8),
	SystemFlags(u32),
	LookPointFlags(u16),
	ObjectFlags(u16),
	EventFlags(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unit {
	Time,
	Length,
	Speed,
	Angle,
	AngularSpeed,
}

#[derive(Debug, Snafu)]
pub enum ReadError {
	Foo,
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	Bar,
}

impl Code {
	pub fn read(
		start: &mut Reader,
		insn: &InsnTable,
		end: Option<usize>,
	) -> Result<Code, ReadError> {
		todo!()
	}

	pub fn write(code: &mut Writer, insn: &InsnTable, func: &Code) -> Result<(), WriteError> {
		todo!()
	}
}
