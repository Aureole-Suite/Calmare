use gospel::read::Reader;
use gospel::write::Writer;
use snafu::prelude::*;

use crate::scena::{
	code::Code, expr::Expr, insn_set as iset, CharId, EventId, FuncId, LookPointId,
};
use crate::types::*;

mod read;
pub use read::ReadError;

#[derive(Debug, Snafu)]
pub enum WriteError {
	Bar,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Insn {
	pub name: String,
	pub args: Vec<Arg>,
}

impl std::fmt::Debug for Insn {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut f = f.debug_tuple(&self.name);
		for arg in &self.args {
			f.field(arg);
		}
		f.finish()
	}
}

impl Insn {
	pub fn new(name: &str, args: Vec<Arg>) -> Insn {
		Insn {
			name: name.to_string(),
			args,
		}
	}

	pub fn parts(&self) -> (&str, &[Arg]) {
		(&self.name, &self.args)
	}

	pub(crate) fn read(f: &mut Reader, iset: &iset::InsnSet) -> Result<Insn, ReadError> {
		read::read(f, iset)
	}

	pub(crate) fn write(
		f: &mut Writer,
		iset: &iset::InsnSet,
		insn: &Insn,
	) -> Result<(), WriteError> {
		todo!()
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
	Label(usize),

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

	Tuple(Vec<Arg>),
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
	CharFlags(u16),
	CharFlags2(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Unit {
	Time,
	Length,
	Speed,
	Angle,
	AngularSpeed,
}
