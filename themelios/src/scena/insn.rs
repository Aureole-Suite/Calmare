use crate::scena::{
	code::Code, CharId, EventId, FuncId, LookPointId,
};
use crate::types::*;

mod read;
pub use read::ReadError;
pub(crate) use read::InsnReader;

mod write;
pub(crate) use write::InsnWriter;
pub use write::WriteError;

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
	Global(u8),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpKind {
	Unary,
	Binary, // includes comparisons
	Assign,
}

#[derive(
	Debug, Clone, Copy, PartialEq, Eq, num_enum::TryFromPrimitive, num_enum::IntoPrimitive,
)]
#[repr(u8)]
pub enum Op {
	Eq = 0x02,      // ==
	Ne = 0x03,      // !=
	Lt = 0x04,      // <
	Gt = 0x05,      // >
	Le = 0x06,      // <=
	Ge = 0x07,      // >=
	Not = 0x08,     // !
	BoolAnd = 0x09, // &&
	And = 0x0A,     // &
	Or = 0x0B,      // | and ||
	Add = 0x0C,     // +
	Sub = 0x0D,     // -
	Neg = 0x0E,     // -
	Xor = 0x0F,     // ^
	Mul = 0x10,     // *
	Div = 0x11,     // /
	Mod = 0x12,     // %
	Ass = 0x13,     // =
	MulAss = 0x14,  // *=
	DivAss = 0x15,  // /=
	ModAss = 0x16,  // %=
	AddAss = 0x17,  // +=
	SubAss = 0x18,  // -=
	AndAss = 0x19,  // &=
	XorAss = 0x1A,  // ^=
	OrAss = 0x1B,   // |=
	Inv = 0x1D,     // ~
}

impl Op {
	pub fn kind(self) -> OpKind {
		use Op::*;
		match self {
			Not | Neg | Inv => OpKind::Unary,
			Eq | Ne | Lt | Le | Gt | Ge => OpKind::Binary,
			BoolAnd | And | Or | Add | Sub | Xor | Mul | Div | Mod => OpKind::Binary,
			Ass | MulAss | DivAss | ModAss | AddAss | SubAss | AndAss | XorAss | OrAss => {
				OpKind::Assign
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Term {
	Arg(Arg),
	Op(Op),
	Insn(Box<Insn>),
	Rand, // random 15-bit number
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr(pub Vec<Term>);
