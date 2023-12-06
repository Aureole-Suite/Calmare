use crate::scena::{code::Code, CharId, EventId, FuncId, LookPointId};
use crate::types::*;

mod read;
pub(crate) use read::InsnReader;
pub use read::ReadError;

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
	Label(Label),

	Int(i64), // easier to have a single integer type encompassing everything
	String(String),

	Time(Time),
	Angle(Angle),
	Angle32(Angle32),
	Speed(Speed),
	AngularSpeed(AngularSpeed),
	Length(Length),
	Color(Color),

	Pos2(Pos2),
	Pos3(Pos3),
	RPos3(Pos3),

	TString(TString),
	Text(Text),

	Tuple(Vec<Arg>),
	FileId(FileId),

	BattleId(BattleId), // This one's weird since it's global in Sky but local in CB
	BgmId(BgmId),
	ItemId(ItemId),
	MagicId(MagicId),
	NameId(NameId),
	QuestId(QuestId),
	RecipeId(RecipeId),
	ShopId(ShopId),
	SoundId(SoundId),
	TownId(TownId),

	FuncId(FuncId),
	LookPointId(LookPointId),
	EventId(EventId),
	EntranceId(u16), // defined in ._en file
	ObjectId(u16),   // defined in ._op file

	ForkId(u16),
	MenuId(u16),
	EffId(u8),
	EffInstanceId(u8),
	ChipId(u16),
	VisId(u8),

	CharId(CharId),

	Flag(Flag),
	Var(u16),
	Global(u8),
	Attr(u8),
	CharAttr(CharId, u8),

	Code(Code),
	Expr(Box<Expr>),

	QuestTask(u16),
	QuestFlags(u8),
	SystemFlags(u32),
	LookPointFlags(u16),
	ObjectFlags(u32),
	EventFlags(u16),
	CharFlags(u16),
	CharFlags2(u16),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum BinOp {
	Eq = 0x02,      // ==
	Ne = 0x03,      // !=
	Lt = 0x04,      // <
	Gt = 0x05,      // >
	Le = 0x06,      // <=
	Ge = 0x07,      // >=
	BoolAnd = 0x09, // &&
	And = 0x0A,     // &
	Or = 0x0B,      // | and ||
	Add = 0x0C,     // +
	Sub = 0x0D,     // -
	Xor = 0x0F,     // ^
	Mul = 0x10,     // *
	Div = 0x11,     // /
	Mod = 0x12,     // %
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum UnOp {
	Not = 0x08, // !
	Neg = 0x0E, // -
	Inv = 0x1D, // ~
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)] // comment to trick rustfmt
#[derive(num_enum::TryFromPrimitive, num_enum::IntoPrimitive)]
#[repr(u8)]
pub enum AssOp {
	Ass = 0x13,    // =
	MulAss = 0x14, // *=
	DivAss = 0x15, // /=
	ModAss = 0x16, // %=
	AddAss = 0x17, // +=
	SubAss = 0x18, // -=
	AndAss = 0x19, // &=
	XorAss = 0x1A, // ^=
	OrAss = 0x1B,  // |=
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
	Arg(Arg),
	Bin(BinOp, Box<Expr>, Box<Expr>),
	Unary(UnOp, Box<Expr>),
	Assign(AssOp, Box<Expr>),
	Insn(Insn),
	Rand, // random 15-bit number
}
