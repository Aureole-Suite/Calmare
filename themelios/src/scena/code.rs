use super::*;
use crate::types::*;

mod read;
pub(crate) use read::InsnReader;
pub use read::ReadError;

mod write;
pub(crate) use write::InsnWriter;
pub use write::WriteError;

pub mod decompile;
pub mod normalize;
pub mod visit;
pub mod visit_mut;

#[derive(Debug, Clone, PartialEq)]
pub struct Code(pub Vec<Insn>);

impl std::ops::Deref for Code {
	type Target = Vec<Insn>;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::ops::DerefMut for Code {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

#[derive(Clone, PartialEq)]
pub struct Insn {
	pub name: String,
	pub args: Vec<Arg>,
}

impl std::fmt::Debug for Insn {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if let ("if", [cond, a @ Arg::Code(_), b @ Arg::Code(c)]) = self.parts() {
			f.debug_tuple(&self.name).field(cond).field(a).finish()?;
			write!(f, " ")?;
			if c.len() == 1 && c[0].name == "if" {
				write!(f, "else ")?;
				c[0].fmt(f)
			} else {
				f.debug_tuple("else").field(b).finish()
			}
		} else {
			let mut f = f.debug_tuple(&self.name);
			for arg in &self.args {
				f.field(arg);
			}
			f.finish()
		}
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

#[derive(Clone, PartialEq)]
pub enum Arg {
	Label(Label),
	Tuple(Vec<Arg>),
	Code(Code),
	Expr(Box<Expr>),
	Atom(Atom),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Label(v) => v.fmt(f),
			Self::Tuple(v) => v.fmt(f),
			Self::Code(v) => v.fmt(f),
			Self::Expr(v) => f.debug_tuple("Expr").field(v).finish(),
			Self::Atom(v) => v.fmt(f),
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
	Int(i64), // easier to have a single integer type encompassing everything
	Float(f32),
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
	EntranceId(EntranceId),
	ObjectId(ObjectId),

	ForkId(ForkId),
	MenuId(MenuId),
	EffId(EffId),
	EffInstanceId(EffInstanceId),
	ChipId(ChipId),
	VisId(VisId),

	CharId(CharId),

	Flag(Flag),
	Var(Var),
	Global(Global),
	Attr(Attr),
	CharAttr(CharAttr),

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

#[derive(Clone, PartialEq)]
pub enum Expr {
	Atom(Atom),
	Bin(BinOp, Box<Expr>, Box<Expr>),
	Unary(UnOp, Box<Expr>),
	Assign(AssOp, Box<Expr>),
	Insn(Insn),
	Rand, // random 15-bit number
}

impl std::fmt::Debug for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Atom(v) => v.fmt(f),
			Self::Bin(op, a, b) => {
				let mut tup = f.debug_tuple(&format!("{op:?}"));
				fn rec(tup: &mut std::fmt::DebugTuple, op: BinOp, a: &Expr) {
					match a {
						Expr::Bin(o, a, b) if *o == op => {
							rec(tup, op, a);
							tup.field(b);
						}
						_ => {
							tup.field(a);
						}
					}
				}
				rec(&mut tup, *op, a);
				tup.field(b);
				tup.finish()
			}
			Self::Unary(op, a) => f.debug_tuple(&format!("{op:?}")).field(a).finish(),
			Self::Assign(op, a) => f.debug_tuple(&format!("{op:?}")).field(a).finish(),
			Self::Insn(v) => v.fmt(f),
			Self::Rand => write!(f, "Rand"),
		}
	}
}
