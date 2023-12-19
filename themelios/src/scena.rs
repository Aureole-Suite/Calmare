use snafu::prelude::*;

use crate::{gamedata::Game, types::NameId, util::ValueError};

pub mod code;
pub mod ed6;
pub mod ed7;

#[derive(Debug, Snafu)]
pub enum ReadError {
	#[snafu(context(false))]
	Gospel { source: gospel::read::Error },
	#[snafu(context(false))]
	Decode { source: crate::util::DecodeError },
	#[snafu(context(false))]
	Code { source: code::ReadError },
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
}

#[derive(Debug, Snafu)]
pub enum WriteError {
	#[snafu(context(false))]
	Gospel { source: gospel::write::Error },
	#[snafu(context(false))]
	Value { source: ValueError },
	#[snafu(context(false))]
	Code { source: code::WriteError },
	#[snafu(context(false))]
	Encode { source: crate::util::EncodeError },
	#[snafu(whatever, display("{message}"))]
	Whatever { message: String },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncId(pub u16, pub u16);

newtype!(ChipId(u16));
newtype!(LocalCharId(u16));
newtype!(CharFlags(u16));
newtype!(EventId(u16));
newtype!(EventFlags(u16));
newtype!(LookPointId(u16));
newtype!(LookPointFlags(u16));
newtype!(EntryId(u16));
newtype!(EntryFlags(u16));

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharId {
	Party(u16),
	Local(LocalCharId),
	Custom(u16),
	Party2(u16),
	Self_,
	Null,
	Name(NameId),
	Chest,
}

impl CharId {
	pub fn from_u16(g: Game, v: u16) -> Result<CharId, ValueError> {
		use Game::*;
		Ok(match v {
			999 => CharId::Chest,
			257.. => CharId::Name(NameId(v - 257)),
			256 => return Err(ValueError::new("CharId", v)),
			255 => CharId::Null,
			254 => CharId::Self_,
			244.. if g == Azure => CharId::Custom(v - 244),
			246.. if g == Sc => CharId::Party2(v - 246),
			238.. if g != Sc => CharId::Party2(v - 238),
			16.. if g == Tc => CharId::Local(LocalCharId(v - 16)),
			8.. if g != Tc => CharId::Local(LocalCharId(v - 8)),
			0.. => CharId::Party(v),
		})
	}

	pub fn to_u16(self, g: Game) -> Result<u16, ValueError> {
		use Game::*;
		Ok(match self {
			CharId::Party(v) => v,
			CharId::Local(v) if g == Tc => 16 + v.0,
			CharId::Local(v) => 8 + v.0,
			CharId::Party2(v) if g == Sc => 246 + v,
			CharId::Party2(v) => 238 + v,
			CharId::Custom(v) if g == Azure => 244 + v,
			CharId::Custom(_) => return Err(ValueError::new("u16", format!("{self:?}"))),
			CharId::Self_ => 254,
			CharId::Null => 255,
			CharId::Name(v) => 257 + v.0,
			CharId::Chest => 999,
		})
	}
}

newtype!(EntranceId(u16)); // defined in ._en file
newtype!(ObjectId(u16)); // defined in ._op file
newtype!(ForkId(u16));
newtype!(MenuId(u16));
newtype!(EffId(u8));
newtype!(EffInstanceId(u8));
newtype!(VisId(u8));

newtype!(Var(u16));
newtype!(Global(u8));
newtype!(Attr(u8));
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CharAttr(pub CharId, pub u8);
