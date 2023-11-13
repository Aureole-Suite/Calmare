use crate::{types::NameId, util::ValueError};

use self::code::Game;

pub mod code;
pub mod ed6;

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
