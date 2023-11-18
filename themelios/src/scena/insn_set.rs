use serde::Deserialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Game {
	Fc,
	Sc,
	Tc,
	Zero,
	Azure,
}

#[serde_with::serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct InsnSet {
	pub game: Game,
	pub switch_table_size: IntArg,
	pub switch_table_type: Arg,
	pub fork_loop_next: String,
	#[serde_as(as = "[_; 256]")]
	pub insns: [Insn; 256],
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Insn {
	Blank,
	Regular {
		name: String,
		args: Vec<Arg>,
	},
	Match {
		head: Vec<Arg>,
		on: IntArg,
		cases: Vec<Insn>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
	Int(IntArg, IntType),
	Misc(MiscArg),
	Tuple(Vec<Arg>),
	Const(IntArg, i64),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntArg {
	u8,
	u16,
	u24,
	u32,
	i8,
	i16,
	i32,
	Const(i64),
}

// Ugly implementation detail
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(remote = "IntArg")]
enum IntArg_inner {
	u8,
	u16,
	u24,
	u32,
	i8,
	i16,
	i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum IntType {
	#[serde(skip)]
	Int,

	Address,

	Time,
	Length,
	Speed,
	Angle,
	AngularSpeed,
	Angle32,
	Color,

	FileId,

	BattleId,
	BgmId,
	ItemId,
	MagicId,
	NameId,
	QuestId,
	RecipeId,
	ShopId,
	SoundId,
	TownId,

	FuncId,
	LookPointId,
	EventId,
	EntranceId,
	ObjectId,

	ForkId,
	MenuId,
	EffId,
	EffInstanceId,
	ChipId,

	CharId,

	Flag,
	Var,
	Attr,
	CharAttr,

	QuestTask,
	QuestFlags,
	SystemFlags,
	LookPointFlags,
	ObjectFlags,
	EventFlags,
	CharFlags,
	CharFlags2,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum MiscArg {
	String,
	TString,
	Text,

	Pos2,
	Pos3,
	RPos3,

	Expr,
	Fork,        // Code
	ForkLoop,    // Code
	SwitchTable, // (u16, Address)...

	QuestList,    // QuestId...
	Menu,         // TString...
	FcPartyEquip, // Slot to put item in: u8 if quartz, Const(0) otherwise (u8 post-FC)
	EffPlayPos,
}

impl<'de> Deserialize<'de> for Arg {
	fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		use serde::de;
		struct V;
		impl<'de> de::Visitor<'de> for V {
			type Value = Arg;

			fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.write_str("Arg")
			}

			fn visit_seq<A: de::SeqAccess<'de>>(self, seq: A) -> Result<Self::Value, A::Error> {
				let de = de::value::SeqAccessDeserializer::new(seq);
				Ok(Arg::Tuple(Vec::deserialize(de)?))
			}

			fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
				let de = de::value::I64Deserializer::new(v);
				Ok(Arg::Int(IntArg::deserialize(de)?, IntType::Int))
			}

			fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
				let de = de::value::U64Deserializer::new(v);
				Ok(Arg::Int(IntArg::deserialize(de)?, IntType::Int))
			}

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let de = de::value::StrDeserializer::new(v);
				if v.chars().next().is_some_and(|c| c.is_uppercase()) {
					Ok(Arg::Misc(MiscArg::deserialize(de)?))
				} else {
					Ok(Arg::Int(IntArg::deserialize(de)?, IntType::Int))
				}
			}

			fn visit_map<A: de::MapAccess<'de>>(self, map: A) -> Result<Self::Value, A::Error> {
				let de = de::value::MapAccessDeserializer::new(map);
				let Kv(int, ty) = Kv::deserialize(de)?;
				Ok(Arg::Int(int, ty))
			}
		}
		de.deserialize_any(V)
	}
}

impl<'de> Deserialize<'de> for IntArg {
	fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		use serde::de;
		struct V;
		impl<'de> de::Visitor<'de> for V {
			type Value = IntArg;

			fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.write_str("integer type or value")
			}

			fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
				let de = de::value::I64Deserializer::new(v);
				i64::deserialize(de).map(IntArg::Const)
			}

			fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
				let de = de::value::U64Deserializer::new(v);
				i64::deserialize(de).map(IntArg::Const)
			}

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let de = de::value::StrDeserializer::new(v);
				IntArg_inner::deserialize(de)
			}
		}
		de.deserialize_any(V)
	}
}

impl<'de> Deserialize<'de> for Insn {
	fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		use serde::de;
		struct V;
		impl<'de> de::Visitor<'de> for V {
			type Value = Insn;

			fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.write_str("Insn")
			}

			fn visit_enum<A: de::EnumAccess<'de>>(self, d: A) -> Result<Self::Value, A::Error> {
				#[derive(Deserialize)]
				#[serde(rename_all = "lowercase", remote = "Insn")]
				enum InsnInner {
					Match {
						#[serde(default)]
						head: Vec<Arg>,
						on: IntArg,
						cases: Vec<Insn>,
					},
				}
				let de = de::value::EnumAccessDeserializer::new(d);
				InsnInner::deserialize(de)
			}

			fn visit_unit<E: de::Error>(self) -> Result<Self::Value, E> {
				Ok(Insn::Blank)
			}

			fn visit_map<A: de::MapAccess<'de>>(self, map: A) -> Result<Self::Value, A::Error> {
				let de = de::value::MapAccessDeserializer::new(map);
				let Kv(name, args) = Kv::deserialize(de)?;
				Ok(Insn::Regular { name, args })
			}
		}
		de.deserialize_any(V)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Kv<K, V>(K, V);

impl<'de, K, V> Deserialize<'de> for Kv<K, V>
where
	K: Deserialize<'de>,
	V: Deserialize<'de>,
{
	fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		use serde::de;
		struct Vis<K, V>(std::marker::PhantomData<(K, V)>);

		impl<'de, K, V> de::Visitor<'de> for Vis<K, V>
		where
			K: Deserialize<'de>,
			V: Deserialize<'de>,
		{
			type Value = Kv<K, V>;

			fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.write_str("map containing 1 entry")
			}

			fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
				match map.next_entry()? {
					Some((k, v)) => Ok(Kv(k, v)),
					None => Err(de::Error::invalid_length(0, &"map containing 1 entry")),
				}
			}
		}
		de.deserialize_map(Vis(Default::default()))
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Builtin {
	Fc,
}

impl Builtin {
	pub fn get(self) -> &'static InsnSet {
		use std::sync::LazyLock;
		match self {
			Builtin::Fc => {
				static SET: LazyLock<InsnSet> = LazyLock::new(|| {
					serde_yaml::from_str(include_str!("../../insn/fc.yml")).unwrap()
				});
				&SET
			}
		}
	}
}

#[test]
fn test_parse() {
	Builtin::Fc.get();
}
