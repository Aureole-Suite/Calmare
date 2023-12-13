use std::borrow::Cow;
use std::collections::BTreeMap;

use serde::Deserialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Game {
	Fc,
	Sc,
	#[serde(rename = "3rd")]
	Tc,
	Zero,
	Azure,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Variant {
	Base,
	Evo,
	Kai,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct InsnSet<'a> {
	pub variant: Variant,
	#[serde(flatten)]
	inner: Cow<'a, InsnSetInner>,
}

impl<'a> std::ops::Deref for InsnSet<'a> {
	type Target = InsnSetInner;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

pub fn get(game: Game, variant: Variant) -> InsnSet<'static> {
	let builtin = match (game, variant) {
		(Game::Fc, _) => Builtin::Fc,
		(Game::Sc, _) => Builtin::Sc,
		(Game::Tc, _) => Builtin::Tc,
		(Game::Zero, Variant::Evo) => Builtin::ZeroEvo,
		(Game::Zero, _) => Builtin::Zero,
		(Game::Azure, Variant::Evo) => Builtin::AzureEvo,
		(Game::Azure, _) => Builtin::Azure,
	};
	InsnSet {
		variant,
		inner: Cow::Borrowed(builtin.get()),
	}
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InsnSetInner {
	pub game: Game,
	pub address_size: IntType,
	pub insns: [Insn; 256],
	pub at_roll: [String; 16],
	pub insns_rev: BTreeMap<String, Vec<Arg>>,
}

#[allow(non_camel_case_types)]
#[serde_with::serde_as]
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(remote = "InsnSetInner")]
struct InsnSet_inner {
	pub game: Game,
	pub address_size: IntType,
	#[serde_as(as = "[_; 256]")]
	pub insns: [Insn; 256],
	pub at_roll: [String; 16],
	#[serde(skip)]
	pub insns_rev: BTreeMap<String, Vec<Arg>>,
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
		on: IntType,
		cases: Vec<Insn>,
	},
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
	Int(IntType, IntArg),
	Misc(MiscArg),
	Tuple(Vec<Arg>),
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntType {
	u8,
	u16,
	u24,
	u32,
	i8,
	i16,
	i32,
	Const(i64),
	// Ugly hack for ED7Battle and ED7NpcBattle instruction.
	// Encodes 1 as `FF FF FF FF` and 0 as empty.
	ED7Battle,
}

// Ugly implementation detail
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(remote = "IntType")]
enum IntType_inner {
	u8,
	u16,
	u24,
	u32,
	i8,
	i16,
	i32,
	#[serde(rename = "ed7battle")]
	ED7Battle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum IntArg {
	#[serde(skip)]
	Int,
	#[serde(skip)]
	Const(i64),

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
	VisId,

	CharId,

	Flag,
	Var,
	Global,
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

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub enum MiscArg {
	Label,

	String,
	TString,
	Text,

	Pos2,
	Pos3,
	RPos3,

	Expr,
	Fork,                           // Code
	ForkLoop(String),               // Code
	SwitchTable(IntType, Box<Arg>), // count, case

	QuestList, // QuestId...
	Menu,      // TString...
	PartySelectMandatory,
	PartySelectOptional,
	TcMembers,
	ED7CharAnimation,

	EvoSave,
	KaiSoundId,
	ED7BattlePos,
	FcPartyEquip,
	ScPartySetSlot,
	EffPlayPos,
}

impl<'de> Deserialize<'de> for InsnSetInner {
	fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		let mut iset = InsnSet_inner::deserialize(de)?;
		make_rev_table(IntType::u8, &iset.insns, &mut iset.insns_rev, vec![])?;
		Ok(iset)
	}
}

fn make_rev_table<D: serde::de::Error>(
	ty: IntType,
	insns: &[Insn],
	insns_rev: &mut BTreeMap<String, Vec<Arg>>,
	prev_args: Vec<Arg>,
) -> Result<(), D> {
	for (i, insn) in insns.iter().enumerate() {
		let mut my_args = prev_args.clone();
		my_args.push(Arg::Int(ty, IntArg::Const(i as i64)));
		match insn {
			Insn::Blank => {}
			Insn::Regular { name, args } => {
				my_args.extend(args.iter().cloned());
				if insns_rev.insert(name.clone(), my_args).is_some() {
					return Err(D::custom(format!("duplicate instruction {name}")));
				}
			}
			Insn::Match { head, on, cases } => {
				my_args.extend(head.iter().cloned());
				make_rev_table(*on, cases, insns_rev, my_args)?;
			}
		}
	}
	Ok(())
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
				Ok(Arg::Int(IntType::deserialize(de)?, IntArg::Int))
			}

			fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
				let de = de::value::U64Deserializer::new(v);
				Ok(Arg::Int(IntType::deserialize(de)?, IntArg::Int))
			}

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let de = de::value::StrDeserializer::new(v);
				if v.chars().next().is_some_and(|c| c.is_uppercase()) {
					Ok(Arg::Misc(MiscArg::deserialize(de)?))
				} else {
					Ok(Arg::Int(IntType::deserialize(de)?, IntArg::Int))
				}
			}

			fn visit_enum<A: de::EnumAccess<'de>>(self, data: A) -> Result<Self::Value, A::Error> {
				let de = de::value::EnumAccessDeserializer::new(data);
				Ok(Arg::Misc(MiscArg::deserialize(de)?))
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

impl<'de> Deserialize<'de> for IntType {
	fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		use serde::de;
		struct V;
		impl<'de> de::Visitor<'de> for V {
			type Value = IntType;

			fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.write_str("integer type or value")
			}

			fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
				let de = de::value::I64Deserializer::new(v);
				i64::deserialize(de).map(IntType::Const)
			}

			fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
				let de = de::value::U64Deserializer::new(v);
				i64::deserialize(de).map(IntType::Const)
			}

			fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
				let de = de::value::StrDeserializer::new(v);
				IntType_inner::deserialize(de)
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
						on: IntType,
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
	Sc,
	Tc,
	Zero,
	Azure,
	ZeroEvo,
	AzureEvo,
}

impl Builtin {
	pub fn get(self) -> &'static InsnSetInner {
		use std::sync::LazyLock;
		macro_rules! builtin {
			($($variant:ident => $file:literal),* $(,)?) => {
				match self {
					$(Builtin::$variant => {
						static SET: LazyLock<InsnSetInner> = LazyLock::new(|| {
							serde_yaml::from_str(include_str!(concat!("../../insn/", $file))).unwrap()
						});
						&SET
					})*
				}
			}
		}
		builtin! {
			Fc => "fc.yml",
			Sc => "sc.yml",
			Tc => "3rd.yml",
			Zero => "zero.yml",
			Azure => "azure.yml",
			ZeroEvo => "zero_evo.yml",
			AzureEvo => "azure_evo.yml",
		}
	}
}

#[test]
fn test_parse() {
	Builtin::Fc.get();
	Builtin::Sc.get();
	Builtin::Tc.get();
	Builtin::Zero.get();
	Builtin::Azure.get();
	Builtin::ZeroEvo.get();
	Builtin::AzureEvo.get();
}
