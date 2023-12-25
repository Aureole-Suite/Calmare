#![feature(lazy_cell)]

use std::borrow::Cow;
use std::collections::BTreeMap;

use serde::{de, Deserialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Game {
	Fc,
	Sc,
	#[serde(rename = "3rd")]
	Tc,
	Zero,
	Azure,
	Cs1,
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
		(Game::Cs1, _) => Builtin::Cs1,
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
	pub insns: Vec<Insn>,
	pub at_roll: [String; 16],
	pub insns_rev: BTreeMap<String, Vec<Arg>>,
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(remote = "InsnSetInner")]
struct InsnSet_inner {
	pub game: Game,
	pub address_size: IntType,
	#[serde(deserialize_with = "insn_list")]
	pub insns: Vec<Insn>,
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
	// Ugly hack for ED7Battle and ED7BattleAuto instruction.
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

	Const(IntType, i64),

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

	#[allow(non_camel_case_types)]
	f32,
	Cs1_13(Box<Arg>),
	Cs1_22,
	Cs1_28_34,
	Cs1_36(Vec<u16>, Box<Arg>),
	Cs1_3C(Box<Arg>),
}

impl<'de> Deserialize<'de> for InsnSetInner {
	fn deserialize<D: de::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		let mut iset = InsnSet_inner::deserialize(de)?;
		if iset.insns.len() != 256 {
			return Err(de::Error::invalid_length(iset.insns.len(), &"256 insns"));
		}
		make_rev_table(IntType::u8, &iset.insns, &mut iset.insns_rev, vec![])?;
		Ok(iset)
	}
}

fn make_rev_table<D: de::Error>(
	ty: IntType,
	insns: &[Insn],
	insns_rev: &mut BTreeMap<String, Vec<Arg>>,
	prev_args: Vec<Arg>,
) -> Result<(), D> {
	for (i, insn) in insns.iter().enumerate() {
		let mut my_args = prev_args.clone();
		my_args.push(Arg::Misc(MiscArg::Const(ty, i as i64)));
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
	fn deserialize<D: de::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
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
				if v.chars().next().is_some_and(|c| c.is_uppercase()) || v == "f32" {
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
	fn deserialize<D: de::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
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

struct Insns<'a>(&'a mut Vec<Insn>);

impl<'de> de::DeserializeSeed<'de> for Insns<'_> {
	type Value = ();
	fn deserialize<D: de::Deserializer<'de>>(self, de: D) -> Result<(), D::Error> {
		de.deserialize_any(self)
	}
}

impl<'de> de::Visitor<'de> for Insns<'_> {
	type Value = ();

	fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.write_str("Insn")
	}

	fn visit_enum<A: de::EnumAccess<'de>>(self, d: A) -> Result<Self::Value, A::Error> {
		#[derive(Deserialize)]
		#[serde(rename_all = "lowercase")]
		enum InsnInner {
			Match(Match),
			Skip(usize),
		}

		let de = de::value::EnumAccessDeserializer::new(d);
		match InsnInner::deserialize(de)? {
			InsnInner::Match(m) => self.0.push(Insn::Match {
				head: m.head,
				on: m.on,
				cases: m.cases,
			}),
			InsnInner::Skip(n) => {
				for _ in 0..n {
					self.0.push(Insn::Blank)
				}
			}
		}
		Ok(())
	}

	fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
		if let Some(name) = seq.next_element()? {
			let de = de::value::SeqAccessDeserializer::new(seq);
			let args = Vec::deserialize(de)?;
			self.0.push(Insn::Regular { name, args });
		} else {
			self.0.push(Insn::Blank);
		}
		Ok(())
	}
}

fn insn_list<'de, D: de::Deserializer<'de>>(de: D) -> Result<Vec<Insn>, D::Error> {
	struct V;

	impl<'de> de::Visitor<'de> for V {
		type Value = Vec<Insn>;

		fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
			formatter.write_str("a sequence")
		}

		fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
			let mut values = Vec::new();
			while let Some(()) = seq.next_element_seed(Insns(&mut values))? {}
			Ok(values)
		}
	}

	de.deserialize_seq(V)
}

fn u8() -> IntType {
	IntType::u8
}

#[derive(Deserialize)]
#[serde(rename_all = "lowercase", remote = "Self")]
struct Match {
	#[serde(default)]
	head: Vec<Arg>,
	#[serde(default = "u8")]
	on: IntType,
	#[serde(deserialize_with = "insn_list")]
	cases: Vec<Insn>,
}

impl<'de> Deserialize<'de> for Match {
	fn deserialize<D: de::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
		struct V;
		impl<'de> de::Visitor<'de> for V {
			type Value = Match;

			fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
				f.write_str("mapping or sequence")
			}

			fn visit_map<A: de::MapAccess<'de>>(self, map: A) -> Result<Self::Value, A::Error> {
				let de = de::value::MapAccessDeserializer::new(map);
				Match::deserialize(de)
			}

			fn visit_seq<A: de::SeqAccess<'de>>(self, seq: A) -> Result<Self::Value, A::Error> {
				let de = de::value::SeqAccessDeserializer::new(seq);
				Ok(Match {
					head: vec![],
					on: IntType::u8,
					cases: insn_list(de)?,
				})
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
	fn deserialize<D: de::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Builtin {
	Fc,
	Sc,
	Tc,
	Zero,
	Azure,
	ZeroEvo,
	AzureEvo,
	Cs1,
}

impl Builtin {
	pub fn get(self) -> &'static InsnSetInner {
		use std::sync::LazyLock;
		macro_rules! builtin {
			($($variant:ident => $file:literal),* $(,)?) => {
				match self {
					$(Builtin::$variant => {
						static SET: LazyLock<InsnSetInner> = LazyLock::new(|| {
							serde_yaml::from_str(include_str!(concat!("../data/", $file))).unwrap()
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
			Cs1 => "cs1.yml",
		}
	}
}

#[cfg(test)]
#[rustfmt::skip]
mod test {
	use super::Builtin as B;
	#[test] fn fc() { B::Fc.get(); }
	#[test] fn sc() { B::Sc.get(); }
	#[test] fn tc() { B::Tc.get(); }
	#[test] fn zero() { B::Zero.get(); }
	#[test] fn azure() { B::Azure.get(); }
	#[test] fn zero_evo() { B::ZeroEvo.get(); }
	#[test] fn azure_evo() { B::AzureEvo.get(); }
	#[test] fn cs1() { B::Cs1.get(); }
}
