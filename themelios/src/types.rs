#[macro_export]
macro_rules! impl_from_into {
	($outer:ident($inner:ident)) => {
		impl From<$inner> for $outer {
			fn from(v: $inner) -> $outer {
				$outer(v)
			}
		}

		impl From<$outer> for $inner {
			fn from($outer(v): $outer) -> $inner {
				v
			}
		}
	};
}

#[macro_export]
macro_rules! newtype {
	($(#[$m:meta])* $outer:ident($inner:ident)) => {
		$(#[$m])*
		#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
		#[repr(transparent)]
		pub struct $outer(pub $inner);
		$crate::impl_from_into!($outer($inner));
	};
	($(#[$m:meta])* $outer:ident($inner:ident), $fmt:literal) => {
		$(#[$m])*
		#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
		#[repr(transparent)]
		pub struct $outer(pub $inner);
		$crate::impl_from_into!($outer($inner));

		impl ::core::fmt::Debug for $outer {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				f.debug_tuple(stringify!($outer))
					.field(&format_args!($fmt, &self.0))
					.finish()
			}
		}
	};
}

newtype!(
	/// A label used in bytecode.
	///
	/// Labels with the top bit set are reserved for internal use and are used at own risk.
	Label(usize)
);

impl std::fmt::Display for Label {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "L{}", self.0)
	}
}

newtype!(BattleId(u32));
newtype!(BgmId(u16));
newtype!(ItemId(u16));
newtype!(MagicId(u16));
newtype!(NameId(u16));
newtype!(QuestId(u16));
newtype!(RecipeId(u16));
newtype!(ShopId(u8));
newtype!(SoundId(u32));
newtype!(TownId(u16));

newtype!(Flag(u16));

newtype!(FileId(u32), "0x{:08X}");

impl FileId {
	pub const NONE: FileId = FileId(0);
}

newtype!(Color(u32), "0x{:08X}");

newtype!(Angle(i16));
newtype!(Length(i32));
newtype!(Time(u32));
newtype!(Speed(u32));
newtype!(AngularSpeed(u16));
newtype!(Angle32(i32));

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos2 {
	pub x: i32,
	pub z: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos3 {
	pub x: i32,
	pub y: i32,
	pub z: i32,
}

/// Translatable string
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct TString(pub String);
impl_from_into!(TString(String));

impl std::ops::Deref for TString {
	type Target = String;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl std::ops::DerefMut for TString {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl<'a> From<&'a String> for &'a TString {
	fn from(value: &'a String) -> Self {
		// SAFETY: repr(transparent)
		unsafe { std::mem::transmute(value) }
	}
}

impl<'a> From<&'a TString> for &'a String {
	fn from(value: &'a TString) -> Self {
		&value.0
	}
}

impl From<&str> for TString {
	fn from(value: &str) -> Self {
		String::from(value).into()
	}
}

impl std::fmt::Debug for TString {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "t{:?}", &self.0)
	}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct Text(pub TString);
impl_from_into!(Text(TString));
