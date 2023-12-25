use std::backtrace::Backtrace;

use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer};
use strict_result::{Strict, StrictResult};

use crate::types::{Pos2, Pos3};

#[derive(Debug, thiserror::Error)]
#[error("Invalid SJIS string {text:?}")]
pub struct DecodeError {
	text: String,
	backtrace: Backtrace,
}

pub fn decode(bytes: &[u8]) -> Result<String, DecodeError> {
	if let Ok(s) = falcom_sjis::decode(bytes) {
		Ok(s)
	} else if let Ok(s) = std::str::from_utf8(bytes) {
		tracing::warn!("unexpected utf8 string — will not roundtrip");
		Ok(s.to_owned())
	} else {
		Err(DecodeError {
			text: falcom_sjis::decode_lossy(bytes),
			backtrace: Backtrace::capture(),
		})
	}
}

#[derive(Debug, thiserror::Error)]
#[error("Cannot encode {text:?} as SJIS")]
pub struct EncodeError {
	text: String,
	backtrace: Backtrace,
}

pub fn encode(text: &str) -> Result<Vec<u8>, EncodeError> {
	falcom_sjis::encode(text).map_err(|_| EncodeError {
		text: text.to_owned(),
		backtrace: Backtrace::capture(),
	})
}

#[extend::ext(name = ReaderExt)]
pub impl Reader<'_> {
	fn string<E>(&mut self) -> StrictResult<String, E>
	where
		E: From<gospel::read::Error> + From<DecodeError>,
	{
		let cstr = self.cstr()?;
		Ok(decode(cstr.to_bytes())?).strict()
	}

	fn sized_string<const N: usize, E>(&mut self) -> StrictResult<String, E>
	where
		E: From<gospel::read::Error> + From<DecodeError>,
	{
		let d = self.slice(N)?;
		let len = d.iter().position(|a| *a == 0).unwrap_or(d.len());
		Ok(decode(&d[..len])?).strict()
	}

	fn pos2(&mut self) -> Result<Pos2, gospel::read::Error> {
		Ok(Pos2 {
			x: self.i32()?,
			z: self.i32()?,
		})
	}

	fn pos3(&mut self) -> Result<Pos3, gospel::read::Error> {
		Ok(Pos3 {
			x: self.i32()?,
			y: self.i32()?,
			z: self.i32()?,
		})
	}

	fn vec3(&mut self) -> Result<glam::Vec3, gospel::read::Error> {
		Ok(glam::Vec3 {
			x: self.f32()?,
			y: self.f32()?,
			z: self.f32()?,
		})
	}
}

#[extend::ext(name = WriterExt)]
pub impl Writer {
	fn string<E>(&mut self, s: &str) -> StrictResult<(), E>
	where
		E: From<EncodeError>,
	{
		let s = encode(s)?;
		self.slice(&s);
		self.array([0]);
		Ok(()).strict()
	}

	fn sized_string<const N: usize, E>(&mut self, s: &str) -> StrictResult<(), E>
	where
		E: From<EncodeError> + From<ValueError>,
	{
		let s = encode(s)?;
		if s.len() > N {
			Err(ValueError::new(
				std::any::type_name::<[u8; N]>(),
				format!("{s:?}"),
			))?
		}
		let mut buf = [0; N];
		buf[..s.len()].copy_from_slice(&s);
		self.array::<N>(buf);
		Ok(()).strict()
	}

	fn pos2(&mut self, p: Pos2) {
		self.i32(p.x);
		self.i32(p.z);
	}

	fn pos3(&mut self, p: Pos3) {
		self.i32(p.x);
		self.i32(p.y);
		self.i32(p.z);
	}

	fn vec3(&mut self, p: glam::Vec3) {
		self.f32(p.x);
		self.f32(p.y);
		self.f32(p.z);
	}
}

pub fn list<V, E>(n: usize, mut f: impl FnMut() -> Result<V, E>) -> Result<Vec<V>, E> {
	let mut a = Vec::with_capacity(n);
	for _ in 0..n {
		a.push(f()?);
	}
	Ok(a)
}

#[derive(Debug, thiserror::Error)]
#[error("cannot represent {value} as {type_}")]
pub struct ValueError {
	type_: &'static str,
	value: String,
}

impl ValueError {
	pub fn new(type_: &'static str, value: impl ToString) -> Self {
		Self {
			type_,
			value: value.to_string(),
		}
	}
}

pub fn cast<A, B>(a: A) -> Result<B, ValueError>
where
	A: std::fmt::Debug + Clone,
	B: TryFrom<A>,
{
	a.clone()
		.try_into()
		.map_err(|_| ValueError::new(std::any::type_name::<B>(), format!("{:?}", a)))
}

pub macro bail {
	($fmt:literal $(, $e:expr)* $(,)?) => { bail!(format!($fmt$(, $e)*)) },
	($e:expr) => { { Err($e)?; loop {} } },
}

pub macro ensure {
	($e:expr) => { ensure!($e, stringify!($e).to_owned()) },
	($e:expr, $($t:tt)*) => { if !($e) { bail!($($t)*) } },
}

#[extend::ext]
pub impl<T> Option<T> {
	fn or_whatever(self, v: impl ToString) -> Result<T, String> {
		self.ok_or_else(|| v.to_string())
	}
}
