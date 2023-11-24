use crate::types::{Pos2, Pos3};
use gospel::read::{Le as _, Reader};
use gospel::write::{Le as _, Writer};
use snafu::prelude::*;
use strict_result::{Strict, StrictResult};

#[derive(Debug, Snafu)]
#[snafu(display("Invalid SJIS string {text:?}"))]
pub struct DecodeError {
	text: String,
}

pub fn decode(bytes: &[u8]) -> Result<String, DecodeError> {
	falcom_sjis::decode(bytes).map_err(|_| DecodeError {
		text: falcom_sjis::decode_lossy(bytes),
	})
}

#[derive(Debug, Snafu)]
#[snafu(display("Cannot encode {text:?} as SJIS"))]
pub struct EncodeError {
	text: String,
}

pub fn encode(text: &str) -> Result<Vec<u8>, EncodeError> {
	falcom_sjis::encode(text).map_err(|_| EncodeError {
		text: text.to_owned(),
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

#[derive(Debug, Snafu)]
#[snafu(display("cannot represent {value} as {type_}"))]
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
