#[derive(Debug, Clone)]
pub struct Printer {
	indent: usize,
	out: String,
	newline: bool,
	space: bool,
}

impl Default for Printer {
	fn default() -> Self {
		Self::new()
	}
}

impl Printer {
	pub fn new() -> Self {
		Printer {
			indent: 0,
			out: String::new(),
			newline: true,
			space: false,
		}
	}

	pub fn finish(self) -> String {
		self.out
	}

	fn put_space(&mut self) {
		let newline = std::mem::take(&mut self.newline);
		let space = std::mem::take(&mut self.space);
		if newline {
			for _ in 0..self.indent {
				self.out.push('\t');
			}
		} else if space {
			self.out.push(' ');
		}
	}

	pub fn space(&mut self) -> &mut Self {
		self.space = true;
		self
	}

	pub fn no_space(&mut self) -> &mut Self {
		self.space = false;
		self
	}

	pub fn line(&mut self) -> &mut Self {
		self.out.push('\n');
		self.newline = true;
		self.space = false;
		self
	}

	pub fn is_line(&self) -> bool {
		self.newline
	}

	pub fn is_space(&self) -> bool {
		self.space && !self.newline
	}

	pub fn indent<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
		self.indent += 1;
		let v = f(self);
		self.indent -= 1;
		v
	}

	pub fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> &mut Self {
		self.put_space();
		std::fmt::Write::write_fmt(&mut self.out, args).unwrap();
		self
	}

	pub fn word(&mut self, arg: &str) -> &mut Self {
		if !arg.is_empty() {
			self.put_space();
			self.out.push_str(arg);
			self.space();
		}
		self
	}

	pub fn block<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
		assert!(!self.newline);
		let v = self.no_space().word(":").line().indent(f);
		assert!(self.newline);
		v
	}

	pub fn term(&mut self, name: &str) -> TermPrinter<'_> {
		self.word(name);
		TermPrinter {
			printer: self,
			named: !name.is_empty(),
			count: 0,
		}
	}
}

pub struct TermPrinter<'a> {
	printer: &'a mut Printer,
	named: bool,
	count: usize,
}

impl<'a> TermPrinter<'a> {
	#[must_use]
	pub fn field(&mut self) -> &mut Printer {
		if self.count == 0 {
			if self.named {
				self.printer.no_space();
			}
			self.printer.word(if self.named { "[" } else { "(" });
			self.printer.no_space();
		} else {
			self.printer.no_space();
			self.printer.word(",");
			if self.named {
				self.printer.no_space();
			}
		}
		self.count += 1;
		self.printer
	}
}

impl<'a> Drop for TermPrinter<'a> {
	fn drop(&mut self) {
		match (self.count, self.named) {
			(0, false) => self.printer.word("()"),
			(1, false) => self.printer.no_space().word(",)"),
			(_, false) => self.printer.no_space().word(")"),
			(0, true) => self.printer,
			(_, true) => self.printer.no_space().word("]"),
		};
	}
}