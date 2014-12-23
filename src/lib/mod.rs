#![allow(dead_code)]
#![allow(unused_attributes)]
#![feature(globs)]
#![feature(unboxed_closures)]

// TODO: Consider making more use of pass-by-reference rather than cloning everywhere

use std::fmt;
use std::mem;

use self::SEle::*;

pub mod scheme_stdlib;

pub enum ScmAlertMode {
	Warn,
	Error
}

impl fmt::Show for ScmAlertMode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			ScmAlertMode::Warn => write!(f, "Warning"),
			ScmAlertMode::Error => write!(f, "Error")
		}
	}
}

impl Copy for ScmAlertMode {}

pub enum ScmAlert<'a> {
	Unexp(&'a str),
	WrongType(&'a str, &'a str),
	ArityMiss(&'a str, uint, &'a str, uint),
	Bad(&'a str),
	Undef(&'a str),
	NaN(SEle),
	Unclosed,
	Custom(String)
}

pub fn scheme_alert(alert: ScmAlert, a_type: &ScmAlertMode) -> SEle {
	match alert {
		ScmAlert::Unexp(s) => println!("; {}: Unexpected `{}`", a_type, s),
		ScmAlert::WrongType(exp, got) => println!("; {}: Wrong type. Expected `{}`, found `{}`",
				a_type, exp, got),
		ScmAlert::Undef(s) => println!("; {}: Undefined variable `{}`", a_type, s),
		ScmAlert::Bad(s) => println!("; {}: Bad `{}`", a_type, s),
		ScmAlert::NaN(e) => println!("; {}: `{}: {}` is not a number", a_type, e.variant(), e),
		ScmAlert::Unclosed => println!("; {}: Unclosed delimiter", a_type),
		ScmAlert::ArityMiss(s, got, vs, exp) => println!("; {}: Arity missmatch in `{}`, {} {} {}",
				a_type, s, got, vs, exp),
		ScmAlert::Custom(s) => println!("; {}: {}", a_type, s),
	}
	if let &ScmAlertMode::Error = a_type {
		panic!()
	}
	unit()
}

pub struct MoveListItems {
	list: List
}

impl Iterator<SEle> for MoveListItems {
	fn next(&mut self) -> Option<SEle> {
		self.list.pop_head()
	}
}

impl MoveListItems {
	pub fn peek(&self) -> Option<&SEle> {
		self.list.head()
	}
}

#[deriving(Clone, PartialEq)]
pub enum List {
	Cons(Box<SEle>, Box<List>),
	Nil
}

impl fmt::Show for List {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			List::Cons(ref ele, ref link) => write!(f, "{} {}", ele, link),
			List::Nil => write!(f, "")
		}
	}
}

impl List {
	pub fn new() -> List {
		List::Nil
	}

	pub fn with_body(head: SEle, tail: List) -> List {
		List::Cons(box head, box tail)
	}

	pub fn with_head(head: SEle) -> List {
		List::Cons(box head, box List::Nil)
	}

	pub fn from_vec(vals: Vec<SEle>) -> List {
		let mut list = List::new();
		for val in vals.into_iter() {
			list.push(val);
		}
		list
	}

	pub fn len(&self) -> uint {
		if self == &List::Nil {
			0
		} else {
			let mut len = 0;
			let mut current = self;
			while let Some(l) = current.tail() {
				current = l;
				len += 1;
			}
			len
		}
	}

	pub fn into_iter(self) -> MoveListItems {
		MoveListItems{list: self}
	}

	pub fn swap_val(&mut self, val: &mut SEle) -> Result<(), ()> {
		if let &List::Cons(box ref mut self_val, _) = self {
			mem::swap(val, self_val);
			Ok(())
		} else {
			Err(())
		}
	}

	pub fn swap_link(&mut self, link: &mut List) -> Result<(), ()> {
		if let &List::Cons(_, box ref mut self_link) = self {
			mem::swap(link, self_link);
			Ok(())
		} else {
			Err(())
		}
	}

	/// Returns a reference the first value in the list.
	pub fn head(&self) -> Option<&SEle> {
		if let &List::Cons(box ref val, _) = self {
			Some(val)
		} else {
			None
		}
	}

	pub fn head_mut(&mut self) -> Option<&mut SEle> {
		if let &List::Cons(box ref mut val, _) = self {
			Some(val)
		} else {
			None
		}
	}

	/// Returns a reference a list containing all but the first value.
	pub fn tail(&self) -> Option<&List> {
		if let &List::Cons(_, box ref list) = self {
			Some(list)
		} else {
			None
		}
	}

	pub fn tail_mut(&mut self) -> Option<&mut List> {
		if let &List::Cons(_, box ref mut list) = self {
			Some(list)
		} else {
			None
		}
	}

	/// Pops the first value from the list. `self` is now its old tail.
	pub fn pop_head(&mut self) -> Option<SEle> {
		if let Some(mut cdr) = self.pop_tail() {
			mem::swap(&mut cdr, self);
			if let List::Cons(box val, _) = cdr {
				Some(val)
			} else {
				None
			}
		} else {
			None
		}
	}

	/// Pops the tail from the list. `self` is now only the first entry.
	pub fn pop_tail(&mut self) -> Option<List> {
		let mut cdr = List::Nil;
		if let &List::Cons(_, _) = self {
			if let Err(_) = self.swap_link(&mut cdr) {
				return None;
			}
			Some(cdr)
		} else {
			None
		}
	}

	/// Pushes an element to the end of the list.
	pub fn push(&mut self, ele: SEle) -> &mut List {
		match self {
			&List::Cons(_, box List::Nil) => {
				let mut to_swap = List::with_head(ele);
				self.swap_link(&mut to_swap).unwrap();
			},
			&List::Cons(_, box ref mut self_link) => {self_link.push(ele);},
			_ => {
				let mut new_self = List::Cons(box ele, box List::Nil);
				mem::swap(self, &mut new_self);
			}
		}
		self
	}

	/// Inserts an element at the beginning of the list.
	pub fn prepush(&mut self, ele: SEle) -> &mut List {
		let mut local_self = List::Nil;
		mem::swap(self, &mut local_self);
		let mut new_self = List::Cons(box ele, box local_self);
		mem::swap(self, &mut new_self);
		self
	}
}

impl FromIterator<SEle> for List {
	fn from_iter<T: Iterator<SEle>>(mut iterator: T) -> List {
		let mut list = List::new();
		for element in iterator {
			list.push(element);
		}
		list
	}
}

impl Index<uint, SEle> for List {
	fn index(&self, index: &uint) -> &SEle {
		let mut current = self;
		for _ in range(0, *index) {
			if let Some(tail) = current.tail() {
				current = tail;
			} else {
				panic!("List index out of range");
			}
		}
		if let Some(val) = current.head() {
			val
		} else {
			panic!("List index out of range")
		}
	}
}

/// An element in an S-Expression list
#[deriving(Clone, PartialEq)]
#[allow(dead_code)]
pub enum SEle {
	SExpr(List),
	SBinding(String),
	SNum(f64),
	SStr(String),
	SSymbol(String),
	SBool(bool),
	SList(List),
	SProc(Box<ProcOrFunc>)
}

impl SEle {
	pub fn variant(&self) -> &'static str {
		match self {
			&SExpr(_) => "Expression",
			&SBinding(_) => "Binding",
			&SNum(_) => "Number",
			&SStr(_) => "String",
			&SSymbol(_) => "Symbol",
			&SBool(_) => "Bool",
			&SList(_) => "List",
			&SProc(_) => "Procedure"
		}
	}
}

impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref xs) | SList(ref xs) => write!(f, "({})", xs),
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "{}", s),
			SSymbol(ref s) => write!(f, "'{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
			SProc(box ProcOrFunc::Proc(ref p)) => write!(f, "#procedure: arity={}",
				p.n_args()),
			SProc(box ProcOrFunc::Func(_)) => write!(f, "#procedure")
		}
	}
}

pub type ScmFn = fn(&mut Env, List) -> SEle;

pub fn unit() -> SEle {
	SList(List::Nil)
}

#[deriving(Clone, PartialEq)]
pub struct Procedure {
	arg_names: Vec<String>,
	body: SEle,
	variadic: bool
}

impl Procedure {
	pub fn new(arg_names: Vec<String>, body: SEle) -> Procedure {
		Procedure{arg_names: arg_names, body: body, variadic: false}
	}

	pub fn new_variadic(arg_name: String, body: SEle) -> Procedure {
		Procedure{arg_names: vec![arg_name], body: body, variadic: true}
	}

	pub fn n_args(&self) -> uint {
		self.arg_names.len()
	}
}

// Either a scheme procedure, with argument names and body, or a rust function accepting an
// environment and a list of arguments
#[deriving(Clone)]
pub enum ProcOrFunc {
	Proc(Procedure),
	Func(ScmFn)
}

impl PartialEq for ProcOrFunc {
	fn eq(&self, other: &ProcOrFunc) -> bool {
		match (self, other) {
			(&ProcOrFunc::Proc(ref p1), &ProcOrFunc::Proc(ref p2)) => p1 == p2,
			(_, _) => false
		}
	}
}

pub struct Env {
	pub var_defs: Vec<(String, SEle)>,
	// When the `error_mode` is set to `Warn`, all errors are reported as warnings.
	// Useful when run in shell mode and stuff like undefined vars should not crash the session.
	pub error_mode: ScmAlertMode
}

impl Env {
	pub fn new_strict(var_defs: Vec<(String, SEle)>) -> Env {
		Env {var_defs: var_defs, error_mode: ScmAlertMode::Error }
	}

	pub fn new_lenient(var_defs: Vec<(String, SEle)>) -> Env {
		Env {var_defs: var_defs, error_mode: ScmAlertMode::Warn }
	}

	pub fn define_var(&mut self, binding: String, val: SEle) {
		// TODO: Maybe adopt a more lazy evaluation stratergy?
		let val = self.elem_value(val);
		self.var_defs.push((binding, val));
	}

	fn get_var(&mut self, to_get: &str) -> SEle {
		for vardef in self.var_defs.iter().rev() {
			let &(ref binding, ref var) = vardef;
			if binding.as_slice() == to_get {
				return var.clone();
			} else {
				continue;
			}
		}
		scheme_alert(ScmAlert::Undef(to_get), &self.error_mode)
	}

	fn pop_var_def(&mut self) -> (String, SEle) {
		self.var_defs.pop().expect("Failed to pop var from stack")
	}

	fn pop_var_defs(&mut self, n: uint) {
		for _ in range(0, n) {
			self.pop_var_def();
		}
	}

	// Extract the procedure name and argument names for the `define` header `head`
	// TODO: use scheme_alert instead of panic
	pub fn dismantle_proc_head(&mut self, mut head: List) -> (String, Vec<String>) {
		if let Some(SBinding(proc_name)) = head.pop_head() {
			let var_names = head.into_iter().map(|e|
					if let SBinding(var_bnd) = e {
						var_bnd
					} else {
						panic!("`{}` is not a binding", e)
					}
				).collect();
			(proc_name, var_names)
		} else {
			panic!("`{}` is not a valid procedure binding", head)
		}
	}

	pub fn define_proc(&mut self, proc_name: String, procedure: ProcOrFunc) {
		scheme_stdlib::scm_define(self, List::with_body(SBinding(proc_name),
				List::with_head(SProc(box procedure))));
	}

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	// TODO: Fix to work with procedures as first element, like the result of a lambda
	/// Evaluate the Scheme expression, assuming `expr` is a list of the
	/// expression body starting with a procedure binding
	fn eval_expr(&mut self, mut expr: List) -> SEle {
		match self.elem_value(expr.pop_head().unwrap()) {
			SProc(box ProcOrFunc::Proc(procedure)) => self.run_proc(procedure, expr),
			SProc(box ProcOrFunc::Func(func)) => func(self, expr),
			x => scheme_alert(ScmAlert::WrongType("Procedure", x.variant()),
				&self.error_mode)
		}
	}

	// Get the value of the element. If it's an expression, get the evaluation. If it's a
	// binding, get the associated value. If it's a value, return it.
	pub fn elem_value(&mut self, ele: SEle) -> SEle {
		match ele {
			SExpr(x) => self.eval_expr(x),
			SBinding(x) => self.get_var(x.as_slice()),
			_ => ele
		}
	}

	pub fn eval_sequence(&mut self, exprs: List) -> SEle {	
		if exprs != List::Nil {
			let mut it = exprs.into_iter();
			while let Some(expr) = it.next() {
				if it.peek() == None {
					return self.elem_value(expr);
				}
				self.elem_value(expr);
			}
		}
		unit()
	}

	// This is run when there are multiple statements in a single expression body, like in a
	// `begin`. Only in these situations can variables and procedures be `define`d, wherefore
	// this method keeps track of variables and procedures in scope.
	pub fn eval_block(&mut self, exprs: List) -> SEle {
		let previous_n_vars = self.var_defs.len();
		let result = self.eval_sequence(exprs);
		let current_n_vars = self.var_defs.len();
		self.pop_var_defs(current_n_vars - previous_n_vars);
		result
	}

	fn eval_multiple(&mut self, exprs: List) -> List {
		exprs.into_iter().map(|e| self.elem_value(e)).collect()
	}

	pub fn elem_is_true(&mut self, ele: SEle) -> bool {
		match ele {
			SBool(b) => b,
			SExpr(_) => {
				let val = self.elem_value(ele);
				self.elem_is_true(val)
			},
			_ => false
		}
	}

	fn run_proc(&mut self, mut procedure: Procedure, args: List) -> SEle {
		let evaled_args = self.eval_multiple(args);
		let n_args = procedure.n_args();
		if procedure.variadic {
			self.define_var(procedure.arg_names.pop().unwrap(), SList(evaled_args));
		} else {
			if n_args != evaled_args.len() {
				return scheme_alert(ScmAlert::ArityMiss("_",
					n_args,"!=",evaled_args.len()),
					&self.error_mode)
			}
			// Bind and push supplied args to environment
			for (var_name, var_val) in procedure.arg_names.clone()
				.into_iter().zip(evaled_args.into_iter())
			{
				self.define_var(var_name, var_val);
			}
		}
		let result = self.elem_value(procedure.body.clone());
		self.pop_var_defs(n_args);
		result
	}
}