#![allow(dead_code)]
#![feature(globs)]
#![allow(unused_attributes)]

use std::fmt;
use std::mem;

use self::SEle::*;

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
}

pub fn scheme_alert(alert: ScmAlert, a_type: &ScmAlertMode) {
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
	}
	if let &ScmAlertMode::Error = a_type {
		panic!()
	}
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
	pub fn push(&mut self, ele: SEle) {
		match self {
			&List::Cons(_, box List::Nil) => {
				let mut to_swap = List::with_head(ele);
				self.swap_link(&mut to_swap).unwrap();
			},
			&List::Cons(_, box ref mut self_link) => self_link.push(ele),
			_ => {
				let mut new_self = List::Cons(box ele, box List::Nil);
				mem::swap(self, &mut new_self);
			}
		}
	}

	/// Inserts an element at the beginning of the list.
	pub fn prepush(&mut self, ele: SEle) {
		let mut local_self = List::Nil;
		mem::swap(self, &mut local_self);
		let mut new_self = List::Cons(box ele, box local_self);
		mem::swap(self, &mut new_self);
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
	SList(List)
	// TODO: Add procedure type. This is what lambda will return
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
			&SList(_) => "List"
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
		}
	}
}

pub fn unit() -> SEle {
	SList(List::Nil)
}

// Arguments: Environment, Argument names, Argument values, Body to evaluate
pub type ScmFn<'a> = |&mut Env, Option<Vec<String>>, List, Option<SEle>|:'a -> SEle;

// TODO: Make use of unboxed closures instead of passing around `arg_names` and `body`
pub struct ProcDef<'a> {
	name: String,
	arg_names: Option<Vec<String>>,
	closure: ScmFn<'a>,
	body: Option<SEle>
}

impl<'a> fmt::Show for ProcDef<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl<'a> ProcDef<'a> {
	pub fn new_basic(name: String, cls: ScmFn<'a>) -> ProcDef<'a> {
		ProcDef{name: name, arg_names: None, closure: cls, body: None}
	}

	pub fn new_full(name: String, args: Vec<String>, cls: ScmFn<'a>, body: SEle) -> ProcDef<'a> {
		ProcDef{name: name, arg_names: Some(args), closure: cls, body: Some(body)}
	}
}

pub struct Env<'a> {
	// Name, Argument names, Closure
	pub fn_defs: Vec<ProcDef<'a>>,
	// Name, Value
	pub var_defs: Vec<(String, SEle)>,
	// When the `error_mode` is set to `Warn`, all errors are reported as warnings.
	// Useful when run in shell mode and stuff like undefined vars should not crash the session.
	pub error_mode: ScmAlertMode
}

impl<'a> Env<'a> {
	pub fn new_strict(fn_defs: Vec<ProcDef<'a>>, var_defs: Vec<(String, SEle)>) -> Env<'a> {
		Env {fn_defs: fn_defs, var_defs: var_defs, error_mode: ScmAlertMode::Error }
	}

	#[allow(dead_code)]
	pub fn new_lenient(fn_defs: Vec<ProcDef<'a>>, var_defs: Vec<(String, SEle)>) -> Env<'a> {
		Env {fn_defs: fn_defs, var_defs: var_defs, error_mode: ScmAlertMode::Warn }
	}

	pub fn define_var(&mut self, binding: String, val: SEle) {
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
		scheme_alert(ScmAlert::Undef(to_get), &self.error_mode);
		unit()
	}

	fn pop_var_def(&mut self) -> (String, SEle) {
		if let Some(var) = self.var_defs.pop() {
			var
		} else {
			panic!("Could not pop variable definition")
		}
	}

	fn pop_var_defs(&mut self, n: uint) {
		for _ in range(0, n) {
			self.pop_var_def();
		}
	}

	fn pop_proc_def(&mut self) -> ProcDef {
		if let Some(f) = self.fn_defs.pop() {
			f
		} else {
			panic!("Could not pop fn definition")
		}
	}

	fn pop_proc_defs(&mut self, n: uint) {
		for _ in range(0, n) {
			self.pop_proc_def();
		}
	}

	// Get the closure, argument names, and expression body for a procedure
	fn get_proc(&mut self, f_name: &str) -> &'a mut ProcDef {
		for fndef in self.fn_defs.iter_mut().rev() {
			if fndef.name.as_slice() == f_name {
				return fndef;
			} else {
				continue;
			}
		}
		panic!("Procedure `{}` is not defined", f_name)
	}

	// Extract the procedure name and argument names for the `define` header `head`
	fn dismantle_proc_head(&mut self, head: SEle) -> (String, Vec<String>) {
		if let SExpr(mut expr) = head {
			if let SBinding(fn_bnd) = expr.pop_head().unwrap() {
				let var_names = expr.into_iter().map(|e|
						if let SBinding(var_bnd) = e {
							var_bnd
						} else {
							panic!("`{}` is not a binding name", e)
						}
					).collect();
				(fn_bnd, var_names)
			} else {
				panic!("`{}` is not a valid procedure binding", expr)
			}
		} else {
			panic!("`{}` is not a valid procedure head", head)
		}
	}

	pub fn define_proc(&mut self, head: SEle, body: SEle) {
		let (fn_binding, arg_names) = self.dismantle_proc_head(head);

		let defined_f: ScmFn<'a> = |env, arg_names_opt, ops, body| {
			let evaled_args = env.eval_args(ops);
			let arg_names = arg_names_opt.unwrap();
			let n_args = arg_names.len();
			if n_args != evaled_args.len() {
				scheme_alert(ScmAlert::ArityMiss("_", n_args,"!=",evaled_args.len()),
					&env.error_mode);
				return unit();
			}
			// Bind and push supplied args to environment
			for (k, v) in arg_names.clone().into_iter().zip(evaled_args.into_iter()) {
				env.define_var(k, v);
			}
			let result = env.elem_value(body.unwrap());
			env.pop_var_defs(n_args);
			result
		};

		self.fn_defs.push(ProcDef::new_full(fn_binding, arg_names, defined_f, body))
	}

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	// TODO: Fix to work with procedures as first element, like the result of a lambda
	/// Evaluate the Scheme expression, assuming `expr` is a list of the
	/// expression body starting with a procedure binding
	fn eval_expr(&mut self, mut expr: List) -> SEle {
		match expr.pop_head().unwrap() {
			SBinding(s) => {
				let ele = call_proc(self, s.as_slice(), expr);
				if let SExpr(inner_expr) = ele {
					self.eval_expr(inner_expr)
				} else {
					ele
				}
			},
			x => panic!("`{}` is not a procedure", x)
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

	pub fn eval_multiple(&mut self, exprs: List) -> SEle {	
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
		let (previous_vars, previous_fns) = (self.var_defs.len(), self.fn_defs.len());

		let result = self.eval_multiple(exprs);

		let (current_vars, current_fns) = (self.var_defs.len(), self.fn_defs.len());
		self.pop_var_defs(current_vars - previous_vars);
		self.pop_proc_defs(current_fns - previous_fns);

		result
	}

	fn eval_args(&mut self, exprs: List) -> List {
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
}

fn call_proc(env: *mut Env, f_name: &str, args: List) -> SEle {
	let env_brw: &mut Env = unsafe{mem::transmute(&mut *env)};
	let fndef = env_brw.get_proc(f_name);
	(fndef.closure)(unsafe{mem::transmute(&mut *env)}, fndef.arg_names.clone(), args,
		fndef.body.clone())
}