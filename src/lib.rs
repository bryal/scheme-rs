#![allow(dead_code)]
#![allow(unused_attributes)]
#![feature(globs)]
#![feature(unboxed_closures)]

// TODO: Consider making more use of pass-by-reference rather than cloning everywhere

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

#[deriving(Clone, PartialEq)]
pub struct Procedure {
	arg_names: Vec<String>,
	body: SEle
}

impl Procedure {
	fn n_args(&self) -> uint {
		self.arg_names.len()
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
	SProc(Box<Procedure>)
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
			SProc(ref p) => write!(f, "#procedure: arity={}", p.n_args())
		}
	}
}

pub type ScmFn = fn(&mut Env, List) -> SEle;

pub fn unit() -> SEle {
	SList(List::Nil)
}

// Either a scheme procedure, with argument names and body, or a rust function accepting an
// environment and a list of arguments
#[deriving(Clone)]
enum ProcOrFunc {
	Proc(Procedure),
	Func(ScmFn)
}

#[deriving(Clone)]
pub struct ProcDef {
	name: String,
	procedure: ProcOrFunc,
}

impl fmt::Show for ProcDef {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

impl ProcDef {
	pub fn new_proc(name: String, procedure: Procedure) -> ProcDef {
		ProcDef{name: name, procedure: ProcOrFunc::Proc(procedure)}
	}

	pub fn new_func(name: String, func: ScmFn) -> ProcDef {
		ProcDef{name: name, procedure: ProcOrFunc::Func(func)}
	}
}

impl ProcDef {
	fn name(&self) -> &str {
		self.name.as_slice()
	}
}

pub struct Env {
	pub proc_defs: Vec<ProcDef>,
	// Name/Key, Value
	// TODO: change structure to similar to ProcDef. Better encapsulation and abstraction?
	pub var_defs: Vec<(String, SEle)>,
	// When the `error_mode` is set to `Warn`, all errors are reported as warnings.
	// Useful when run in shell mode and stuff like undefined vars should not crash the session.
	pub error_mode: ScmAlertMode
}

impl Env {
	pub fn new_strict(proc_defs: Vec<ProcDef>, var_defs: Vec<(String, SEle)>) -> Env {
		Env {proc_defs: proc_defs, var_defs: var_defs, error_mode: ScmAlertMode::Error }
	}

	#[allow(dead_code)]
	pub fn new_lenient(proc_defs: Vec<ProcDef>, var_defs: Vec<(String, SEle)>) -> Env {
		Env {proc_defs: proc_defs, var_defs: var_defs, error_mode: ScmAlertMode::Warn }
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

	fn pop_proc_def(&mut self) -> ProcDef {
		self.proc_defs.pop().expect("Failed to pop proc from stack")
	}

	fn pop_proc_defs(&mut self, n: uint) {
		for _ in range(0, n) {
			self.pop_proc_def();
		}
	}

	// Get the closure, argument names, and expression body for a procedure
	fn get_proc_def(&mut self, to_get: &str) -> Option<ProcDef> {
		for proc_def in self.proc_defs.iter().rev() {
			if proc_def.name() == to_get {
				return Some(proc_def.clone());
			} else {
				continue;
			}
		}
		None
	}

	// Extract the procedure name and argument names for the `define` header `head`
	// TODO: use scheme_alert instead of panic
	fn dismantle_proc_head(&mut self, head: SEle) -> (String, Vec<String>) {
		if let SExpr(mut expr) = head {
			if let Some(SBinding(procname)) = expr.pop_head() {
				let var_names = expr.into_iter().map(|e|
						if let SBinding(var_bnd) = e {
							var_bnd
						} else {
							panic!("`{}` is not a binding name", e)
						}
					).collect();
				(procname, var_names)
			} else {
				panic!("`{}` is not a valid procedure binding", expr)
			}
		} else {
			panic!("`{}` is not a valid procedure head", head)
		}
	}

	pub fn define_proc(&mut self, head: SEle, body: SEle) {
		let (proc_name, arg_names) = self.dismantle_proc_head(head);
		let procedure = Procedure{arg_names: arg_names, body: body};
		self.proc_defs.push(ProcDef::new_proc(proc_name, procedure))
	}

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	// TODO: Fix to work with procedures as first element, like the result of a lambda
	/// Evaluate the Scheme expression, assuming `expr` is a list of the
	/// expression body starting with a procedure binding
	fn eval_expr(&mut self, mut expr: List) -> SEle {
		match expr.pop_head().unwrap() {
			SBinding(s) => {
				let ele = self.call_proc(s.as_slice(), expr);
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
		let (previous_vars, previous_fns) = (self.var_defs.len(), self.proc_defs.len());

		let result = self.eval_sequence(exprs);

		let (current_vars, current_fns) = (self.var_defs.len(), self.proc_defs.len());
		self.pop_var_defs(current_vars - previous_vars);
		self.pop_proc_defs(current_fns - previous_fns);

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

	fn call_proc(&mut self, proc_name: &str, args: List) -> SEle {
		if let Some(proc_def) = self.get_proc_def(proc_name)  {
			match proc_def.procedure {
				ProcOrFunc::Proc(procedure) => {
					let evaled_args = self.eval_multiple(args);
					let n_args = procedure.n_args();
					if n_args != evaled_args.len() {
						return scheme_alert(ScmAlert::ArityMiss(proc_name,
							n_args,"!=",evaled_args.len()),
							&self.error_mode)
					}
					// Bind and push supplied args to environment
					for (var_name, var_val) in procedure.arg_names.clone()
						.into_iter().zip(evaled_args.into_iter())
					{
						self.define_var(var_name, var_val);
					}
					let result = self.elem_value(procedure.body.clone());
					self.pop_var_defs(n_args);
					result
				},
				ProcOrFunc::Func(func) => func(self, args)
			}
		} else {
			scheme_alert(ScmAlert::Undef(proc_name), &self.error_mode)
		}
	}
}