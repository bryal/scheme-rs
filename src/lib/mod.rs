#![allow(dead_code)]
#![allow(unused_attributes)]
#![feature(globs)]
#![feature(unboxed_closures)]

// TODO: Consider making more use of pass-by-reference rather than cloning everywhere
// TODO: Document wether functions return bindings, atoms, expressions or whatever.
// TODO: display line number on error

use std::fmt;
use std::mem;
use std::cmp;
use std::ops::Index;
use std::iter::FromIterator;

use self::SEle::*;

pub mod scheme_stdlib;
pub mod scm_macro;

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
		ScmAlert::Bad(s) => println!("; {}: Bad {}", a_type, s),
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

pub struct ListItems<'a> {
	list: &'a List
}

impl<'a> Iterator for ListItems<'a> {
	type Item = &'a SEle;
	fn next(&mut self) -> Option<&'a SEle> {
		let ret = self.list.head();
		if let Some(tail) = self.list.tail() {
			self.list = tail;
		}
		ret
	}
}

impl<'a> ListItems<'a> {
	pub fn peek(&self) -> Option<&SEle> {
		self.list.head()
	}
}

pub struct MoveListItems {
	list: List
}

impl Iterator for MoveListItems {
	type Item = SEle;
	fn next(&mut self) -> Option<SEle> {
		self.list.pop_head()
	}
}

impl MoveListItems {
	pub fn peek(&self) -> Option<&SEle> {
		self.list.head()
	}
}

#[derive(Clone, PartialEq)]
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

	pub fn iter(&self) -> ListItems {
		ListItems{list: self}
	}

	pub fn into_iter(self) -> MoveListItems {
		MoveListItems{list: self}
	}

	pub fn contains(&self, other: &SEle) -> bool {
		self.iter().any(|e| e == other)
	}

	pub fn swap_head(&mut self, val: &mut SEle) -> Result<(), ()> {
		if let &List::Cons(box ref mut self_val, _) = self {
			mem::swap(val, self_val);
			Ok(())
		} else {
			Err(())
		}
	}

	pub fn swap_tail(&mut self, link: &mut List) -> Result<(), ()> {
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

	pub fn last_link(&mut self) -> &mut List {
		if let &List::Cons(_, box ref mut list) = self {
			list.last_link()
		} else {
			self
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
			if let Err(_) = self.swap_tail(&mut cdr) {
				return None;
			}
			Some(cdr)
		} else {
			None
		}
	}

	/// Pushes an element to the end of the list. Returns link to last Cons in list.
	pub fn push(&mut self, ele: SEle) -> &mut List {
		match self {
			&List::Cons(_, box List::Nil) => {
				let mut to_swap = List::with_head(ele);
				self.swap_tail(&mut to_swap).unwrap();
				self
			},
			&List::Cons(_, box ref mut self_link) => self_link.push(ele),
			_ => {
				let mut new_self = List::Cons(box ele, box List::Nil);
				mem::swap(self, &mut new_self);
				self
			}
		}
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
	fn from_iter<T: Iterator<Item=SEle>>(mut iterator: T) -> List {
		let mut list = List::new();
		for element in iterator {
			list.push(element);
		}
		list
	}
}

impl Index<uint> for List {
	type Output = SEle;
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

type VarStack = Vec<(String, SEle)>;

/// Returns empty list (Nil) if var is not on stack.
fn get_var(stack: &mut VarStack, to_get: &str) -> Option<SEle> {
	for &(ref binding, ref var) in stack.iter().rev() {
		if *binding == to_get {
			return Some(var.clone());
		} else {
			continue;
		}
	}
	None
}

fn pop_var_defs(stack: &mut VarStack, n: uint) -> VarStack {
	range(0, cmp::min(n, stack.len())).map(|_| stack.pop().unwrap()).collect::<Vec<_>>()
		.into_iter().rev().collect()
}

#[derive(Clone, PartialEq)]
pub struct Lambda {
	binding: Option<String>,
	arg_names: Vec<String>,
	// Body is an s-expression as a `List`.
	body: List,
	variadic: bool,
	// Lambdas will capture variables by reference when in scope, but if the lambda is passed to
	// a lower scope, or in case of Tail Calls, some vars will be captured by move/copy.
	captured_var_stack: VarStack
}

impl Lambda {
	pub fn new(arg_names: Vec<String>, body: List) -> Lambda {
		Lambda{binding: None, arg_names: arg_names, body: body, variadic: false,
			captured_var_stack: vec![]}
	}

	pub fn new_variadic(arg_name: String, body: List) -> Lambda {
		Lambda{binding: None, arg_names: vec![arg_name], body: body, variadic: true,
			captured_var_stack: vec![]}
	}

	pub fn n_args(&self) -> uint {
		self.arg_names.len()
	}
}

// Either a scheme lambda, or a rust function
#[derive(Clone)]
pub enum LamOrFn {
	Lam(Lambda),
	Fn(&'static ScmFn)
}

// `fn`s does not, at least at the moment, implement `PartialEq`.
impl PartialEq for LamOrFn {
	fn eq(&self, other: &LamOrFn) -> bool {
		match (self, other) {
			(&LamOrFn::Lam(ref p1), &LamOrFn::Lam(ref p2)) => p1 == p2,
			(_, _) => false
		}
	}
}

/// An element in an S-Expression list
#[derive(Clone, PartialEq)]
#[allow(dead_code)]
pub enum SEle {
	SExpr(List),
	SBinding(String),
	SNum(f64),
	SStr(String),
	SSymbol(String),
	SBool(bool),
	SList(List),
	// The SProc itself may optionally contain a name. This is applied in `apply_args` so that
	// the procedure may call itself
	// TODO: fix better solution than sometimes having a name.
	SProc(Box<LamOrFn>)
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
			&SList(ref l) if l == &List::Nil => "Empty List (Nil)",
			&SList(_) => "List",
			&SProc(_) => "Procedure"
		}
	}

	fn into_expr(self) -> List {
		if let SExpr(expr) = self {
			expr
		} else {
			panic!("Element is not an expression!")
		}
	}
}

impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref xs) | SList(ref xs) => write!(f, "({})", xs),
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "\"{}\"", s),
			SSymbol(ref s) => write!(f, "{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
			SProc(box LamOrFn::Lam(ref p)) => write!(f,
				"#procedure: arity={}, captured={}", p.n_args(),
				p.captured_var_stack),
			SProc(box LamOrFn::Fn(_)) => write!(f, "#procedure")
		}
	}
}

pub fn unit() -> SEle {
	SList(List::Nil)
}

pub type ScmFn = fn(&mut Env, List) -> SEle;

pub struct Env {
	pub var_stack: VarStack,
	// When the `error_mode` is set to `Warn`, all errors are reported as warnings.
	// Useful when run in shell mode and stuff like undefined vars should not crash the session.
	pub error_mode: ScmAlertMode
}

impl Env {
	pub fn new(var_definitions: VarStack) -> Env {
		Env {var_stack: var_definitions, error_mode: ScmAlertMode::Error }
	}

	pub fn make_strict(&mut self) {
		self.error_mode = ScmAlertMode::Error
	}

	pub fn make_lenient(&mut self) {
		self.error_mode = ScmAlertMode::Warn
	}

	/// Evaluate `val` and push it to either var stack of self, or optionally `maybe_stack`.
	pub fn define_var(&mut self, binding: String, val: SEle, maybe_stack: Option<&mut VarStack>)
	{
		let val = self.eval(val);
		let stack = if let Some(stack) = maybe_stack { stack } else { &mut self.var_stack };
		stack.push((binding, val));
	}

	pub fn get_var(&mut self, to_get: &str) -> SEle {
		if let Some(result) = get_var(&mut self.var_stack, to_get) {
			result
		} else {
			scheme_alert(ScmAlert::Undef(to_get), &self.error_mode)
		}
	}

	pub fn set_var(&mut self, to_set: &str, mut value: SEle) -> bool {
		value = self.eval(value);
		for &(ref binding, ref mut var) in self.var_stack.iter_mut().rev() {
			if *binding == to_set {
				*var = value;
				return true;
			} else {
				continue;
			}
		}
		scheme_alert(ScmAlert::Undef(to_set), &self.error_mode);
		false
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

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	/// Evaluate the Scheme expression, assuming `expr` is a list of the expression body
	/// starting with a procedure binding. May return binding.
	fn eval_expr_lazy(&mut self, mut expr: List) -> SEle {
		match self.eval(expr.pop_head().unwrap()) {
			SProc(box LamOrFn::Lam(procedure)) => self.run_lambda(procedure, expr),
			SProc(box LamOrFn::Fn(func)) => (*func)(self, expr),
			x => scheme_alert(ScmAlert::WrongType("Procedure", x.variant()),
				&self.error_mode)
		}
	}

	// Will not return binding
	fn eval_expr_to_tail(&mut self, mut expr: List) -> SEle {
		while let Some(_) = expr.head() {
			let head = expr.head().unwrap().clone();
			if let SBinding(proc_name) = head {
				match proc_name.as_slice() {
					// Exceptions: Procedures that have tail contexts (http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.20)
					// and procedures where the arguments should not be applied,
					// but rather sent directly to the function.
					"cond" | "begin" => {
						let evaled = self.eval_expr_lazy(expr);
						match evaled {
							SExpr(x) => expr = x,
							SBinding(b) => return
								self.get_var(b.as_slice()),
							_ => return evaled,
						}
					},
					_ => return SExpr(expr),
				}
			} else {
				return SExpr(expr);
			}
		}
		scheme_alert(ScmAlert::Bad("Expression"), &self.error_mode)
	}

	fn get_bound_elem(&mut self, ele: SEle) -> SEle {
		match ele {
			SBinding(b) => self.get_var(b.as_slice()),
			_ => ele 
		}
	}

	// Get the value of the element. If it's an expression, get the evaluation. If it's a
	// binding, get the associated value. If it's a value, return it.
	pub fn eval(&mut self, ele: SEle) -> SEle {
		match ele {
			SExpr(x) => {
				self.trampoline(x)	
			},
			SBinding(x) => {
				let bound_ele = self.get_var(x.as_slice());
				self.eval(bound_ele)
			},
			_ => ele
		}
	}

	fn trampoline(&mut self, mut expr: List) -> SEle {
		loop {
			let evaled = {let tmp = self.eval_expr_lazy(expr); self.get_bound_elem(tmp)};
			if let SExpr(inner_expr) = evaled {
				expr = inner_expr;
			} else {
				return evaled;
			}
		}
	}

	fn eval_sequence_lazy(&mut self, exprs: List) -> SEle {	
		if exprs != List::Nil {
			let mut it = exprs.into_iter();
			while let Some(elem) = it.next() {
				if it.peek() == None {
					return self.get_bound_elem(elem);
				}
				self.eval(elem);
			}
		}
		unit()
	}

	// Evaluates the elements in `exprs` ins equence, returning the value of the last eval.
	pub fn eval_sequence(&mut self, exprs: List) -> SEle {
		let last = self.eval_sequence_lazy(exprs);
		self.eval(last)
	}

	// Evaluates all elements in `exprs` and collect into List.
	fn eval_multiple(&mut self, exprs: List) -> List {
		exprs.into_iter().map(|e| self.eval(e)).collect()
	}

	pub fn elem_is_true(&mut self, mut ele: SEle) -> bool {
		ele = self.get_bound_elem(ele);
		match ele {
			SBool(b) => b,
			SExpr(_) => {
				let val = self.eval(ele);
				self.elem_is_true(val)
			},
			_ => true
		}
	}

	/// Returns expression with evaled args.
	pub fn apply_args(&mut self, mut expr: List) -> List {
		let mut head = expr.pop_head().unwrap();
		if let SBinding(binding) = head.clone() {
			// TODO: When define, lambda, etc. are macros to internal functions that
			// work as expected when args are applied, remove following exceptions.
			if ["define", "lambda"].contains(&binding.as_slice()) {
				return List::with_body(head, expr);
			} else if let SProc(box LamOrFn::Lam(mut lambda)) = self.eval(head.clone())
			{
				lambda.binding = Some(binding);
				head = SProc(box LamOrFn::Lam(lambda));
			}
		}
		// if let SBinding(binding) = head.clone() {
		// 	// TODO: When define, lambda, etc. are macros to internal functions that
		// 	// work as expected when args are applied, remove following exceptions.
		// 	if ["define", "lambda"].contains(&binding.as_slice()) {
		// 		return List::with_body(head, expr);
		// 	} else {
		// 		lambda.binding = Some(binding);
		// 		head = self.eval(head.clone());
		// 	}
		// }
		let evaled_args = self.eval_multiple(expr);
		List::with_body(head, evaled_args)
	}

	// If the returned element is a lambda, or an expression with a user defined procedure as
	// its head, capture all vars in current scope. This is required in order for closures and
	// tail call optimization to work.
	pub fn capture_vars_for_lambda(&mut self, lambda: &mut Lambda, vars: VarStack) {
		for var_def in vars.into_iter() {
			lambda.captured_var_stack.push(var_def);
		}
	}

	// TODO: Implement TCO some different way. The procedure must be run in the scope it is
	// called or either a) closures won't be possible; or b) the stack will grow fuck fast.
	// Maybe compare tail expression name. If same, apply args and return expression for
	// trampolining. Else, eval the expression in current scope.
	fn run_lambda(&mut self, mut lambda: Lambda, args: List) -> SEle {
		let previous_n_vars = self.var_stack.len();
		let n_args = lambda.n_args();

		// Define all the vars!
		if let Some(ref lam_name) = lambda.binding {
			self.define_var(lam_name.clone(), SProc(box LamOrFn::Lam(lambda.clone())),
				None);
		}
		for var_def in lambda.captured_var_stack.iter() {
			self.var_stack.push(var_def.clone());
		}
		if lambda.variadic {
			self.define_var(lambda.arg_names.pop().unwrap(), SList(args), None);
		} else {
			if n_args != args.len() {
				return scheme_alert(ScmAlert::ArityMiss("_",
					n_args,"!=",args.len()), &self.error_mode)
			}
			for (var_name, var_val) in lambda.arg_names.clone().into_iter()
				.zip(args.into_iter())
			{
				self.define_var(var_name, var_val, None);
			}
		}
		let maybe_name =
			if let Some(ref name) = lambda.binding { Some(name) } else { None };
		let mut tail_elem = self.eval_expr_to_tail(lambda.body);
		let n_vars_in_block = self.var_stack.len() - previous_n_vars;
		if let SExpr(tail_expr) = tail_elem {
			// Tail Call Elimination
			let same_name = if let (Some(lambda_name), Some(head)) =
				(maybe_name, tail_expr.head())
			{
				let head_binding = match head {
					&SBinding(ref b) => Some(b),
					&SProc(box LamOrFn::Lam(ref lambda)) =>
						match lambda.binding {
							Some(ref n) => Some(n), _ => None },
					_ => None,
				};
				Some(lambda_name) == head_binding
			} else { false };
			if same_name {
				let result = SExpr(self.apply_args(tail_expr));
				pop_var_defs(&mut self.var_stack, n_vars_in_block);
				return result;
			}
			let with_applied_args = SExpr(self.apply_args(tail_expr));
			let mut evaled = self.eval(with_applied_args);
			// FIXME, repetition right below
			if let SProc(box LamOrFn::Lam(ref mut lambda)) = evaled {
				// Closure - Capture environment.
				let vars_from_block = pop_var_defs(&mut self.var_stack, n_vars_in_block);
				self.capture_vars_for_lambda(lambda, vars_from_block);
			} else {
				pop_var_defs(&mut self.var_stack, n_vars_in_block);
			}
			return evaled;
		} else if let SProc(box LamOrFn::Lam(ref mut lambda)) = tail_elem {
			// Closure - Capture environment.
			let vars_from_block = pop_var_defs(&mut self.var_stack, n_vars_in_block);
			self.capture_vars_for_lambda(lambda, vars_from_block);
		} else {
			pop_var_defs(&mut self.var_stack, n_vars_in_block);
		}
		tail_elem
	}
}