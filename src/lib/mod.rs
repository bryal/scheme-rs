#![allow(dead_code)]
#![allow(unused_attributes)]


// TODO: Consider making more use of pass-by-reference rather than cloning everywhere
// TODO: Document wether functions return bindings, atoms, expressions or whatever.
// TODO: display line number on error
// NOTE: Maybe change use of List to rusts own DList? Cleaner, and maybe better performance.
// http://doc.rust-lang.org/std/collections/dlist/struct.DList.html
// TODO: add tests
// TODO: Add Reference variant to SEle. Make stuff work using references where possible.
// TODO: Find out when exactly an expression might lack a head.
// TODO: Fix procedures having to be copied everytime an expression is evaluated.

use std::fmt;
use std::cmp;
use std::mem;

pub use super::linked_list::List;
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
	WrongType(&'a str, &'a SEle),
	ArityMiss(&'a str, usize, &'a str, usize),
	Bad(&'a str),
	Undef(&'a str),
	NaN(SEle),
	Unclosed,
	Custom(String)
}

pub fn scheme_alert(alert: ScmAlert, a_type: &ScmAlertMode) -> SEle {
	match alert {
		ScmAlert::Unexp(s) => println!("; {:?}: Unexpected `{}`", a_type, s),
		ScmAlert::WrongType(exp, got) =>
			println!("; {:?}: Wrong type. Expected `{}`, found `{}: {}`", a_type, exp,
				got, got.variant()),
		ScmAlert::Undef(s) => println!("; {:?}: Undefined variable `{}`", a_type, s),
		ScmAlert::Bad(s) => println!("; {:?}: Bad {}", a_type, s),
		ScmAlert::NaN(e) =>
			println!("; {:?}: `{}: {}` is not a number", a_type, e, e.variant()),
		ScmAlert::Unclosed => println!("; {:?}: Unclosed delimiter", a_type),
		ScmAlert::ArityMiss(s, got,vs, exp) =>
			println!("; {:?}: Arity missmatch in `{}`, {} {} {}", a_type, s, got,vs,exp),
		ScmAlert::Custom(s) => println!("; {:?}: {}", a_type, s),
	}
	if let &ScmAlertMode::Error = a_type {
		panic!()
	}
	scm_nil()
}

// Option because values may be moved.
type VarStack = Vec<(String, Option<SEle>)>;

fn clone_var(stack: &VarStack, to_get: &str) -> Result<SEle, String> {
	for &(ref binding, ref maybe_moved) in stack.iter().rev() {
		if *binding == to_get {
			if let &Some(ref var) = maybe_moved {
				return Ok(var.clone());
			} else {
				return Err(format!("Variable `{}` has been moved.", to_get));
			}
		} else {
			continue;
		}
	}
	return Err(format!("Variable `{}` is undefined.", to_get));
}
fn move_var(stack: &mut VarStack, to_get: &str) -> Result<SEle, String> {
	for &mut (ref binding, ref mut var_in_stack) in stack.iter_mut().rev() {
		if *binding == to_get {
			if let Some(var) = mem::replace(var_in_stack, None) {
				return Ok(var);
			} else {
				return Err(format!("Variable `{}` has been moved.", to_get));
			}
		} else {
			continue;
		}
	}
	return Err(format!("Variable `{}` is undefined.", to_get));
}
fn ref_var<'a>(stack: &'a VarStack, to_get: &str) -> Result<&'a SEle, String> {
	for &(ref binding, ref var_in_stack) in stack.iter().rev() {
		if *binding == to_get {
			if let &Some(ref var) = var_in_stack {
				return Ok(var);
			} else {
				return Err(format!("Variable `{}` has been moved.", to_get));
			}
		} else {
			continue;
		}
	}
	return Err(format!("Variable `{}` is undefined.", to_get));
}
fn ref_mut_var<'a>(stack: &'a mut VarStack, to_get: &str) -> Result<&'a mut SEle, String> {
	for &mut (ref binding, ref mut var_in_stack) in stack.iter_mut().rev() {
		if *binding == to_get {
			if let &mut Some(ref mut var) = var_in_stack {
				return Ok(var);
			} else {
				return Err(format!("Variable `{}` has been moved.", to_get));
			}
		} else {
			continue;
		}
	}
	return Err(format!("Variable `{}` is undefined.", to_get));
}

fn pop_var_defs(stack: &mut VarStack, n_to_pop: usize) -> VarStack {
	(0..cmp::min(n_to_pop, stack.len())).map(|_| stack.pop().unwrap()).collect::<Vec<_>>()
		.into_iter().rev().collect()
}

pub type ScmFn = fn(&mut Env, List<SEle>) -> SEle;
impl PartialEq for &'static ScmFn {
	fn eq(&self, other: &Self) -> bool {
		(*self) as *const ScmFn == (*other) as *const ScmFn
	}
}

#[derive(Clone, PartialEq)]
pub struct Lambda {
	arg_names: Vec<String>,
	body: SEle,
	variadic: bool,
	// Lambdas will capture variables by reference when in scope, but if the lambda is passed to
	// a lower scope, or in case of Tail Calls, some vars will be captured by move/copy.
	captured_var_stack: VarStack
}
impl Lambda {
	pub fn new(arg_names: Vec<String>, body: SEle) -> Lambda {
		Lambda{arg_names: arg_names, body: body, variadic: false,
			captured_var_stack: vec![]}
	}
	pub fn new_variadic(arg_name: String, body: SEle) -> Lambda {
		Lambda{arg_names: vec![arg_name], body: body, variadic: true,
			captured_var_stack: vec![]}
	}

	pub fn n_args(&self) -> usize {
		self.arg_names.len()
	}
}

// Either a scheme lambda, or a rust function
#[derive(Clone)]
pub enum LamOrFn {
	Lam(Lambda),
	Fn(&'static ScmFn)
}
impl PartialEq for LamOrFn {
	fn eq(&self, other: &LamOrFn) -> bool {
		match (self, other) {
			(&LamOrFn::Lam(ref p1), &LamOrFn::Lam(ref p2)) => p1 == p2,
			(&LamOrFn::Fn(a), &LamOrFn::Fn(b)) => a == b,
			_ => false
		}
	}
}

/// An element in an S-Expression list
#[derive(Clone, PartialEq)]
#[allow(dead_code)]
pub enum SEle {
	SExpr(List<SEle>),
	SBinding(String),
	SNum(f64),
	SStr(String),
	SSymbol(String),
	SBool(bool),
	SList(List<SEle>),
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
			&SList(ref l) if l == &list![] => "Empty List (Nil)",
			&SList(_) => "List",
			&SProc(_) => "Procedure"
		}
	}

	fn into_expr(self) -> List<SEle> {
		if let SExpr(expr) = self {
			expr
		} else {
			panic!("Element is not an expression. `{}`", self)
		}
	}
	fn into_list(self) -> List<SEle> {
		if let SList(list) = self {
			list
		} else {
			panic!("Element is not a list. `{}`", self)
		}
	}
	fn binding(&self) -> &String {
		if let &SBinding(ref b) = self {
			b
		} else {
			panic!("Element is not a binding. `{}`", self)
		}
	}
	fn into_binding(self) -> String {
		if let SBinding(b) = self {
			b
		} else {
			panic!("Element is not a binding. `{}`", self)
		}
	}
}
impl fmt::String for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref xs) | SList(ref xs) => write!(f, "({})", xs.partial_fmt()),
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "\"{}\"", s),
			SSymbol(ref s) => write!(f, "{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
			SProc(box LamOrFn::Lam(ref p)) =>
				write!(f, "#procedure: arity={}, captured={:?}", p.n_args(),
					p.captured_var_stack),
			SProc(box LamOrFn::Fn(_)) => write!(f, "#procedure")
		}
	}
}
impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref xs) => write!(f, "({})", xs.partial_fmt()),
			SList(ref xs) => write!(f, "[{}]", xs.partial_fmt()),
			SBinding(ref b) => write!(f, "{:?}", b),
			SNum(n) => write!(f, "{:?}", n),
			SStr(ref s) => write!(f, "\"{:?}\"", s),
			SSymbol(ref s) => write!(f, "{:?}", s),
			SBool(b) => write!(f, "{:?}", b),
			SProc(box LamOrFn::Lam(ref p)) =>
				write!(f, "#procedure: arity={}, captured={:?}", p.n_args(),
					p.captured_var_stack),
			SProc(box LamOrFn::Fn(_)) => write!(f, "#procedure")
		}
	}
}

pub fn scm_nil() -> SEle {
	SList(list![])
}

pub struct Env {
	pub var_stack: VarStack,
	// Error => Panic, Warn => Proceed as if nothing happened. In REPL, this is set to Warn.
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
	pub fn define_var(&mut self, binding: String, mut val: SEle) {
		val = self.eval(val);
		self.var_stack.push((binding, Some(val)));
	}
	pub fn set_var(&mut self, to_set: &str, mut value: SEle) -> bool {
		value = self.eval(value);
		for &mut(ref binding, ref mut var) in self.var_stack.iter_mut().rev() {
			if *binding == to_set {
				// NOTE: Should moved values be settable?
				*var = Some(value);
				return true;
			} else {
				continue;
			}
		}
		scheme_alert(ScmAlert::Undef(to_set), &self.error_mode);
		false
	}

	pub fn clone_var(&self, to_get: &str) -> SEle {
		match clone_var(&self.var_stack, to_get) {
			Ok(result) => result,
			Err(e) => scheme_alert(ScmAlert::Custom(e), &self.error_mode)
		}
	}
	pub fn move_var(&mut self, to_get: &str) -> SEle {
		match move_var(&mut self.var_stack, to_get) {
			Ok(result) => result,
			Err(e) => scheme_alert(ScmAlert::Custom(e), &self.error_mode)
		}
	}
	pub fn ref_var(&self, to_get: &str) -> &SEle {
		match ref_var(&self.var_stack, to_get) {
			Ok(result) => result,
			Err(e) =>
				// TODO: Try to improve this. Maybe return result?
				{scheme_alert(ScmAlert::Custom(e), &ScmAlertMode::Error); panic!()}
		}
	}
	pub fn ref_mut_var(&mut self, to_get: &str) -> &mut SEle {
		match ref_mut_var(&mut self.var_stack, to_get) {
			Ok(result) => result,
			Err(e) =>
				{scheme_alert(ScmAlert::Custom(e), &ScmAlertMode::Error); panic!()}
		}
	}

	fn clone_if_binding(&mut self, ele: SEle) -> SEle {
		match ele {
			SBinding(b) => self.clone_var(b.as_slice()),
			_ => ele 
		}
	}

	// Extract the procedure name and argument names for the `define` header `head`
	// TODO: use scheme_alert instead of panic
	// TODO: this is unnecesary, remove.
	pub fn dismantle_proc_head(&mut self, mut head: List<SEle>) -> (String, Vec<String>) {
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
			panic!("`{:?}` is not a valid procedure binding", head)
		}
	}

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	/// Evaluate the Scheme expression, assuming `expr` is a list of the expression body
	/// starting with a procedure binding. Will return expression in case of tail call.
	fn eval_expr(&mut self, mut expr: List<SEle>) -> SEle {
		let (head_val, opt_name) = match expr.pop_head().unwrap() {
				SBinding(bnd) => (self.clone_var(bnd.as_slice()), Some(bnd)),
				e => (e, None)
			};
		match head_val {
			SProc(box LamOrFn::Lam(lambda)) => self.run_lambda(lambda, opt_name, expr),
			SProc(box LamOrFn::Fn(func)) => (*func)(self, expr),
			x => scheme_alert(ScmAlert::WrongType("Procedure", &x), &self.error_mode),
		}
	}
	/// Evaluate to the expression in tail context 
	/// http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_11.20
	fn eval_expr_to_tail(&mut self, mut expr: List<SEle>) -> SEle {
		while !expr.is_empty() {
			let head = expr.pop_head().unwrap();
			let has_tail_context = if let SBinding(ref proc_name) = head {
					["cond", "begin"].contains(&proc_name.as_slice())
				} else {
					false
				};
			if has_tail_context {
				
				match self.eval_expr(List::with_body(head, expr)) {
					SExpr(x) => expr = x,
					SBinding(b) => return self.clone_var(b.as_slice()),
					atom => return atom,
				}
			} else {
				return SExpr(List::with_body(head, expr));
			}
		}
		scheme_alert(ScmAlert::Bad("Expression"), &self.error_mode)
	}
	// If it's an expression, get the evaluation. If it's a binding, get the associated value.
	// If it's a value, return it.
	pub fn eval(&mut self, ele: SEle) -> SEle {
		match ele {
			SExpr(x) => {
				self.trampoline(x)	
			},
			SBinding(x) => {
				let bound_ele = self.clone_var(x.as_slice());
				self.eval(bound_ele)
			},
			_ => ele
		}
	}

	fn trampoline(&mut self, mut expr: List<SEle>) -> SEle {
		loop {
			let evaled = {let tmp = self.eval_expr(expr); self.clone_if_binding(tmp)};
			if let SExpr(mut inner_expr) = evaled {
				// The proc in the expression might me an old lambda. Captured vars
				// must be cleared.
				if let Some(&mut SProc(box LamOrFn::Lam(ref mut lambda))) =
					inner_expr.head_mut()
				{
					lambda.captured_var_stack.clear();
				}
				expr = inner_expr;
			} else {
				return evaled;
			}
		}
	}

	fn eval_sequence_lazy(&mut self, exprs: List<SEle>) -> SEle {	
		if exprs != list![] {
			let mut it = exprs.into_iter();
			while let Some(elem) = it.next() {
				if it.peek() == None {
					return self.clone_if_binding(elem);
				}
				self.eval(elem);
			}
		}
		scm_nil()
	}

	// Evaluates the elements in `exprs` ins equence, returning the value of the last eval.
	pub fn eval_sequence(&mut self, exprs: List<SEle>) -> SEle {
		let last = self.eval_sequence_lazy(exprs);
		self.eval(last)
	}

	// Evaluates all elements in `exprs` and collect into List<SEle>.
	fn eval_multiple(&mut self, exprs: List<SEle>) -> List<SEle> {
		exprs.into_iter().map(|e| self.eval(e)).collect()
	}

	pub fn elem_is_true(&mut self, mut ele: SEle) -> bool {
		ele = self.clone_if_binding(ele);
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
	pub fn apply_args(&mut self, mut expr: List<SEle>) -> List<SEle> {
		let head = expr.pop_head().unwrap();
		let is_exception = if let SBinding(ref binding) = head {
				// Exceptions where the application of args is not possible, even
				// with macros.
				["define", "lambda", "quote"].contains(&binding.as_slice())
			} else { false };
		if is_exception {
			List::with_body(head, expr)
		} else {
			List::with_body(head, self.eval_multiple(expr))
		}
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
	fn run_lambda(&mut self, mut lambda: Lambda, maybe_name: Option<String>, args: List<SEle>)
		-> SEle
	{
		let previous_n_vars = self.var_stack.len();
		let n_args = lambda.n_args();

		// Define all the vars!
		for var_def in lambda.captured_var_stack.iter() {
			self.var_stack.push(var_def.clone());
		}
		if lambda.variadic {
			self.define_var(lambda.arg_names.pop().unwrap(), SList(args));
		} else {
			if n_args != args.len() {
				return scheme_alert(ScmAlert::ArityMiss("_",
					n_args,"!=",args.len()), &self.error_mode)
			}
			for (var_name, var_val) in lambda.arg_names.clone().into_iter()
				.zip(args.into_iter())
			{
				self.define_var(var_name, var_val);
			}
		}

		let mut tail_elem = lambda.body.clone();
		if let SExpr(lambda_expr) = tail_elem {
			tail_elem = self.eval_expr_to_tail(lambda_expr);
		}
		self.tail_elem_to_return(tail_elem, maybe_name, previous_n_vars,
			&lambda.captured_var_stack)
	}

	// Takes the tail element of an expression and depending on the type of the element pop vars
	// and capture for lambdas etc. Return the final element that the calling procedure is to
	// return.
	// TODO: this is an ugly method! REMEDY THIS!
	fn tail_elem_to_return(&mut self, mut tail_elem: SEle, maybe_name: Option<String>,
		previous_n_vars: usize, caller_params: &VarStack) -> SEle
	{
		if let SExpr(tail_expr) = tail_elem {
			let mut applied = self.apply_args(tail_expr);
			// Tail Call Elimination
			// If the name of the proc in tail == name of caller, it's a tail call.
			let same_name = if let Some(&SBinding(ref tail_name)) = applied.head() {
					maybe_name.as_ref() == Some(tail_name)
				} else { false };
			if same_name {
				// Name can't come from local binding, e.g. callers parameters,
				// because then there may be conflicting args in lambda body.
				if let Ok(new_head) = clone_var(caller_params,
					applied.head().unwrap().binding().as_slice())
				{
					*applied.head_mut().unwrap() = new_head;
					tail_elem = self.eval(SExpr(applied));
				} else {
					tail_elem = SExpr(applied);
				}
			} else {
				tail_elem = self.eval(SExpr(applied));
			}
		}
		let n_vars_in_block = self.var_stack.len() - previous_n_vars;
		if let SProc(box LamOrFn::Lam(ref mut lambda)) = tail_elem {
			// Closure - Capture environment.
			let vars_from_block = pop_var_defs(&mut self.var_stack, n_vars_in_block);
			self.capture_vars_for_lambda(lambda, vars_from_block);
		} else {
			pop_var_defs(&mut self.var_stack, n_vars_in_block);
		}
		tail_elem
	}
}