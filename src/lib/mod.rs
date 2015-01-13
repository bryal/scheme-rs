#![allow(dead_code)]
#![allow(unused_attributes)]


// TODO: Consider making more use of pass-by-reference rather than cloning everywhere
// TODO: Document wether functions return bindings, atoms, expressions or whatever.
// TODO: display line number on error
// NOTE: Maybe change use of List to rusts own DList? Cleaner, and maybe better performance.
// http://doc.rust-lang.org/std/collections/dlist/struct.DList.html

use std::fmt;
use std::cmp;

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
		ScmAlert::WrongType(exp, got) =>
			println!("; {}: Wrong type. Expected `{}`, found `{}`", a_type, exp, got),
		ScmAlert::Undef(s) => println!("; {}: Undefined variable `{}`", a_type, s),
		ScmAlert::Bad(s) => println!("; {}: Bad {}", a_type, s),
		ScmAlert::NaN(e) =>
			println!("; {}: `{}: {}` is not a number", a_type, e.variant(), e),
		ScmAlert::Unclosed => println!("; {}: Unclosed delimiter", a_type),
		ScmAlert::ArityMiss(s, got,vs, exp) =>
			println!("; {}: Arity missmatch in `{}`, {} {} {}", a_type, s, got,vs,exp),
		ScmAlert::Custom(s) => println!("; {}: {}", a_type, s),
	}
	if let &ScmAlertMode::Error = a_type {
		panic!()
	}
	unit()
}

type VarStack = Vec<(String, SEle)>;

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
	(0..cmp::min(n, stack.len())).map(|_| stack.pop().unwrap()).collect::<Vec<_>>()
		.into_iter().rev().collect()
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
		if let (&LamOrFn::Lam(ref p1), &LamOrFn::Lam(ref p2)) = (self, other) {
			p1 == p2
		} else { false }
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
	fn into_binding(self) -> String {
		if let SBinding(b) = self {
			b
		} else {
			panic!("Element is not a binding. `{}`", self)
		}
	}
}
impl fmt::Show for SEle {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			SExpr(ref xs) | SList(ref xs) => write!(f, "({})", xs.partial_fmt()),
			SBinding(ref b) => write!(f, "{}", b),
			SNum(n) => write!(f, "{}", n),
			SStr(ref s) => write!(f, "\"{}\"", s),
			SSymbol(ref s) => write!(f, "{}", s),
			SBool(b) => write!(f, "{}", if b {"#t"} else {"#f"}),
			SProc(box LamOrFn::Lam(ref p)) =>
				write!(f, "#procedure: arity={}, captured={}", p.n_args(),
					p.captured_var_stack),
			SProc(box LamOrFn::Fn(_)) => write!(f, "#procedure")
		}
	}
}

pub fn unit() -> SEle {
	SList(list![])
}

pub type ScmFn = fn(&mut Env, List<SEle>) -> SEle;

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
	pub fn define_var(&mut self, binding: String, mut val: SEle, maybe_stack: Option<&mut VarStack>)
	{
		val = self.eval(val);
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
			panic!("`{}` is not a valid procedure binding", head)
		}
	}

	// TODO: Fix to work with empty lists by remedying the `unwrap`
	/// Evaluate the Scheme expression, assuming `expr` is a list of the expression body
	/// starting with a procedure binding. May return binding.
	fn eval_expr_lazy(&mut self, mut expr: List<SEle>) -> SEle {
		let opt_name = if let &SBinding(ref name) = expr.head().unwrap() {
				Some(name.clone())
			} else { None };
		match self.eval(expr.pop_head().unwrap()) {
			SProc(box LamOrFn::Lam(lambda)) => self.run_lambda(lambda, opt_name, expr),
			SProc(box LamOrFn::Fn(func)) => (*func)(self, expr),
			x => scheme_alert(ScmAlert::WrongType("Procedure", x.variant()),
				&self.error_mode)
		}
	}

	// Will not return binding
	fn eval_expr_to_tail(&mut self, mut expr: List<SEle>) -> SEle {
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

	fn trampoline(&mut self, mut expr: List<SEle>) -> SEle {
		loop {
			let evaled = {let tmp = self.eval_expr_lazy(expr); self.get_bound_elem(tmp)};
			if let SExpr(mut inner_expr) = evaled {
				// The proc in the expression might me an old lambda. Captured vars
				// must be cleared.
				if let Some(&SProc(box LamOrFn::Lam(ref mut lambda))) =
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
					return self.get_bound_elem(elem);
				}
				self.eval(elem);
			}
		}
		unit()
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
	pub fn apply_args(&mut self, mut expr: List<SEle>) -> List<SEle> {
		let head = expr.pop_head().unwrap();
		let is_exception = if let SBinding(ref binding) = head {
				["define", "lambda"].contains(&binding.as_slice())
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

		let mut tail_elem = lambda.body.clone();
		if let SExpr(lambda_expr) = tail_elem {
			tail_elem = self.eval_expr_to_tail(lambda_expr);
		}
		let n_vars_in_block = self.var_stack.len() - previous_n_vars;
		self.tail_elem_to_return(tail_elem, maybe_name, n_vars_in_block)
	}

	// Takes the tail element of an expression and depending on the type of the element pop vars
	// and capture for lambdas etc. Return the final element that the calling procedure is to
	// return.
	// TODO: this is an ugly method! REMEDY THIS!
	fn tail_elem_to_return(&mut self, mut tail_elem: SEle, maybe_name: Option<String>,
		n_vars_in_block: uint) -> SEle
	{
		if let SExpr(tail_expr) = tail_elem {
			let applied = self.apply_args(tail_expr);
			// Tail Call Elimination
			// If the name of the proc in tail == name of caller, it's a tail call
			let is_tailcall = if let Some(&SBinding(ref tail_name)) = applied.head() {
					maybe_name.as_ref() == Some(tail_name)
				} else { false };
			if is_tailcall {
				pop_var_defs(&mut self.var_stack, n_vars_in_block);
				return SExpr(applied);
			}
			
			tail_elem = self.eval(SExpr(applied));
		}
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