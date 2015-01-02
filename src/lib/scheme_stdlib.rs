use std::f64::consts::PI;
use std::f64::NAN;

use super::{
	List,
	SEle,
	Env,
	ScmFn,
	Lambda,
	LamOrFn,
	ScmAlert,
	scheme_alert,
	unit,
	pop_var_defs};
use super::SEle::*;

// TODO: instead of doing env.eval_expr, the direct use of the scm_* function can often be used
// e.g. scm_define(env, list)
// TODO: Make the arithmetic functions iterative rather than recursive. They are recursive to
// imitate scheme, but it is probably not optimal for performance
// TODO: Concretely decide which functions/procedures will return what sort of values.
// e.g. `begin` will not return a binding, only expressions with applied arguments or literals
// TODO: macro for constructing scheme expression

fn scm_add(env: &mut Env, args: List) -> SEle {
	let mut acc = 0.0;
	for expr in args.into_iter() {
		let ele = env.eval(expr);
		if let SNum(x) = ele {
			acc += x;
		} else {
			scheme_alert(ScmAlert::NaN(ele), &env.error_mode);
		}
	}
	SNum(acc)
}
fn scm_append(env: &mut Env, args: List) -> SEle {
	let mut list = List::new();
	for maybe_list in args.into_iter() {
		if let SList(mut other_list) = env.eval(maybe_list) {
			list.last_link().swap_tail(&mut other_list).unwrap();
		} else {
			return scheme_alert(ScmAlert::WrongType("List", "_"), &env.error_mode);
		}
	}
	SList(list)
}
// TODO: define's are lost on return of applied expression. In case of local procedure, things fuck.
// might be specific to define's, check it out
/// Will return literal or expression with applied args, no bindings.
fn scm_begin(env: &mut Env, args: List) -> SEle {
	let previous_n_vars = env.var_stack.len();
	let first_pass = env.eval_sequence_lazy(args);
	let mut tail_element = if let SExpr(expr) = first_pass {
		let tail_elem = env.eval_expr_to_tail(expr);
		if let SExpr(tail_expr) = tail_elem {
			SExpr(env.apply_args(tail_expr))
		} else {
			tail_elem
		}
	} else {
		first_pass
	};
	let current_n_vars = env.var_stack.len();
	let popped_var_defs = pop_var_defs(&mut env.var_stack, current_n_vars - previous_n_vars);
	tail_element = env.capture_vars_for_lambda(tail_element, popped_var_defs);
	tail_element
}
fn scm_car(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		scheme_alert(ScmAlert::ArityMiss("car", args.len(),"!=",1), &env.error_mode)
	} else {
		let evaled = env.eval(args.pop_head().unwrap());
		if let SList(mut list) = evaled {
			if let Some(head) = list.pop_head() {
				head
			} else {
				scheme_alert(ScmAlert::WrongType("List", "Empty List (Nil)"),
					&env.error_mode)
			}
		} else {
			scheme_alert(ScmAlert::WrongType("List", evaled.variant()), &env.error_mode)
		}
	}
}
fn scm_cdr(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		scheme_alert(ScmAlert::ArityMiss("cdr", args.len(),"!=",1), &env.error_mode)
	} else {
		let evaled = env.eval(args.pop_head().unwrap());
		if let SList(mut list) = evaled {
			if let Some(tail) = list.pop_tail() {
				SList(tail)
			} else {
				scheme_alert(ScmAlert::WrongType("List", "Empty List (Nil)"),
					&env.error_mode)
			}
		} else {
			scheme_alert(ScmAlert::WrongType("List", evaled.variant()), &env.error_mode)
		}
	}
}
/// Will return any element or composite expression, including bindings.
fn scm_cond(env: &mut Env, args: List) -> SEle {
	if args.len() < 2 {
		return scheme_alert(ScmAlert::ArityMiss("cond", args.len(),"<",2), &env.error_mode);
	}
	for clause in args.into_iter() {
		if let SExpr(mut clause_v) = clause {
			if clause_v.len() != 2 {
				return scheme_alert(ScmAlert::Bad("cond clause"), &env.error_mode);
			}
			let test = clause_v.pop_head().unwrap();
			if let SBinding(maybe_else) = test {
				if maybe_else.as_slice() == "else" {
					return clause_v.pop_head().unwrap();
				}
			} else {
				if match env.eval(test) { SBool(b) => b, _ => true } {
					return clause_v.pop_head().unwrap();
				}
			}
		} else {
			return scheme_alert(ScmAlert::Bad("cond clause"), &env.error_mode);
		}
	}
	// Disableable scheme alert about non-exhaustible pattern?
	unit()
}
// TODO: In case of defining expression to return lambda using the expressions args, lambda requires
// associated var defs
pub fn scm_define(env: &mut Env, mut args: List) -> SEle {
	if args.len() < 2 {
		scheme_alert(ScmAlert::ArityMiss("define", args.len(),"<",2), &env.error_mode)
	} else {
		match args.pop_head().unwrap() {
			SBinding(binding) =>
				if args.len() != 1 {
					scheme_alert(ScmAlert::ArityMiss("define",
						args.len(),"<",2), &env.error_mode);
				} else {
					env.define_var(binding, args.pop_head().unwrap(), None);
				},
			SExpr(head) => {
				let wrapped_body = List::with_body(SBinding("begin".to_string()),
					args);
				let (proc_name, arg_names) = env.dismantle_proc_head(head);
				let procedure = box LamOrFn::Lam(Lambda::new(arg_names,
						wrapped_body));
				scm_define(env, List::with_body(SBinding(proc_name),
						List::with_head(SProc(procedure))));
			},
			_ => {scheme_alert(ScmAlert::Bad("define body"), &env.error_mode);},
		}
		unit()
	}
}
fn scm_display(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		panic!("Arity missmatch")
	}
	println!("{}", env.eval(args.pop_head().expect(
		"Could not retrieve argument")));
	unit()
}
fn scm_div_iter(env: &mut Env, numerator: f64, mut denoms: List) -> f64 {
	if let Some(expr) = denoms.pop_head() {
		match env.eval(expr) {
			SNum(x) => scm_div_iter(env, numerator / x, denoms),
			x => {
				scheme_alert(ScmAlert::NaN(x), &env.error_mode);
				NAN
			}
		}
	} else {
		numerator
	}
}
fn scm_div(env: &mut Env, mut args: List) -> SEle {
	SNum(if let Some(expr) = args.pop_head() {
		if let SNum(x) = env.eval(expr) {
			if args.len() < 1 {
				1.0 / x
			} else {
				scm_div_iter(env, x, args)
			}
		} else {
			panic!("NaN")
		}
	} else {
		panic!("Wrong number of arguments")
	})
}
fn scm_eqv_q(env: &mut Env, mut args: List) -> SEle {
	if args.len() == 0 || args.len() == 1 {
		SBool(true)
	} else {
		args = env.eval_multiple(args);
		let v1 = args.pop_head().unwrap();
		SBool(args.into_iter().all(|v2| v1 == v2))
	}
}
fn scm_lambda(env: &mut Env, mut args: List) -> SEle {
	if args.len() < 2 {
		scheme_alert(ScmAlert::ArityMiss("lambda", args.len(),"<",2), &env.error_mode)
	} else {
		let wrapped_body = List::with_body(SBinding("begin".to_string()),
				args.pop_tail().unwrap());
		match args.pop_head().unwrap() {
			SBinding(binding) => {
				let procedure = Lambda::new_variadic(binding, wrapped_body);
				SProc(box LamOrFn::Lam(procedure))
			},
			SExpr(lambda_bindings) => {
				let lambda_arg_names = lambda_bindings.into_iter().map(|e|
						if let SBinding(s) = e { s }
						else { "".to_string() /* TODO: Error here? :/ */ }
					).collect();
				let procedure = Lambda::new(lambda_arg_names, wrapped_body);
				SProc(box LamOrFn::Lam(procedure))
			},
			_ => scheme_alert(ScmAlert::Bad("lambda body"), &env.error_mode),
		}
	}
}
fn scm_list(env: &mut Env, args: List) -> SEle {
	SList(args.into_iter().map(|e| env.eval(e)).collect())
}
// Like `list`, but uses the last argument as tail if list.
fn scm_list_star(env: &mut Env, args: List) -> SEle {
	let len = args.len();
	let mut iter = args.into_iter();
	let mut list: List =
		range(0, len - 2).map(|_| env.eval(iter.next().unwrap())).collect();
	{
		let mut next_to_last_cons = list.push(env.eval(iter.next().unwrap()));
		match env.eval(iter.next().unwrap()) {
			SList(mut last) => next_to_last_cons.swap_tail(&mut last).unwrap(),
			x => {next_to_last_cons.push(env.eval(x));},
		}
	}
	SList(list)
}
fn scm_mul(env: &mut Env, args: List) -> SEle {
	let mut acc = 1.0;
	for expr in args.into_iter() {
		let ele = env.eval(expr);
		if let SNum(x) = ele {
			acc *= x;
		} else {
			scheme_alert(ScmAlert::NaN(ele), &env.error_mode);
		}
	}
	SNum(acc)
}
#[allow(unused_variables)]
fn scm_newline(env: &mut Env, args: List) -> SEle {
	println!("");
	unit()
}
fn scm_not(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		scheme_alert(ScmAlert::ArityMiss("not", args.len(),"!=",1), &env.error_mode)
	} else {
		SBool(!env.elem_is_true(args.pop_head().unwrap()))
	}
}
fn scm_number_q(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		scheme_alert(ScmAlert::ArityMiss("number?", args.len(),"!=",1), &env.error_mode)
	} else {
		SBool(if let SNum(_) = env.eval(args.pop_head().unwrap()) { true } else { false })
	}
}
fn scm_op_eq(env: &mut Env, mut args: List) -> SEle {
	args = env.eval_multiple(args);
	if args.iter().all(|e| if let &SNum(_) = e { true } else { false }) {
		scm_eqv_q(env, args)
	} else {
		scheme_alert(ScmAlert::WrongType("Number", "_"), &env.error_mode)
	}
}
fn scm_op_gt(env: &mut Env, mut args: List) -> SEle {
	if args.len() == 0 || args.len() == 1 {
		SBool(true)
	} else {
		args = env.eval_multiple(args);
		let mut iter = args.into_iter().map(|e| if let SNum(n) = e { n } else {
				scheme_alert(ScmAlert::WrongType("Number", e.variant()),
					&env.error_mode);
				NAN
			});
		let mut last = iter.next().unwrap();
		SBool(iter.all(|e| { let ret = last > e; last = e; ret }))
	}
}
fn scm_op_gteq(env: &mut Env, mut args: List) -> SEle {
	if args.len() == 0 || args.len() == 1 {
		SBool(true)
	} else {
		args = env.eval_multiple(args);
		let mut iter = args.into_iter().map(|e| if let SNum(n) = e { n } else {
				scheme_alert(ScmAlert::WrongType("Number", e.variant()),
					&env.error_mode);
				NAN
			});
		let mut last = iter.next().unwrap();
		SBool(iter.all(|e| { let ret = last >= e; last = e; ret }))
	}
}
fn scm_op_lt(env: &mut Env, mut args: List) -> SEle {
	if args.len() == 0 || args.len() == 1 {
		SBool(true)
	} else {
		args = env.eval_multiple(args);
		let mut iter = args.into_iter().map(|e| if let SNum(n) = e { n } else {
				scheme_alert(ScmAlert::WrongType("Number", e.variant()),
					&env.error_mode);
				NAN
			});
		let mut last = iter.next().unwrap();
		SBool(iter.all(|e| { let ret = last < e; last = e; ret }))
	}
}
fn scm_op_lteq(env: &mut Env, mut args: List) -> SEle {
	if args.len() == 0 || args.len() == 1 {
		SBool(true)
	} else {
		args = env.eval_multiple(args);
		let mut iter = args.into_iter().map(|e| if let SNum(n) = e { n } else {
				scheme_alert(ScmAlert::WrongType("Number", e.variant()),
					&env.error_mode);
				NAN
			});
		let mut last = iter.next().unwrap();
		SBool(iter.all(|e| { let ret = last <= e; last = e; ret }))
	}
}
fn scm_push(env: &mut Env, mut args: List) -> SEle {
	if args.len() < 2 {
		scheme_alert(ScmAlert::ArityMiss("push", args.len(),"<",2), &env.error_mode)
	} else {
		match env.eval(args.pop_head().unwrap()) {
			SList(mut list) => {
				for e in args.into_iter().map(|e| env.eval(e)) {
					list.push(e);
				}
				SList(list)
			},
			x => scheme_alert(ScmAlert::WrongType("List", x.variant()), &env.error_mode)
		}
	}
}
fn scm_quote(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		scheme_alert(ScmAlert::ArityMiss("quote", args.len(),"!=",1), &env.error_mode)
	} else {
		let to_quote = args.pop_head().unwrap();
		match to_quote {
			SBinding(b) => SSymbol(b),
			SExpr(expr) => SList(expr.into_iter().map(|e| scm_quote(env,
							List::with_head(e))).collect()),
			_ => to_quote,
		}
	}
}
fn scm_remainder(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 2 {
		scheme_alert(ScmAlert::ArityMiss("remainder", args.len(),"!=",2), &env.error_mode)
	} else {
		let e1 = env.eval(args.pop_head().unwrap());
		let e2 = env.eval(args.pop_head().unwrap());
		match (e1, e2) {
			(SNum(n1), SNum(n2)) => SNum(n1 % n2),
			(t1, t2) => {
				scheme_alert(ScmAlert::WrongType("Number, Number", format!("{}, {}",
							t1.variant(), t2.variant()).as_slice()),
					&env.error_mode)
			}
		}
	}
}
fn scm_set_binding_to_value(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 2 {
		scheme_alert(ScmAlert::ArityMiss("set!", args.len(),"!=",2), &env.error_mode)
	} else {
		if let (SSymbol(to_set), value) = (env.eval(args.pop_head().unwrap()),
				args.pop_head().unwrap())
		{
			env.set_var(to_set.as_slice(), value);
			unit()
		} else {
			scheme_alert(ScmAlert::Bad("`set!`"), &env.error_mode)
		}
	}
}
fn scm_sub_iter(env: &mut Env, diff: f64, mut terms: List) -> f64 {
	if let Some(expr) = terms.pop_head() {
		match env.eval(expr) {
			SNum(x) => scm_sub_iter(env, diff - x, terms),
			x => {
				scheme_alert(ScmAlert::NaN(x), &env.error_mode);
				NAN
			}
		}
	} else {
		diff
	}
}
fn scm_sub(env: &mut Env, mut args: List) -> SEle {
	SNum(if let Some(expr) = args.pop_head() {
		if let SNum(x) = env.eval(expr) {
			scm_sub_iter(env, x, args)
		} else {
			panic!("NaN")
		}
	} else {
		0.0
	})
}
#[allow(unused_variables)]
fn scm_var_stack(env: &mut Env, args: List) -> SEle {
	println!("var stack: {}", env.var_stack);
	unit()
}

static P_SCM_ADD: &'static ScmFn = &(scm_add as ScmFn);
static P_SCM_APPEND: &'static ScmFn = &(scm_append as ScmFn);
static P_SCM_BEGIN: &'static ScmFn = &(scm_begin as ScmFn);
static P_SCM_CAR: &'static ScmFn = &(scm_car as ScmFn);
static P_SCM_CDR: &'static ScmFn = &(scm_cdr as ScmFn);
static P_SCM_COND: &'static ScmFn = &(scm_cond as ScmFn);
static P_SCM_DEFINE: &'static ScmFn = &(scm_define as ScmFn);
static P_SCM_DISPLAY: &'static ScmFn = &(scm_display as ScmFn);
static P_SCM_DIV: &'static ScmFn = &(scm_div as ScmFn);
static P_SCM_EQV_Q: &'static ScmFn = &(scm_eqv_q as ScmFn);
static P_SCM_LAMBDA: &'static ScmFn = &(scm_lambda as ScmFn);
static P_SCM_LIST: &'static ScmFn = &(scm_list as ScmFn);
static P_SCM_LIST_STAR: &'static ScmFn = &(scm_list_star as ScmFn);
static P_SCM_MUL: &'static ScmFn = &(scm_mul as ScmFn);
static P_SCM_NEWLINE: &'static ScmFn = &(scm_newline as ScmFn);
static P_SCM_NOT: &'static ScmFn = &(scm_not as ScmFn);
static P_SCM_NUMBER_Q: &'static ScmFn = &(scm_number_q as ScmFn);
static P_SCM_OP_EQ: &'static ScmFn = &(scm_op_eq as ScmFn);
static P_SCM_OP_GT: &'static ScmFn = &(scm_op_gt as ScmFn);
static P_SCM_OP_GTEQ: &'static ScmFn = &(scm_op_gteq as ScmFn);
static P_SCM_OP_LT: &'static ScmFn = &(scm_op_lt as ScmFn);
static P_SCM_OP_LTEQ: &'static ScmFn = &(scm_op_lteq as ScmFn);
static P_SCM_PUSH: &'static ScmFn = &(scm_push as ScmFn);
static P_SCM_QUOTE: &'static ScmFn = &(scm_quote as ScmFn);
static P_SCM_REMAINDER: &'static ScmFn = &(scm_remainder as ScmFn);
static P_SCM_SET_BINDING_TO_VALUE: &'static ScmFn = &(scm_set_binding_to_value as ScmFn);
static P_SCM_SUB: &'static ScmFn = &(scm_sub as ScmFn);
static P_SCM_VAR_STACK: &'static ScmFn = &(scm_var_stack as ScmFn);

pub fn standard_library() -> (Vec<(&'static str, &'static ScmFn)>, Vec<(&'static str, SEle)>) {
	// Scheme Standard library
	// TODO: any other way than using len to measure num of args? Unefficient with List
	let std_procs = vec![
		("+", P_SCM_ADD),
		("append", P_SCM_APPEND),
		("begin", P_SCM_BEGIN),
		("car", P_SCM_CAR),
		("cdr", P_SCM_CDR),
		("cond", P_SCM_COND),
		("define", P_SCM_DEFINE),
		("display", P_SCM_DISPLAY),
		("/", P_SCM_DIV),
		("eqv?", P_SCM_EQV_Q),
		("lambda", P_SCM_LAMBDA),
		("list", P_SCM_LIST),
		("list*", P_SCM_LIST_STAR),
		("*", P_SCM_MUL),
		("newline", P_SCM_NEWLINE),
		("not", P_SCM_NOT),
		("number?", P_SCM_NUMBER_Q),
		("=", P_SCM_OP_EQ),
		(">", P_SCM_OP_GT),
		(">=", P_SCM_OP_GTEQ),
		("<", P_SCM_OP_LT),
		("<=", P_SCM_OP_LTEQ),
		("push", P_SCM_PUSH),
		("quote", P_SCM_QUOTE),
		("remainder", P_SCM_REMAINDER),
		("set-binding-to-value", P_SCM_SET_BINDING_TO_VALUE),
		("-", P_SCM_SUB),
		("var-stack", P_SCM_VAR_STACK),
	];

	let std_vars = vec![
		("PI", SNum(PI)),
	];

	(std_procs, std_vars)
}