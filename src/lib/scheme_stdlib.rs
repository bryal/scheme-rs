use std::f64::consts::PI;
use std::f64::NAN;

use super::{
	List,
	SEle,
	Env,
	ScmFn,
	Procedure,
	ProcOrFunc,
	ScmAlert,
	scheme_alert,
	unit};
use super::SEle::*;

// TODO: instead of doing env.eval_expr, the direct use of the scm_* function can often be used
// e.g. scm_define(env, list)
// TODO: Make the arithmetic functions iterative rather than recursive. They are recursive to
// imitate scheme, but it is probably not optimal for performance

fn scm_add_iter(env: &mut Env, sum: f64, mut terms: List) -> f64 {
	if let Some(expr) = terms.pop_head() {
		match env.elem_value(expr) {
			SNum(x) => scm_add_iter(env, sum + x, terms),
			x => {
				scheme_alert(ScmAlert::NaN(x), &env.error_mode);
				NAN
			}
		}
	} else {
		sum
	}
}
fn scm_add(env: &mut Env, args: List) -> SEle {
	SNum(scm_add_iter(env, 0.0, args))
}

fn scm_sub_iter(env: &mut Env, diff: f64, mut terms: List) -> f64 {
	if let Some(expr) = terms.pop_head() {
		match env.elem_value(expr) {
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
		if let SNum(x) = env.elem_value(expr) {
			scm_sub_iter(env, x, args)
		} else {
			panic!("NaN")
		}
	} else {
		0.0
	})
}

fn scm_div_iter(env: &mut Env, numerator: f64, mut denoms: List) -> f64 {
	if let Some(expr) = denoms.pop_head() {
		match env.elem_value(expr) {
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
		if let SNum(x) = env.elem_value(expr) {
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

fn scm_remainder(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 2 {
		scheme_alert(ScmAlert::ArityMiss("remainder", args.len(),"!=",2), &env.error_mode)
	} else {
		let e1 = env.elem_value(args.pop_head().unwrap());
		let e2 = env.elem_value(args.pop_head().unwrap());
		match (e1, e2) {
			(SNum(n1), SNum(n2)) => SNum(n1 % n2),
			(t1, t2) => {
				scheme_alert(ScmAlert::WrongType("Number, Number", format!("{}, {}",
							t1.variant(), t2.variant()).as_slice()),
					&env.error_mode);
				unit()
			}
		}
	}
}

fn scm_lambda(env: &mut Env, mut args: List) -> SEle {
	if args.len() < 2 {
		scheme_alert(ScmAlert::ArityMiss("lambda", args.len(),"<",2), &env.error_mode)
	} else {
		let wrapped_body = SExpr(List::with_body(SBinding("begin".to_string()),
				args.pop_tail().unwrap()));
		match args.pop_head().unwrap() {
			SBinding(binding) => {
				let procedure = Procedure::new_variadic(binding, wrapped_body);
				SProc(box ProcOrFunc::Proc(procedure))
			},
			SExpr(lambda_bindings) => {
				let lambda_arg_names = lambda_bindings.into_iter().map(|e|
						if let SBinding(s) = e { s }
						else { "".to_string() /* This is ugly :/ */ }
					).collect();
				let procedure = Procedure::new(lambda_arg_names, wrapped_body);
				SProc(box ProcOrFunc::Proc(procedure))
			},
			_ => scheme_alert(ScmAlert::Bad("lambda body"), &env.error_mode),
		}
	}
}

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
					env.define_var(binding, args.pop_head().unwrap());
				},
			SExpr(head) => {
				// TODO: recursive definition of expression as var binding to lambda
				let wrapped_body = SExpr(List::with_body(SBinding(
							"begin".to_string()), args));
				let (proc_name, arg_names) = env.dismantle_proc_head(head);
				let procedure = box ProcOrFunc::Proc(Procedure::new(arg_names,
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
	println!("{}", env.elem_value(args.pop_head().expect(
		"Could not retrieve argument")));
	unit()
}

fn scm_begin(env: &mut Env, args: List) -> SEle {
	env.eval_block(args)
}

fn scm_eqv(env: &mut Env, mut args: List) -> SEle {
	if args.len() == 0 || args.len() == 1 {
		SBool(true)
	} else {
		let e1 = args.pop_head().unwrap();
		let v1 = env.elem_value(e1);
		for e2 in args.into_iter() {
			if v1 != env.elem_value(e2) {
				return SBool(false);
			}
		}
		SBool(true)
	}
}

fn scm_number(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 1 {
		scheme_alert(ScmAlert::ArityMiss("numbers?", args.len(),"!=",1),
			&env.error_mode);
		unit()
	} else {
		if let SNum(_) = env.elem_value(args.pop_head().unwrap()) {
			SBool(true)
		} else {
			SBool(false)
		}
	}
}

fn scm_eq_sign(env: &mut Env, mut args: List) -> SEle {
	// TODO: use create macro to artificially construct scheme expression
	if args.clone().into_iter().all(|e| {
			let expr = SExpr(List::from_vec(vec![
				SBinding("number?".to_string()), e]));
			env.elem_is_true(expr)
	})
	{
		// TODO: use List.prepush()?
		args = List::with_body(SBinding("eqv?".to_string()), args);
		SBool(env.elem_is_true(SExpr(args)))
	} else {
		scheme_alert(ScmAlert::WrongType("Number", "_"), &env.error_mode);
		unit()
	}
}

fn scm_cond(env: &mut Env, args: List) -> SEle {
	if args.len() < 2 {
		scheme_alert(ScmAlert::ArityMiss("cond", args.len(),"<",2), &env.error_mode);
		return unit();
	}
	for clause in args.into_iter() {
		if let SExpr(mut clause_v) = clause {
			if clause_v.len() != 2 {
				scheme_alert(ScmAlert::Bad("cond clause"), &env.error_mode);
				return unit();
			}
			let test = clause_v.pop_head().unwrap();
			if let SBinding(maybe_else) = test {
				if maybe_else.as_slice() == "else" {
					return clause_v.pop_head().unwrap();
				}
			} else {
				let test_val = env.elem_value(test);
				if match test_val { SBool(b) => b, _ => true } {
					return env.elem_value(clause_v.pop_head()
							.unwrap());
				}
			}
		} else {
			scheme_alert(ScmAlert::Bad("cond clause"), &env.error_mode);
			return unit();
		}
	}
	unit()
}

fn scm_if(env: &mut Env, mut args: List) -> SEle {
	if args.len() != 3  {
		scheme_alert(ScmAlert::ArityMiss("if", args.len(),"!=",3), &env.error_mode);
		unit()
	} else {
		let condition = args.pop_head().unwrap();
		let consequense = args.pop_head().unwrap();
		let alternative = args.pop_head().unwrap();
		let scm_else = SBinding("else".to_string());
		let cond = SExpr(List::from_vec(vec![
			SBinding("cond".to_string()),
			SExpr(List::from_vec(vec![condition, consequense])),
			SExpr(List::from_vec(vec![scm_else, alternative]))
		]));
		env.elem_value(cond)
	}
}

#[allow(unused_variables)]
fn scm_var_stack(env: &mut Env, args: List) -> SEle {
	println!("var stack: {}", env.var_defs);
	unit()
}

#[allow(unused_variables)]
fn scm_newline(env: &mut Env, args: List) -> SEle {
	println!("");
	unit()
}


pub fn standard_library() -> (Vec<(&'static str, ScmFn)>, Vec<(&'static str, SEle)>) {
	// Scheme Standard library
	// TODO: any other way than using len to measure num of args? Unefficient with List
	let std_procs = vec![
		("+", scm_add),
		("-", scm_sub),
		("/", scm_div),
		("remainder", scm_remainder),
		("lambda", scm_lambda),
		("define", scm_define),
		("display", scm_display),
		("begin", scm_begin),
		("eqv?", scm_eqv),
		("number?", scm_number),
		("=", scm_eq_sign),
		("cond", scm_cond),
		("if", scm_if),
		("var-stack", scm_var_stack),
		("newline", scm_newline),
	];

	let std_vars = vec![
		("PI", SNum(PI)),
	];

	(std_procs, std_vars)
}