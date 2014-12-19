use lib::{List, scheme_alert};
use lib::SEle::*;
use lib::ScmAlert;
use lib::ScmAlertMode;

pub fn split_source_text(src: &str) -> Vec<&str> {
	let mut ss = Vec::with_capacity(src.len() / 4);
	let mut start_pos = 0;
	let (mut in_string, mut in_escape, mut in_comment) = (false,false,false);
	let mut in_space = true;
	let mut char_iter = src.char_indices();
	loop {
		// Manual iteration in case we have to skip an iteration sometime
		let (i, c) = match char_iter.next() {
			Some(p) => p,
			None => break
		};

		// First pass: Check for comment
		match c {
			'\n' => in_comment = false,
			';' if !in_string && !in_escape => in_comment = true,
			_ => ()
		}
		if in_comment {
			start_pos = i+1;
			continue
		}
		// Second pass: General splitting
		match c {
			' ' => {
				if !in_string {
					if !in_space && start_pos != i{
						ss.push(src.slice(start_pos, i));
					}
					start_pos = i+1;
				}
				if in_escape { in_escape = false; }
			},
			'\r' | '\n' => {
				if !in_string {
					if !in_space && start_pos != i{
						ss.push(src.slice(start_pos, i));
					}
					// Handle \r\n line endings in case of windows
					if c == '\r' {
						start_pos = i + 2;
						char_iter.next();
					} else {
						start_pos = i + 1;
					}
				}
				if in_escape { in_escape = false; }
			},
			'"' if !in_escape => {
				if in_string {
					ss.push(src.slice(start_pos, i+1));
					start_pos = i+1;
				} else {
					start_pos = i;
				}
				in_string = !in_string;
			},
			'\\' => if !in_escape { in_escape = true; },
			'(' | ')' if !in_string && !in_escape => {
				if !in_space && start_pos != i {
					ss.push(src.slice(start_pos, i));
				}
				ss.push(src.slice(i, i+1));
				start_pos = i+1;
			},
			_ => (),
		}
		// Third pass: Check for space
		match c {
			' ' | '\n' => in_space = true,
			_ => in_space = false
		}
	}
	ss
}

/// Index the matching closing parenthesis in `v` for the opening parenthesis
/// preceding the slice
fn matching_paren(v: &[&str]) -> Option<uint> {
	let mut unclosed_parens: i32 = 0;
	for (i,s) in v.iter().enumerate() {
		if *s == ")" {
			if unclosed_parens == 0 {
				return Some(i);
			}
			unclosed_parens -= 1;
		} else if *s == "(" {
			unclosed_parens += 1;
		}
	}
	None
}

fn parse_binding(s: &str) -> Option<String> {
	Some(s.to_string())
}

fn parse_number(s: &str) -> Option<f64> {
	from_str(s)
}

fn parse_str_literal(s: &str) -> Option<&str> {
	if s.starts_with("\"") && s.ends_with("\"") {
		Some(s.slice(1, s.len()-1))
	} else {
		None
	}
}

fn parse_symbol(s: &str) -> Option<&str> {
	if s.starts_with("'") {
		Some(s.slice_from(1))
	} else {
		None
	}
}

fn parse_bool(s: &str) -> Option<bool> {
	if s == "#t" {
		Some(true)
	} else if s == "#f" {
		Some(false)
	} else {
		None
	}
}

/// Takes a slice of separated symbols, each being a parenthesis, a bool literal, a numeric literal
/// or whatever. These tokens are parsed into `SEle`'s so that they can be evaluated as expressions.
pub fn parse_expressions(unparsed: &[&str]) -> List {
	let mut parsed = List::new();
	let mut i = 0;
	while i < unparsed.len() {
		let current_s = unparsed[i];
		if current_s == "(" {
			if let Some(matching_paren) = matching_paren(unparsed.slice_from(i+1)) {
				let exprs_to_parse = unparsed.slice(i+1, i+1+matching_paren);
				parsed.push(SExpr(parse_expressions(exprs_to_parse)));
				i = i+matching_paren+1;
			} else {
				scheme_alert(ScmAlert::Unclosed, &ScmAlertMode::Error);
			}
		} else if current_s == "'" && i+1 < unparsed.len() && unparsed[i+1] == "(" {
			i += 1;
			if let Some(matching_paren) = matching_paren(unparsed.slice_from(i+1)) {
				let list_item_to_parse = unparsed.slice(i+1, i+1+matching_paren);
				let list = SExpr(parse_expressions(list_item_to_parse));
				// TODO: add macro to create Lists similar to vec![]
				let quoted = SExpr(List::from_vec(vec![SBinding("quote".to_string()),
							list]));
				parsed.push(quoted);
				i = i+matching_paren+1;
			} else {
				scheme_alert(ScmAlert::Unclosed, &ScmAlertMode::Error);
			}
		} else if let Some(f) = parse_number(current_s) {
			parsed.push(SNum(f));
		} else if let Some(s) = parse_str_literal(current_s) {
			parsed.push(SStr(s.to_string()));
		} else if let Some(s) = parse_symbol(current_s) {
			parsed.push(SSymbol(s.to_string()));
		} else if let Some(b) = parse_bool(current_s) {
			parsed.push(SBool(b));
		} else if let Some(b) = parse_binding(current_s) {
			parsed.push(SBinding(b));
		}
		i += 1;
	}
	parsed
}