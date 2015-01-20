use lib::{
	List,
	SEle,
	scheme_alert};
use lib::SEle::*;
use lib::ScmAlert;
use lib::ScmAlertMode;

enum Token {
	OpenParen,
	CloseParen,
	Quote,
	Octothorpe,
	StrLit(String),
	Ident(&str), // Any non-delim char sequence not in a string
}

// TODO: Raw string literal. Maybe R"delim(raw text)delim"

fn char_is_number(c: char) -> bool {
	match c {
		'0'...'9' => true,
		_ => false
	}
}

fn escape_raw_str(s: &str) -> String {
	let mut v = Vec::with_capacity(s.len());
	for b in s.bytes() {
		let c = b as char;
		match c {
			'\t' | '\r' | '\n' | '"' | '\\' => for ch in c.escape_default() {
				v.push(ch as u8)
			},
			_ => v.push(b),
		}
	}
	String::from_utf8(v).unwrap()
}

struct Parser {
	in_str_lit: bool,
	in_raw_str_lit: bool,
	comp_str_start_line: usize,
	comp_str_lit: String
}
impl Parser {
	fn get_rest_of_str_lit(&self, src_line: &str) -> (Option<usize>, &str) {
		let p = if self.in_str_lit {
				str_lit_exit(src_line)
			} else {
				raw_str_lit_exit(src_line)
			};
		if p == 0 {
			(None, line_src[0..])
		} else {
			(Some(p), line_src[0..p])
		}
	}

	fn match_chars_to_tokens(&mut self, src_line: &str) -> Vec<Token> {
		let mut tokens = Vec::with_capacity(3);
		let mut char_iter = src_line.char_indices();
		while let (i, c) = char_iter.next() {
			match c {
				';' => break,
				' ' | '\t' => continue,
				'(' => tokens.push(Token::OpenParen),
				')' => tokens.push(Token::CloseParen),
				'\'' => tokens.push(Token::Quote),
				// '#' => tokens.push(Token::Octothorpe),
				'"' => match str_lit_exit(src_line[i+1..]) {
					None => {
						self.comp_str_lit = line_src[i+1..].to_string();
						self.in_str_lit = true;
						break
					},
					Some(p) => {
						tokens.push(Token::StrLit(
								line_src[i+1..p].to_string()));
						char_iter = src_line[p+1..].char_indices();
					}
				},
				'R' if src_line[i+1..i+4] == "\"_(" =>
					match raw_str_lit_exit(src_line[i+4..])
				{
					None => {
						self.comp_st_rlit = line_src[i..].to_string();
						self.in_str_lit = true;
						break
					},
					Some(p) => {
						tokens.push(Token::StrLit(
							escape_raw_str(line_src[i+4..p])));
						char_iter = src_line[p+4..].char_indices();
					}
				},
				x => {
					let p = ident_exit(src_line, i);
					tokens.push(Token::Ident(line_src[i..p]));
					char_iter = src_line[p+1..].char_indices();
				}
			}
		}
		tokens
	}

	fn tokenize_line(&mut self, mut src_line: &str) -> Option<Vec<Token>> {
		if in_str_lit || in_raw_str_lit {
			let (maybe_end, s) = self.get_rest_of_str_lit(src_line);
			self.comp_str_lit.push(s);
			if let Some(end_pos) = maybe_end {
				if end_pos == (src_line.len()-1) {
					return None;
				}
				src_line = src_line[i+1..].trim_left();
			} else {
				return None;
			}
		}

		Some(self.match_chars_to_tokens(src_line))
	}
	pub fn tokenize_source(&mut self, src: &str) -> Vec<(usize, Vec<Token>)> {
		let mut ret_lines = Vec::with_capacity(src.len() / 40);

		for (line_n, src_line) in src.lines().map(|l| l.trim()).enumerate()
			.filter(|(_, l)| !l.is_empty() && !l.begins_with(';'))
		{
			let maybe_tokenized = self.tokenize_line(src_line);
			if let Some(tokenized) = maybe_tokenized {
				if self.in_str_lit || self.in_raw_str_lit {
					(self.in_str_lit, self.in_raw_str_lit) = (false, false);
					ret_lines.push((comp_str_start_line, tokenized))
				} else {
					ret_lines.push((line_n, tokenized))
				}
			} else {
				continue
			}
		}

		ret_lines
	}
}

/// Index the matching closing parenthesis in `v` for the opening parenthesis
/// preceding the slice
fn matching_paren(v: &[Token]) -> Option<usize> {
	let mut unclosed_parens: i32 = 0;
	for (i, t) in v.iter().enumerate() {
		match t {
			Token::CloseParen => if unclosed_parens == 0 {
					return Some(i)
				} else {
					unclosed_parens -= 1
				},
			Token::OpenParen => unclosed_parens += 1,
			_ => ()
		}
	}
	None
}

fn parse_number(s: &str) -> Option<f64> {
	s.replace("_", "").parse()
}
fn parse_bool(s: &str) -> Option<bool> {
	match s {
		"#t" => Some(true),
		"#f" => Some(false),
		_ => None
	}
}
fn parse_binding(s: &str) -> Option<String> {
	Some(s.to_string())
}


/// Tokens are parsed to `SEle`'s so that they can be evaluated as expressions.
pub fn parse_tokens(unparsed: &[(usize, Token)]) -> List<SEle> {
	let mut parsed = list![];
	let mut i = 0;
	let unparsed_len = unparsed.len();
	while i < unparsed_len {
		let (line_n, current_token) = unparsed[i];
		match *current_token {
			Token::OpenParen =>
				if let Some(mp) = matching_paren(&unparsed[i+1..]) {
					parsed.push(SExpr(parse_tokens(&unparsed[i+1..i+1+mp])));
					i+= mp+1;
				} else {
					scheme_alert(ScmAlert::Unclosed(line_n),
						&ScmAlertMode::Error);
				},
			Token::Quote if unparsed[i+1] == Token::OpenParen && i+1 < unparsed_len => {
					i += 1;
					if let Some(mp) = matching_paren(&unparsed[i+1..]) {
						let expr = list![
							SBinding("quote".to_string()),
							SExpr(parse_tokens(
									&unparsed[i+1..i+1+mp]))];
						parsed.push(SExpr(expr));
						i += mp+1;
					} else {
						scheme_alert(ScmAlert::Unclosed(line_n),
							&ScmAlertMode::Error);
					}
				},
			Token::Quote => if let Token::Ident(s) = unparsed[i+1] {
					parsed.push(SSymbol(s.to_string()));
					i += 1;
				},
			StrLit(ref s) => parsed.push(SBinding(s.clone())),
			Ident(s) => if let Some(f) = parse_number(s) {
					parsed.push(SNum(f));
				} else if let Some(b) = parse_bool(s) {
					parsed.push(SBool(b));
				} else {
					parsed.push(SBinding(s.to_string()));
				},
			_ => ()
		}
		i += 1;
	}
	parsed
}