// This file should be less than 500 loc in length (not
// including the standard library).

use std::process::exit;
use std::collections::HashMap;
use std::io::{self, Read};
use std::rc::Rc;

mod stdlib;
mod random;

const NONSYMB: [char; 16] = [ '{', '}', '(', ')', '[', ']',
    '"', '#', '|', '\\', ' ', '\t', '\n', '\r', '@', '!' ];

#[derive(Clone, Debug)]
pub enum ZfToken {
    Number(f64),
    String(String),
    Symbol(String),
    SymbRef(String),
    Fetch(String),
    Store(String),
    Return,
}

impl Into<bool> for &ZfToken {
    fn into(self) -> bool {
        match self {
            ZfToken::Number(n) => if *n == 0_f64 { false } else { true },
            _ => true,
        }
    }
}

fn parse(env: &mut ZfEnv, input: &str, in_def: bool)
    -> Result<(usize, Vec<ZfToken>), ()>
{
    fn eat<F>(ch: &[char], mut c: usize, until: F)
        -> (String, usize, bool) where F: Fn(&[char]) -> bool
    {
        let mut buf = String::new();
        let mut early_return = true;

        while c < ch.len() {
            if until(&ch[c..]) {
                early_return = false;
                break;
            } else {
                buf += &format!("{}", ch[c]);
                c += 1;
            }
        }

        (buf, c, early_return)
    }

    let mut toks = Vec::new();
    let chs = input.chars()
        .collect::<Vec<char>>();
    let mut i = 0;

    while i < chs.len() {
        match chs[i] {
            // --- whitespace ---
            ' ' | '\t' | '\r' | '\n' => { i += 1; continue; },

            // --- comments ---
            '(' => {
                let s = eat(&chs, i + 1, |c| c[0] == ')');
                if s.2 { return Err(()); }
                i = s.1 + 2;
            },
            '\\' => {
                let s = eat(&chs, i + 2, |c| c[0] == '\n');
                i = s.1 + 1;
            },

            // --- strings ---
            '"' => {
                let s = eat(&chs, i + 1, |c| c[0] == '"');
                if s.2 { return Err(()); }
                i = s.1 + 1;
                toks.push(ZfToken::String(s.0));
            },

            // --- quotes ---
            '[' => {
                let res = parse(env, &input[i + 1..], false)?;
                let name = random::phrase();
                env.dict.insert(name.clone(), ZfProc::User(res.1));
                toks.push(ZfToken::SymbRef(name));
                i += res.0 + 1;
            },
            ']' => { i += 1; return Ok((i, toks)) },

            ':' if !in_def => {
                i = eat(&chs, i + 1, |c| c[0].is_whitespace()).1;
                let name = eat(&chs,  i + 1, |c| NONSYMB.contains(&c[0]));
                i = name.1;
                let body = parse(env, &input[i + 1..], true)?;
                env.dict.insert(name.0, ZfProc::User(body.1));
                i += body.0 + 1;
            },
            ';' if in_def  => { i += 1; return Ok((i, toks)) },
            ':' if in_def  => return Err(()),
            ';' if !in_def => return Err(()),

            // syntactic sugar
            '$' if chs.len() > i && !chs[i + 1].is_whitespace() => {
                toks.push(ZfToken::Number(chs[i + 1] as u32 as f64));
                i += 2;
            },
            '\'' => {
                let s = eat(&chs, i + 1, |c| c[0].is_whitespace());
                toks.push(ZfToken::String(s.0));
                i = s.1;
            },
            '!' if chs.len() > i && !chs[i + 1].is_whitespace() => {
                let n = eat(&chs, i + 1, |c| NONSYMB.contains(&c[0]));
                i = n.1;
                toks.push(ZfToken::Store(n.0));
            },
            '@' if chs.len() > i && !chs[i + 1].is_whitespace() => {
                let n = eat(&chs, i + 1, |c| NONSYMB.contains(&c[0]));
                i = n.1;
                toks.push(ZfToken::Fetch(n.0));
            },

            _ => {
                let n = eat(&chs, i, |c| NONSYMB.contains(&c[0]));

                i = n.1;
                match n.0.replace("_", "").parse::<f64>() {
                    Ok(o) =>  toks.push(ZfToken::Number(o)),
                    Err(_) => {
                        if n.0 == "ret" {
                            toks.push(ZfToken::Return);
                        } else {
                            toks.push(ZfToken::Symbol(n.0));
                        }
                    },
                };
            },
        }
    }

    return Ok((i, toks));
}

type ZfProcFunc = dyn Fn(&mut ZfEnv) -> Result<(), String>;

#[derive(Clone)]
pub enum ZfProc {
    Builtin(Rc<Box<ZfProcFunc>>),
    User(Vec<ZfToken>),
}

#[derive(Clone, Default)]
pub struct ZfEnv {
    pile: Vec<ZfToken>,
    dict: HashMap<String, ZfProc>,
    vars: HashMap<String, ZfToken>,
}

impl ZfEnv {
    pub fn new() -> ZfEnv { Default::default() }

    pub fn call(&mut self, proc: &ZfProc) -> Result<(), String> {
        match proc {
            ZfProc::Builtin(b) => (b)(self)?,
            ZfProc::User(u) => run(&u, self),
        };

        Ok(())
    }
}

fn run(code: &[ZfToken], env: &mut ZfEnv) {
    for token in code {
        match token {
            ZfToken::Fetch(var) => {
                if env.vars.contains_key(var) {
                    env.pile.push(env.vars[var].clone());
                } else {
                    eprintln!("unknown variable {}", var);
                    exit(1);
                }
            },
            ZfToken::Store(var) => {
                env.vars.insert(var.clone(), match env.pile.pop() {
                    Some(e) => e,
                    None => { eprintln!("stack underflow"); exit(1) },
                });
            },
            ZfToken::Symbol(s) => {
                if !env.dict.contains_key(s) {
                    eprintln!("I don't know what {} is.", s);
                    exit(1);
                }

                match env.call(&env.dict[s].clone()) {
                    Ok(()) => (),
                    Err(s) => { eprintln!("{}", s); exit(1); },
                }
            },
            ZfToken::Return => return,
            _ => env.pile.push(token.clone()),
        }
    }
}

fn main() {
    let mut env = ZfEnv::new();

    macro_rules! builtin {
        ($s:expr, $x:path) =>
            (env.dict.insert($s.to_string(),
                ZfProc::Builtin(Rc::new(Box::new($x)))))
    }

    builtin!("if",        stdlib::IF);
    builtin!("depth",  stdlib::DEPTH);
    builtin!("pick",    stdlib::PICK);
    builtin!("roll",    stdlib::ROLL);
    builtin!("drop",    stdlib::DROP);
    builtin!("not",      stdlib::NOT);
    builtin!("cmp",      stdlib::CMP);
    builtin!("+",       stdlib::PLUS);
    builtin!("-",        stdlib::SUB);
    builtin!("*",        stdlib::MUL);
    builtin!("/mod",    stdlib::DMOD);
    builtin!("b&",      stdlib::bAND);
    builtin!("b|",       stdlib::bOR);
    builtin!("b^",      stdlib::bXOR);
    builtin!("b!",      stdlib::bNOT);
    builtin!("<<",       stdlib::SHL);
    builtin!(">>",       stdlib::SHR);
    builtin!("until",  stdlib::UNTIL);
    builtin!("emit",    stdlib::EMIT);
    builtin!(".S",       stdlib::DBG);
    builtin!(".D",   stdlib::DICTDBG);

    macro_rules! include_zf {
        ($path:expr) =>
            (std::str::from_utf8(include_bytes!($path)).unwrap())
    }

    let stdlib_builtin = include_zf!("std/builtin.zf");
    let stdlib_parsed  = parse(&mut env, stdlib_builtin, false);
    run(&stdlib_parsed.unwrap().1, &mut env);

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let parsed = parse(&mut env, &buffer, false);

    if parsed.is_ok() {
        run(&parsed.unwrap().1, &mut env);
    } else {
        eprintln!("syntax error");
    }
}
