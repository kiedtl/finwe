// This file should be less than 500 loc in length (not
// including the standard library).

use std::collections::HashMap;
use std::fmt::{self, Display};
use std::io::{self, Read};
use std::rc::Rc;

mod stdlib;

const NONSYMB: [char; 16] = [ '{', '}', '(', ')', '[', ']',
    '"', '#', '|', '\\', ' ', '\t', '\n', '\r', '@', '!' ];

#[derive(Clone, Debug)]
pub enum ZfToken {
    List(Vec<ZfToken>),
    Number(f64),
    String(String),
    Symbol(String),
    Fetch(String),
    Store(String),
}

impl Display for ZfToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ZfToken::List(s) => {
                if s.len() == 0 {
                    return write!(f, "<list>");
                }

                write!(f, "<list {}", s[0])?;
                if s.len() > 1 {
                    for token in &s[1..] {
                        write!(f, ", {}", token)?;
                    }
                }
                write!(f, ">")
            },
            ZfToken::Number(n) => write!(f, "{}", n),
            ZfToken::String(s) => write!(f, "{:?}", s),
            ZfToken::Symbol(s) => write!(f, "<symb {}>", s),
            ZfToken::Fetch(s)  => write!(f, "<fetch {}>", s),
            ZfToken::Store(s)  => write!(f, "<store {}>", s),
        }
    }
}

impl Into<bool> for ZfToken {
    fn into(self) -> bool {
        match self {
            ZfToken::List(l) => if l.len() == 0 { false } else { true },
            ZfToken::Number(n) => if n == 0_f64 { false } else { true },
            _ => true,
        }
    }
}

fn parse(input: &str) -> Result<(usize, Vec<ZfToken>), ()> {
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
                let res = parse(&input[i + 1..])?;
                toks.push(ZfToken::List(res.1));
                i += res.0 + 2; // move past matching ']'
            },
            ']' => return Ok((i, toks)),

            // syntactic sugar
            '#' if chs.len() > i && !chs[i + 1].is_whitespace() => {
                toks.push(ZfToken::Number(chs[i + 1] as u32 as f64));
                i += 1;
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
                    Err(_) => toks.push(ZfToken::Symbol(n.0)),
                };
            },
        }
    }

    return Ok((i, toks));
}

type ZfProcFunc = dyn Fn(&mut ZfEnv) -> Result<(), &str>;

#[derive(Clone)]
pub enum ZfProc {
    Builtin(Rc<Box<ZfProcFunc>>),
    User(Vec<ZfToken>),
}

impl ZfProc {
    pub fn exec<'a>(&self, env: &'a mut ZfEnv) -> Result<(), &'a str> {
        match self {
            ZfProc::Builtin(b) => (b)(env)?,
            ZfProc::User(u) => run(&u, env),
        };

        Ok(())
    }
}


#[derive(Clone)]
pub struct ZfEnv {
    pile: Vec<ZfToken>,
    dict: HashMap<String, ZfProc>,
    vars: HashMap<String, ZfToken>,
}

impl ZfEnv {
    pub fn new() -> ZfEnv {
        ZfEnv {
            pile: Vec::new(),
            dict: HashMap::new(),
            vars: HashMap::new(),
        }
    }
}

fn run(code: &[ZfToken], env: &mut ZfEnv) {
    for token in code {
        match token {
            ZfToken::Symbol(s) => {
                if !env.dict.contains_key(s) {
                    eprintln!("I don't know what {} is.", s);
                    return
                }

                match env.dict[s].clone().exec(env) {
                    Ok(()) => (),
                    Err(s) => { eprintln!("{}", s); return },
                }
            },
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
    builtin!("proc",    stdlib::PROC);
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
    builtin!("while",  stdlib::WHILE);
    builtin!(".",     stdlib::io_TOS);
    builtin!("cr",     stdlib::io_CR);
    builtin!(".S",       stdlib::DBG);

    macro_rules! include_zf {
        ($path:expr) =>
            (std::str::from_utf8(include_bytes!($path)).unwrap())
    }

    let stdlib_builtin = include_zf!("std/builtin.zf");
    run(&parse(stdlib_builtin).unwrap().1, &mut env);

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let parsed = parse(&buffer);

    if parsed.is_ok() {
        run(&parsed.unwrap().1, &mut env);
    } else {
        eprintln!("syntax error");
    }
}
