// This file should be less than 500 loc in length (not
// including the standard library).

use std::collections::HashMap;
use std::fmt::{self, Display};
use std::io::{self, Read};
use std::rc::Rc;

mod stdlib;

const NONSYMB: [char; 17] = [ '{', '}', '(', ')',
    '[', ']', '"', '#', '|', '\\', ' ', '\t', '\n', '\r', '@', '$', '!' ];

#[derive(Clone, Debug)]
pub enum Token {
    List(Vec<Token>),
    Number(f64),
    String(String),
    Symbol(String),
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Token::List(s) => {
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
            Token::Number(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "{:?}", s),
            Token::Symbol(s) => write!(f, "<symb {}>", s),
        }
    }
}

impl Into<bool> for Token {
    fn into(self) -> bool {
        match self {
            Token::String(_) | Token::Symbol(_) => true,
            Token::List(l) => if l.len() == 0 { false } else { true },
            Token::Number(n) => if n == 0_f64 { false } else { true },
        }
    }
}

fn parse(input: &str) -> Result<(usize, Vec<Token>), ()> {
    fn eat<F>(ch: &[char], mut c: usize, until: F)
        -> (String, usize, bool)
    where
        F: Fn(&[char]) -> bool
    {
        let mut buf = String::new();
        let early_return;

        loop {
            if c >= ch.len() {
                early_return = true;
                break;
            }

            if until(&ch[c..]) == true {
                early_return = false;
                break;
            }

            buf += &format!("{}", ch[c]);
            c += 1;
        }

        (buf, c, early_return)
    }

    let mut toks = Vec::new();
    let chs = input.chars()
        .collect::<Vec<char>>();
    let mut i = 0;

    while i < chs.len() {
        match chs[i] {
            ' ' | '\n' | '\t' => { i += 1; continue; },

            '(' => {
                let s = eat(&chs, i + 1, |c| c[0] == ')');
                if s.2 { return Err(()); }
                i = s.1 + 2;
            },
            '\\' => {
                let s = eat(&chs, i + 2, |c| c[0] == '\n');
                i = s.1 + 1;
            },
            '"' => {
                let s = eat(&chs, i + 1, |c| c[0] == '"');
                if s.2 { return Err(()); }
                i = s.1 + 1;
                toks.push(Token::String(s.0));
            },

            '[' => {
                let res = parse(&input[i + 1..])?;
                toks.push(Token::List(res.1));
                i += res.0 + 2; // move past matching ']'
            },
            ']' => return Ok((i, toks)),

            '#' if chs.len() > i => {
                toks.push(Token::Number(chs[i + 1] as u32 as f64));
                i += 1;
            },
            '@' if chs.len() > i => {
                let n = eat(&chs, i + 1, |c| NONSYMB.contains(&c[0]));
                toks.push(Token::String(n.0));
                toks.push(Token::Symbol("fetch".to_owned()));
                i = n.1;
            },
            '!' if chs.len() > i => {
                let n = eat(&chs, i + 1, |c| NONSYMB.contains(&c[0]));
                toks.push(Token::String(n.0));
                toks.push(Token::Symbol("store".to_owned()));
                i = n.1;
            },

            _ => {
                let mut n = eat(&chs, i, |c| NONSYMB.contains(&c[0]));

                i = n.1;
                n.0 = n.0.to_lowercase();

                match n.0.replace("_", "").parse::<f64>() {
                    Ok(o) =>  toks.push(Token::Number(o)),
                    Err(_) => toks.push(Token::Symbol(n.0)),
                };
            },
        }
    }

    return Ok((i, toks));
}

#[derive(Clone)]
pub enum ZfProc {
    Builtin(Rc<Box<dyn Fn(&mut ZfEnv)>>),
    User(Vec<Token>),
}

impl ZfProc {
    pub fn exec(&self, env: &mut ZfEnv) {
        match self {
            ZfProc::Builtin(b) => (b)(env),
            ZfProc::User(u) => run(&u, env),
        }
    }
}


#[derive(Clone)]
pub struct ZfEnv {
    pile: Vec<Token>,
    dict: HashMap<String, ZfProc>,
    vars: HashMap<String, Token>,
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

fn run(code: &[Token], env: &mut ZfEnv) {
    for token in code {
        match token {
            Token::Symbol(s) => {
                if !env.dict.contains_key(s) {
                    eprintln!("I don't know what {} is.", s);
                    return
                }

                env.dict[s].clone().exec(env);
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

    builtin!("proc",    stdlib::PROC);
    builtin!("tuck",    stdlib::TUCK);
    builtin!("dup",      stdlib::DUP);
    builtin!("drop",    stdlib::DROP);
    builtin!("mod",      stdlib::MOD);
    builtin!("while",  stdlib::WHILE);
    builtin!(".",     stdlib::io_TOS);
    builtin!("cr",     stdlib::io_CR);
    builtin!("store",  stdlib::STORE);
    builtin!("fetch",  stdlib::FETCH);
    builtin!("dbg",      stdlib::DBG);

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let parsed = parse(&buffer);

    if parsed.is_ok() {
        run(&parsed.unwrap().1, &mut env);
    } else {
        eprintln!("syntax error");
    }
}
