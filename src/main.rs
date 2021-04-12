// The goal is for this project to be less than 800 loc in length (not including
// std/builtin.zf and tests).

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest;

use std::collections::HashMap;
use std::io::{self, Read};
use std::rc::Rc;

#[macro_use]
mod utils;

mod ratios;
mod floats;
mod parser;
mod errors;
mod stdlib;
mod random;

pub const DEFAULT_STACK: &'static str = "_";

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GuardItem {
    Any, Number, Str, Quote,
    Unchecked
}

#[derive(Clone, Debug)]
pub enum ZfToken {
    Nop,
    Number(f64),
    String(String),
    Symbol(usize),
    SymbRef(usize),
    Switch(String),
    Stack(String),
    Fetch(String),
    Store(String),
    Table(HashMap<ZfToken, ZfToken>),
    CJump(isize),
    ZJump(isize),
    UJump(isize),

    Guard {
        before: Vec<GuardItem>,
        after:  Vec<GuardItem>
    },

    // Only used during parsing.
    Ident(String),
    Continue,
    Break,
}

impl ZfToken {
    fn fmt(&self, e: &ZfEnv) -> String {
        match self {
            ZfToken::Nop        => format!("<nop>"),
            ZfToken::Number(i)  => format!("{}", i),
            ZfToken::String(s)  => format!("{:?}", s),
            ZfToken::Symbol(s)  => format!("<symb {}>", e.dict[*s].0),
            ZfToken::SymbRef(s) => format!("<ref {}>",  e.dict[*s].0),
            ZfToken::Stack(s)   => format!("<stack {}>", s),
            ZfToken::Switch(s)  => format!("<sw {}>", s),
            ZfToken::Fetch(s)   => format!("<fetch {}>", s),
            ZfToken::Store(s)   => format!("<store {}>", s),
            ZfToken::Table(t)   => format!("{:?}", t),
            ZfToken::Ident(i)   => format!("<ident {}>", i),
            ZfToken::CJump(i)   => format!("<?jmp {}>", i),
            ZfToken::ZJump(i)   => format!("<zjmp {}>", i),
            ZfToken::UJump(i)   => format!("<ujmp {}>", i),
            ZfToken::Continue   => format!("<continue>"),
            ZfToken::Break      => format!("<break>"),

            ZfToken::Guard { before: _, after: _ }
                => format!("<guard {:?}>", self),
        }
    }
}

impl Eq for ZfToken {}

impl PartialEq for ZfToken {
    fn eq(&self, rhs: &Self) -> bool {
        use ZfToken::*;

        match (self, rhs) {
            (Number(l),   Number(r)) => l.to_bits() == r.to_bits(),
            (String(l),   String(r)) => l == r,
            (Symbol(l),   Symbol(r)) => l == r,
            (SymbRef(l), SymbRef(r)) => l == r,
            (Fetch(l),     Fetch(r)) => l == r,
            (Store(l),     Store(r)) => l == r,
            (Table(l),     Table(r)) => {
                if l.len() != r.len() { return false; }
                for (k, v) in l {
                    if !r.contains_key(k) || &r[k] != v { return false; }
                }
                for (k, v) in r {
                    if !l.contains_key(k) || &r[k] != v { return false; }
                }

                true
            },
            (Guard{before: lb, after: la},
                Guard{before: rb, after: ra}) => lb == rb && la == ra,
            _ => false,
        }
    }
}

impl std::hash::Hash for ZfToken {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ZfToken::Number(i) => i.to_bits().hash(state),
            ZfToken::String(s) => s.hash(state),
            ZfToken::SymbRef(s) => s.hash(state),
            ZfToken::Table(t) => for (k, v) in t {
                k.hash(state);
                v.hash(state);
            },

            // This method will only be called when using tables in Zf code.
            // Fetch/Store/Guard/Ident/Nop cannot be put into a table, so this
            // path should never be chosen.
            _ => unreachable!(),
        }
    }
}

impl Into<bool> for &ZfToken {
    fn into(self) -> bool {
        match self {
            ZfToken::Number(n) => if *n == 0_f64 { false } else { true },
            _ => true,
        }
    }
}

// The returned bool tells calling code whether the instruction pointer or
// return stack was modified. If it was not, the calling code will know
// it's safe to increment the IP
type ZfProcFunc = dyn Fn(&mut ZfEnv) -> Result<bool, String>;

#[derive(Clone)]
pub enum ZfProc {
    Builtin(Rc<Box<ZfProcFunc>>),
    User(Vec<ZfToken>),
}

#[derive(Clone)]
pub struct ZfRsFrame {
    dictid: usize,
    ip: usize,
}

#[derive(Clone)]
pub struct ZfEnv {
    vars:  HashMap<String, ZfToken>,
    piles: HashMap<String, Vec<ZfToken>>,
    current: String,
    dict:  Vec<(String, ZfProc)>,
    rs:    Vec<ZfRsFrame>,
}

impl ZfEnv {
    pub fn new() -> ZfEnv {
        ZfEnv {
            vars: HashMap::new(),
            piles: HashMap::new(),
            current: DEFAULT_STACK.to_owned(),
            dict: Vec::new(),
            rs: Vec::new(),
        }
    }

    pub fn stack<'a>(&'a mut self, name: &str) -> &'a mut Vec<ZfToken> {
        if !self.piles.contains_key(name) {
            self.piles.insert(name.to_owned(), Vec::new());
        }

        self.piles.get_mut(name).unwrap().as_mut()
    }

    pub fn cur_stack(&mut self) -> &mut Vec<ZfToken> {
        let cur = &self.current.clone();
        self.stack(cur)
    }

    pub fn push_to(&mut self, stack: &str, item: ZfToken) {
        self.stack(stack).push(item);
    }

    pub fn push(&mut self, item: ZfToken) {
        let cur = &self.current.clone();
        self.push_to(cur, item);
    }

    pub fn peek_from<'a>(&'a mut self, stack: &str) -> Result<&'a ZfToken, String> {
        let stack = self.stack(stack);
        let len = stack.len();

        match len {
            0 => Err(format!("stack underflow")),
            _ => Ok(&stack[len - 1])
        }
    }

    pub fn peek<'a>(&'a mut self) -> Result<&'a ZfToken, String> {
        let cur = &self.current.clone();
        self.peek_from(cur)
    }

    pub fn pop_from(&mut self, stack: &str) -> Result<ZfToken, String> {
        match self.stack(stack).pop() {
            Some(e) => Ok(e),
            None => Err(format!("stack underflow")),
        }
    }

    pub fn pop(&mut self) -> Result<ZfToken, String> {
        let cur = &self.current.clone();
        self.pop_from(cur)
    }

    pub fn findword(&self, name: &str) -> Option<usize> {
        for i in 0..self.dict.len() {
            if self.dict[i].0 == name {
                return Some(i);
            }
        }
        None
    }

    pub fn addword(&mut self, name: String, body: Vec<ZfToken>) -> usize {
        match self.findword(&name) {
            Some(i) => self.dict[i].1 = ZfProc::User(body),
            None => self.dict.push((name.clone(), ZfProc::User(body))),
        }
        self.findword(&name).unwrap()
    }

    pub fn pushrs(&mut self, funcid: usize, iptr: usize) {
        self.rs.push(ZfRsFrame {
            dictid: funcid,
            ip: iptr,
        });
    }

    pub fn incip(&mut self) {
        let crs = self.rs.len() - 1;
        self.rs[crs].ip += 1;
    }
}

fn run(code: Vec<ZfToken>, env: &mut ZfEnv) -> Result<(), String> {
    let main = env.addword("main".to_owned(), code);

    env.pushrs(main, 0);

    loop {
        if env.rs.len() == 0 { break }

        let crs = env.rs.len() - 1;
        let (c_ib, ip) = (env.rs[crs].dictid, env.rs[crs].ip);

        let ib;
        match env.dict[c_ib].1.clone() {
            ZfProc::User(u) => ib = u,
            ZfProc::Builtin(b) => {
                // Pop a stack frame (builtin words assume that they're on the
                // frame of their caller).
                env.rs.pop();

                // Execute the inbuilt word and continue.
                match (b)(env) {
                    Ok(co) => {
                        if !co { env.incip(); }
                        continue;
                    },
                    Err(e) => return Err(e),
                }
            },
        }

        if ip >= ib.len() {
            env.rs.pop();
            if env.rs.len() > 0 {
                env.incip();
            }
            continue;
        }

        match &ib[ip] {
            ZfToken::Nop => (),

            ZfToken::Symbol(s) => {
                env.pushrs(*s, 0);
                continue; // don't increment IP below
            },
            ZfToken::SymbRef(i) => env.push(ZfToken::Symbol(*i)),
            ZfToken::Fetch(var) => if env.vars.contains_key(var) {
                env.push(env.vars[var].clone());
            } else {
                return Err(format!("unknown variable {}", var))
            },
            ZfToken::Store(var) => {
                let i = env.pop()?;
                env.vars.insert(var.clone(), i);
            },
            ZfToken::CJump(i) => {
                let item = env.pop()?;
                if Into::<bool>::into(&item) {
                    env.rs[crs].ip = (env.rs[crs].ip as isize + i) as usize;
                    continue;
                }
            },
            ZfToken::ZJump(i) => {
                let item = env.pop()?;
                if !Into::<bool>::into(&item) {
                    env.rs[crs].ip = (env.rs[crs].ip as isize + i) as usize;
                    continue;
                }
            },
            ZfToken::UJump(i) => {
                env.rs[crs].ip = (env.rs[crs].ip as isize + i) as usize;
                continue;
            }
            ZfToken::Switch(s) => env.current = s.clone(),
            ZfToken::Guard { before: _b, after: _a } => (), // TODO
            _ => env.push(ib[ip].clone()),
        }

        env.incip();
    }

    Ok(())
}

fn main() {
    let mut env = ZfEnv::new();

    macro_rules! keyword {
        ($s:expr, $x:ident) =>
            (env.dict.push(($s.to_string(),
                ZfProc::Builtin(Rc::new(Box::new(stdlib::$x))))))
    }

    keyword!("?do",          IF);
    keyword!("ret",         RET);
    keyword!("?ret",       CRET);
    keyword!("depth",     DEPTH);
    keyword!("arrange", ARRANGE);
    keyword!("pick",       PICK);
    keyword!("roll",       ROLL);
    keyword!("drop",       DROP);
    keyword!("not",         NOT);
    keyword!("cmp",         CMP);
    keyword!("+",          PLUS);
    keyword!("-",           SUB);
    keyword!("*",           MUL);
    keyword!("/mod",       DMOD);
    keyword!("and",        bAND);
    keyword!("or",          bOR);
    keyword!("xor",        bXOR);
    keyword!("bnot",       bNOT);
    keyword!("shl",         SHL);
    keyword!("shr",         SHR);
    keyword!("emit",       EMIT);
    keyword!("wait",       WAIT);
    keyword!("push",       PUSH);
    keyword!("pop",         POP);
    keyword!("<-",       S_PUSH);
    keyword!("<<-",   S_DUPPUSH);
    keyword!("->",        S_POP);
    keyword!("->>",    S_DUPPOP);
    keyword!(">-",       S_DROP);
    keyword!("dbg",         DBG);
    keyword!("ddbg",    DICTDBG);
    keyword!("ceil",       CEIL);
    keyword!("floor",     FLOOR);
    keyword!("atan",       ATAN);
    keyword!("logn",       LOGN);
    keyword!("pow",         POW);
    keyword!("_.f",        FFMT);
    keyword!("#",         TALLY);
    keyword!("&",            AT);

    macro_rules! include_zf {
        ($path:expr) => {
            run(parser::parse(&mut env,
                    std::str::from_utf8(include_bytes!($path)).unwrap()).unwrap(),
                &mut env).unwrap()
        }
    }

    include_zf!("std/builtin.zf");

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let parsed = match parser::parse(&mut env, &buffer) {
        Ok(tokens) => tokens,
        Err(error) => { eprintln!("{}", error); return; },
    };

    match run(parsed, &mut env) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("error: {}", e);
            errors::stacktrace(&mut env);
            std::process::exit(1);
        },
    }
}
