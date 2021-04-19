// The goal is for this project to be less than 800 loc in length (not including
// std/builtin.zf and tests).

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest;

use std::collections::HashMap;
use std::io::{self, Read};

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
    ZJump(isize),
    UJump(isize),

    Guard {
        before: Vec<GuardItem>,
        after:  Vec<GuardItem>
    },

    // Only used during parsing.
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
            // Fetch/Store/Guard/Nop cannot be put into a table, so this
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
type ZfProcFunc = &'static dyn Fn(&mut ZfEnv) -> Result<bool, String>;

#[derive(Clone)]
pub enum ZfProc {
    Builtin(ZfProcFunc),
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

    pub fn run(&mut self, word: usize) -> Result<(), String> {
        self.pushrs(word, 0);

        loop {
            if self.rs.len() == 0 { break }

            let crs = self.rs.len() - 1;
            let (c_ib, ip) = (self.rs[crs].dictid, self.rs[crs].ip);

            let ib;
            match self.dict[c_ib].1.clone() {
                ZfProc::User(u) => ib = u,
                ZfProc::Builtin(b) => {
                    // Pop a stack frame (builtin words assume that they're on the
                    // frame of their caller).
                    self.rs.pop();

                    // Execute the inbuilt word and continue.
                    match (b)(self) {
                        Ok(co) => {
                            if !co { self.incip(); }
                            continue;
                        },
                        Err(e) => return Err(e),
                    }
                },
            }

            if ip >= ib.len() {
                self.rs.pop();
                if self.rs.len() > 0 {
                    self.incip();
                }
                continue;
            }

            match &ib[ip] {
                ZfToken::Nop => (),

                ZfToken::Symbol(s) => {
                    self.pushrs(*s, 0);
                    continue; // don't increment IP below
                },
                ZfToken::SymbRef(i) => self.push(ZfToken::Symbol(*i)),
                ZfToken::Fetch(var) => if self.vars.contains_key(var) {
                    self.push(self.vars[var].clone());
                } else {
                    return Err(format!("unknown variable {}", var))
                },
                ZfToken::Store(var) => {
                    let i = self.pop()?;
                    self.vars.insert(var.clone(), i);
                },
                ZfToken::ZJump(i) => {
                    let item = self.pop()?;
                    if !Into::<bool>::into(&item) {
                        self.rs[crs].ip = (self.rs[crs].ip as isize + i) as usize;
                        continue;
                    }
                },
                ZfToken::UJump(i) => {
                    self.rs[crs].ip = (self.rs[crs].ip as isize + i) as usize;
                    continue;
                }
                ZfToken::Switch(s) => self.current = s.clone(),
                ZfToken::Guard { before: _b, after: _a } => (), // TODO
                _ => self.push(ib[ip].clone()),
            }

            self.incip();
        }

        Ok(())
    }
}

fn main() {
    let mut env = ZfEnv::new();

    for (word_name, word_ref) in &stdlib::STDLIB_WORDS {
        env.dict.push((word_name.to_string(), ZfProc::Builtin(word_ref.clone())));
    }

    let parsed = parser::parse(include_str!("std/builtin.zf"));
    let compiled = parser::compile(&mut env, parsed.unwrap());
    let stdlib = env.addword("main".to_owned(), compiled.unwrap());
    env.run(stdlib).unwrap();

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let parsed = match parser::parse(&buffer) {
        Ok(tokens) => tokens,
        Err(error) => { eprintln!("{}", error); return; },
    };
    let compiled = parser::compile(&mut env, parsed);
    let main = env.addword("main".to_owned(), compiled.unwrap());

    match env.run(main) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("error: {}", e);
            errors::stacktrace(&mut env);
            std::process::exit(1);
        },
    }
}
