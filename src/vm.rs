use std::collections::HashMap;

pub const DEFAULT_STACK: &'static str = "_";

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GuardItem {
    Any, Number, Str, Quote,
    Unchecked
}

#[derive(Clone, Debug)]
pub enum Value {
    Nop,
    Number(f64),
    String(String),
    Symbol(usize),
    SymbRef(usize),
    Switch(String),
    Stack(String),
    Fetch(String),
    Store(String),
    Table(HashMap<Value, Value>),
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

impl Value {
    pub fn fmt(&self, e: &VM) -> String {
        match self {
            Value::Nop        => format!("<nop>"),
            Value::Number(i)  => format!("{}", i),
            Value::String(s)  => format!("{:?}", s),
            Value::Symbol(s)  => format!("<symb {}>", e.dict[*s].0),
            Value::SymbRef(s) => format!("<ref {}>",  e.dict[*s].0),
            Value::Stack(s)   => format!("<stack {}>", s),
            Value::Switch(s)  => format!("<sw {}>", s),
            Value::Fetch(s)   => format!("<fetch {}>", s),
            Value::Store(s)   => format!("<store {}>", s),
            Value::Table(t)   => format!("{:?}", t),
            Value::ZJump(i)   => format!("<zjmp {}>", i),
            Value::UJump(i)   => format!("<ujmp {}>", i),
            Value::Continue   => format!("<continue>"),
            Value::Break      => format!("<break>"),

            Value::Guard { before: _, after: _ }
                => format!("<guard {:?}>", self),
        }
    }
}

impl Eq for Value {}

impl PartialEq for Value {
    fn eq(&self, rhs: &Self) -> bool {
        use Value::*;

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

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Number(i) => i.to_bits().hash(state),
            Value::String(s) => s.hash(state),
            Value::SymbRef(s) => s.hash(state),
            Value::Table(t) => for (k, v) in t {
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

impl Into<bool> for &Value {
    fn into(self) -> bool {
        match self {
            Value::Number(n) => if *n == 0_f64 { false } else { true },
            _ => true,
        }
    }
}

// The returned bool tells calling code whether the instruction pointer or
// return stack was modified. If it was not, the calling code will know
// it's safe to increment the IP
pub type NativeWord = &'static dyn Fn(&mut VM) -> Result<bool, String>;

#[derive(Clone)]
pub enum Word {
    Builtin(NativeWord),
    User(Vec<Value>),
}

#[derive(Clone)]
pub struct RSFrame {
    pub dictid: usize,
    pub ip: usize,
}

#[derive(Clone)]
pub struct VM {
    pub vars:  HashMap<String, Value>,
    pub piles: HashMap<String, Vec<Value>>,
    pub current: String,
    pub dict:  Vec<(String, Word)>,
    pub rs:    Vec<RSFrame>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            vars: HashMap::new(),
            piles: HashMap::new(),
            current: DEFAULT_STACK.to_owned(),
            dict: Vec::new(),
            rs: Vec::new(),
        }
    }

    pub fn stack<'a>(&'a mut self, name: &str) -> &'a mut Vec<Value> {
        if !self.piles.contains_key(name) {
            self.piles.insert(name.to_owned(), Vec::new());
        }

        self.piles.get_mut(name).unwrap().as_mut()
    }

    pub fn cur_stack(&mut self) -> &mut Vec<Value> {
        let cur = &self.current.clone();
        self.stack(cur)
    }

    pub fn push_to(&mut self, stack: &str, item: Value) {
        self.stack(stack).push(item);
    }

    pub fn push(&mut self, item: Value) {
        let cur = &self.current.clone();
        self.push_to(cur, item);
    }

    pub fn peek_from<'a>(&'a mut self, stack: &str) -> Result<&'a Value, String> {
        let stack = self.stack(stack);
        let len = stack.len();

        match len {
            0 => Err(format!("stack underflow")),
            _ => Ok(&stack[len - 1])
        }
    }

    pub fn peek<'a>(&'a mut self) -> Result<&'a Value, String> {
        let cur = &self.current.clone();
        self.peek_from(cur)
    }

    pub fn pop_from(&mut self, stack: &str) -> Result<Value, String> {
        match self.stack(stack).pop() {
            Some(e) => Ok(e),
            None => Err(format!("stack underflow")),
        }
    }

    pub fn pop(&mut self) -> Result<Value, String> {
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

    pub fn addword(&mut self, name: String, body: Vec<Value>) -> usize {
        match self.findword(&name) {
            Some(i) => self.dict[i].1 = Word::User(body),
            None => self.dict.push((name.clone(), Word::User(body))),
        }
        self.findword(&name).unwrap()
    }

    pub fn pushrs(&mut self, funcid: usize, iptr: usize) {
        self.rs.push(RSFrame {
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
                Word::User(u) => ib = u,
                Word::Builtin(b) => {
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
                Value::Nop => (),

                Value::Symbol(s) => {
                    self.pushrs(*s, 0);
                    continue; // don't increment IP below
                },
                Value::SymbRef(i) => self.push(Value::Symbol(*i)),
                Value::Fetch(var) => if self.vars.contains_key(var) {
                    self.push(self.vars[var].clone());
                } else {
                    return Err(format!("unknown variable {}", var))
                },
                Value::Store(var) => {
                    let i = self.pop()?;
                    self.vars.insert(var.clone(), i);
                },
                Value::ZJump(i) => {
                    let item = self.pop()?;
                    if !Into::<bool>::into(&item) {
                        self.rs[crs].ip = (self.rs[crs].ip as isize + i) as usize;
                        continue;
                    }
                },
                Value::UJump(i) => {
                    self.rs[crs].ip = (self.rs[crs].ip as isize + i) as usize;
                    continue;
                }
                Value::Switch(s) => self.current = s.clone(),
                Value::Guard { before: _b, after: _a } => (), // TODO
                _ => self.push(ib[ip].clone()),
            }

            self.incip();
        }

        Ok(())
    }
}

