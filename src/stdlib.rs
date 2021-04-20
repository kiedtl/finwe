#![allow(non_snake_case)]

use crate::vm::*;
use std::collections::HashMap;

pub const STDLIB_WORDS: [(&'static str, NativeWord, bool); 39] = [ // {{{
    ("do",           &DO,  true),
    ("ret",         &RET, false),
    ("?ret",       &CRET, false),
    ("depth",     &DEPTH,  true),
    ("arrange", &ARRANGE,  true),
    ("pick",       &PICK,  true),
    ("roll",       &ROLL,  true),
    ("drop",       &DROP,  true),
    ("not",         &NOT,  true),
    ("cmp",         &CMP,  true),
    ("+",          &PLUS,  true),
    ("-",           &SUB,  true),
    ("*",           &MUL,  true),
    ("/mod",       &DMOD,  true),
    ("and",        &bAND,  true),
    ("or",          &bOR,  true),
    ("xor",        &bXOR,  true),
    ("bnot",       &bNOT,  true),
    ("shl",         &SHL,  true),
    ("shr",         &SHR,  true),
    ("emit",       &EMIT,  true),
    ("wait",       &WAIT,  true),
    ("push",       &PUSH,  true),
    ("pop",         &POP,  true),
    ("<-",       &S_PUSH,  true),
    ("<<-",   &S_DUPPUSH,  true),
    ("->",        &S_POP,  true),
    ("->>",    &S_DUPPOP,  true),
    (">-",       &S_DROP,  true),
    ("dbg",         &DBG, false),
    ("ddbg",    &DICTDBG, false),
    ("ceil",       &CEIL,  true),
    ("floor",     &FLOOR,  true),
    ("atan",       &ATAN,  true),
    ("logn",       &LOGN,  true),
    ("pow",         &POW,  true),
    ("_.f",        &FFMT,  true),
    ("#",         &TALLY,  true),
    ("&",            &AT,  true),
]; // }}}}

pub fn RET(env: &mut VM) -> Result<bool, String> {
    env.rs.pop();
    if env.rs.len() > 0 {
        let l = env.rs.len() - 1;//}}}
        env.rs[l].ip += 1;
    }
    Ok(true)
}

pub fn CRET(env: &mut VM) -> Result<bool, String> {
    if Into::<bool>::into(&env.pop()?) {
        env.rs.pop();
        if env.rs.len() > 0 {
            let l = env.rs.len() - 1;
            env.rs[l].ip += 1;
        }
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn DO(env: &mut VM) -> Result<bool, String> {
    let func = pop_as!(env, Symbol);
    env.pushrs(func, 0);
    Ok(true)
}

pub fn PUSH(env: &mut VM) -> Result<bool, String> {
    let item = env.pop_from("_")?;
    env.push(item);
    Ok(false)
}

pub fn POP(env: &mut VM) -> Result<bool, String> {
    let pupped = env.pop()?;
    env.push_to("_", pupped);
    Ok(false)
}

pub fn S_PUSH(env: &mut VM) -> Result<bool, String> {
    let stack = pop_as!(env, Stack);
    let item = env.pop()?;
    env.push_to(&stack, item);
    Ok(false)
}

pub fn S_DUPPUSH(env: &mut VM) -> Result<bool, String> {
    let stack = pop_as!(env, Stack);
    let item = env.peek()?.clone();
    env.push_to(&stack, item);
    Ok(false)
}

pub fn S_POP(env: &mut VM) -> Result<bool, String> {
    let stack = pop_as!(env, Stack);
    let pupped = env.pop_from(&stack)?;
    env.push(pupped);
    Ok(false)
}

pub fn S_DUPPOP(env: &mut VM) -> Result<bool, String> {
    let stack = pop_as!(env, Stack);
    let pupped = env.peek_from(&stack)?.clone();
    env.push(pupped);
    Ok(false)
}

pub fn S_DROP(env: &mut VM) -> Result<bool, String> {
    let stack = pop_as!(env, Stack);
    let _ = env.pop_from(&stack)?;
    Ok(false)
}

pub fn DEPTH(env: &mut VM) -> Result<bool, String> {
    let depth = env.cur_stack().len();
    env.push(Value::Number(depth as f64));
    Ok(false)
}

pub fn ARRANGE(env: &mut VM) -> Result<bool, String> {
    let mut cells = HashMap::new();
    let (b, a) = (pop_as!(env, String), pop_as!(env, String));

    for ch in a.chars() { cells.insert(ch, env.pop()?); }
    for ch in b.chars() {
        if !cells.contains_key(&ch) {
            return Err(format!("unknown identifier '{}'", ch));
        }
        env.push(cells[&ch].clone());
    }

    Ok(false)
}

pub fn PICK(env: &mut VM) -> Result<bool, String> {
    let i = pop_as!(env, Number) as usize;
    let len = env.cur_stack().len();

    if i + 1 > len {
        return Err(format!("stack underflow ({} > {})", (i + 1), len));
    }

    let v = env.cur_stack()[len - 1 - i].clone();
    env.push(v);
    Ok(false)
}

pub fn ROLL(env: &mut VM) -> Result<bool, String> {
    let mut i = pop_as!(env, Number) as usize;

    let mut stuff = Vec::new();
    while i > 0 {
        stuff.push(env.pop()?);
        i -= 1;
    }
    let needle = env.pop()?;
    for thing in stuff.iter().rev() {
        env.push(thing.clone());
    }
    env.push(needle);

    Ok(false)
}

pub fn DROP(env: &mut VM) -> Result<bool, String> {
    let _ = env.pop()?;
    Ok(false)
}

pub fn NOT(env: &mut VM) -> Result<bool, String> {
    let c = !Into::<bool>::into(&env.pop()?);
    env.push(Value::Number(if c {1f64} else {0f64}));
    Ok(false)
}

pub fn CMP(env: &mut VM) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    if a == b {
        env.push(Value::Number( 0f64));
    } else if a > b {
        env.push(Value::Number( 1f64));
    } else if a < b {
        env.push(Value::Number(-1f64));
    }
    Ok(false)
}

pub fn PLUS(env: &mut VM) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(Value::Number(a + b));
    Ok(false)
}

pub fn SUB(env: &mut VM) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(Value::Number(a - b));
    Ok(false)
}

pub fn MUL(env: &mut VM) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(Value::Number(a * b));
    Ok(false)
}

pub fn DMOD(env: &mut VM) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(Value::Number(a % b));
    env.push(Value::Number(a / b));
    Ok(false)
}

pub fn bAND(env: &mut VM) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(Value::Number((a & b) as f64));
    Ok(false)
}

pub fn bOR(env: &mut VM) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(Value::Number((a | b) as f64));
    Ok(false)
}

pub fn bXOR(env: &mut VM) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(Value::Number((a ^ b) as f64));
    Ok(false)
}

pub fn bNOT(env: &mut VM) -> Result<bool, String> {
    let a = pop_as!(env, Number) as usize;
    env.push(Value::Number((!a) as f64));
    Ok(false)
}

pub fn SHL(env: &mut VM) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(Value::Number((a << b) as f64));
    Ok(false)
}

pub fn SHR(env: &mut VM) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(Value::Number((a >> b) as f64));
    Ok(false)
}

pub fn EMIT(env: &mut VM) -> Result<bool, String> {
    let val = pop_as!(env, Number);
    let err = format!("emit: {} is not a valid unicode codepoint", val);

    if val < 0.0 { return Err(err); }

    match std::char::from_u32(val as u32) {
        Some(e) => print!("{}", e),
        None => return Err(err),
    };

    use std::io::Write;
    std::io::stdout().flush().unwrap();

    Ok(false)
}

pub fn WAIT(env: &mut VM) -> Result<bool, String> {
    let n = pop_as!(env, Number) as u64;
    std::thread::sleep(std::time::Duration::from_millis(n));
    Ok(false)
}

pub fn DBG(env: &mut VM) -> Result<bool, String> {
    let stack = env.cur_stack().clone(); // fuck u borrow chkr
    if stack.len() > 0 {
        print!("{}", stack[stack.len()-1].fmt(env));
        if stack.len() > 1 {
            for thing in stack.iter().rev().skip(1) {
                print!(", {}", thing.fmt(env));
            }
        }
        println!();
    } else {
        println!("(empty)");
    }
    Ok(false)
}

pub fn DICTDBG(env: &mut VM) -> Result<bool, String> {
    let word = pop_as!(env, String);
    match env.findword(&word) {
        Some(p) => match &env.dict[p].definition {
            Definition::User(u) => eprintln!("{:?}", u),
            Definition::Builtin(b) => eprintln!("<builtin {:p}>", b),
        },
        None => return Err(format!("unknown word {}", word)),
    }
    Ok(false)
}


// --- floating-point stuff ---

pub fn CEIL(env: &mut VM) -> Result<bool, String> {
    let r = pop_as!(env, Number).ceil();
    env.push(Value::Number(r));
    Ok(false)
}

pub fn FLOOR(env: &mut VM) -> Result<bool, String> {
    let r = pop_as!(env, Number).floor();
    env.push(Value::Number(r));
    Ok(false)
}

pub fn ATAN(env: &mut VM) -> Result<bool, String> {
    let r = pop_as!(env, Number).atan();
    env.push(Value::Number(r));
    Ok(false)
}

pub fn LOGN(env: &mut VM) -> Result<bool, String> {
    let (x, base) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(Value::Number(base.log(x)));
    Ok(false)
}

pub fn POW(env: &mut VM) -> Result<bool, String> {
    let (y, x) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(Value::Number(x.powf(y)));
    Ok(false)
}

pub fn FFMT(env: &mut VM) -> Result<bool, String> {
    let (prec, x) = (pop_as!(env, Number) as usize, pop_as!(env, Number));
    env.push(Value::String(format!("{:.1$}", x, prec)));
    Ok(false)
}

// --- string/table stuff ---

pub fn TALLY(env: &mut VM) -> Result<bool, String> {
    let len = match env.peek()? {
        Value::String(s) => s.len(),
        Value::Table(t)  => t.len(),
        x => return Err(format!("{:?} is not indexable", x)),
    };
    env.push(Value::Number(len as f64));
    Ok(false)
}

pub fn AT(env: &mut VM) -> Result<bool, String> {
    let index = pop_as!(env, Number);
    let item = match env.peek()? {
        Value::String(s) => match s.chars().nth(index as usize) {
            Some(c) => Value::Number(c as u32 as f64),
            None => return Err(format!("index out of range: {} >= {}",
                    index as usize, s.len())),
        },
        Value::Table(t)  => match t.get(&Value::Number(index)) {
            Some(t) => t.clone(),
            None => return Err(format!("index {} nonexistant", index as usize)),
        },
        x => return Err(format!("{:?} is not indexable", x)),
    };
    env.push(item);
    Ok(false)
}
