#![allow(non_snake_case)]

use crate::*;

pub fn CRET(env: &mut ZfEnv) -> Result<bool, String> {
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

pub fn IF(env: &mut ZfEnv) -> Result<bool, String> {
    let func = pop_as!(env, Symbol);

    if Into::<bool>::into(&env.pop()?) {
        env.pushrs(func, 0);
        Ok(true)
    } else {
        Ok(false)
    }
}

pub fn AGAIN(env: &mut ZfEnv) -> Result<bool, String> {
    let len = env.rs.len() - 1;
    env.rs[len].ip = 0;
    Ok(true)
}

pub fn PUSH(env: &mut ZfEnv) -> Result<bool, String> {
    let len = env.rs.len() - 1;
    let item = env.pop()?;
    env.rs[len].altpile.push(item);
    Ok(false)
}

pub fn POP(env: &mut ZfEnv) -> Result<bool, String> {
    let len = env.rs.len() - 1;
    let pupped = match env.rs[len].altpile.pop() {
        Some(v) => v,
        None => return Err(format!("stack underflow on alternate stack")),
    };
    env.push(pupped);
    Ok(false)
}

pub fn DEPTH(env: &mut ZfEnv) -> Result<bool, String> {
    let depth = env.cur_stack().len();
    env.push(ZfToken::Number(depth as f64));
    Ok(false)
}

pub fn ARRANGE(env: &mut ZfEnv) -> Result<bool, String> {
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

pub fn PICK(env: &mut ZfEnv) -> Result<bool, String> {
    let i = pop_as!(env, Number) as usize;
    let len = env.cur_stack().len();

    if i + 1 > len {
        return Err(format!("stack underflow ({} > {})", (i + 1), len));
    }

    let v = env.cur_stack()[len - 1 - i].clone();
    env.push(v);
    Ok(false)
}

pub fn ROLL(env: &mut ZfEnv) -> Result<bool, String> {
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

pub fn DROP(env: &mut ZfEnv) -> Result<bool, String> {
    let _ = env.pop()?;
    Ok(false)
}

pub fn NOT(env: &mut ZfEnv) -> Result<bool, String> {
    let c = !Into::<bool>::into(&env.pop()?);
    env.push(ZfToken::Number(if c {1f64} else {0f64}));
    Ok(false)
}

pub fn CMP(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    if a == b {
        env.push(ZfToken::Number( 0f64));
    } else if a > b {
        env.push(ZfToken::Number( 1f64));
    } else if a < b {
        env.push(ZfToken::Number(-1f64));
    }
    Ok(false)
}

pub fn PLUS(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(ZfToken::Number(a + b));
    Ok(false)
}

pub fn SUB(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(ZfToken::Number(a - b));
    Ok(false)
}

pub fn MUL(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(ZfToken::Number(a * b));
    Ok(false)
}

pub fn DMOD(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(ZfToken::Number(a % b));
    env.push(ZfToken::Number(a / b));
    Ok(false)
}

pub fn bAND(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(ZfToken::Number((a & b) as f64));
    Ok(false)
}

pub fn bOR(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(ZfToken::Number((a | b) as f64));
    Ok(false)
}

pub fn bXOR(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(ZfToken::Number((a ^ b) as f64));
    Ok(false)
}

pub fn bNOT(env: &mut ZfEnv) -> Result<bool, String> {
    let a = pop_as!(env, Number) as usize;
    env.push(ZfToken::Number((!a) as f64));
    Ok(false)
}

pub fn SHL(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(ZfToken::Number((a << b) as f64));
    Ok(false)
}

pub fn SHR(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.push(ZfToken::Number((a >> b) as f64));
    Ok(false)
}

pub fn EMIT(env: &mut ZfEnv) -> Result<bool, String> {
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

pub fn WAIT(env: &mut ZfEnv) -> Result<bool, String> {
    let n = pop_as!(env, Number) as u64;
    std::thread::sleep(std::time::Duration::from_millis(n));
    Ok(false)
}

pub fn DBG(env: &mut ZfEnv) -> Result<bool, String> {
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

pub fn DICTDBG(env: &mut ZfEnv) -> Result<bool, String> {
    let word = pop_as!(env, String);
    match env.findword(&word) {
        Some(p) => match &env.dict[p].1 {
            ZfProc::User(u) => eprintln!("{:?}", u),
            ZfProc::Builtin(b) => eprintln!("<builtin {:p}>", b),
        },
        None => return Err(format!("unknown word {}", word)),
    }
    Ok(false)
}


// --- floating-point stuff ---

pub fn CEIL(env: &mut ZfEnv) -> Result<bool, String> {
    let r = pop_as!(env, Number).ceil();
    env.push(ZfToken::Number(r));
    Ok(false)
}

pub fn FLOOR(env: &mut ZfEnv) -> Result<bool, String> {
    let r = pop_as!(env, Number).floor();
    env.push(ZfToken::Number(r));
    Ok(false)
}

pub fn ATAN(env: &mut ZfEnv) -> Result<bool, String> {
    let r = pop_as!(env, Number).atan();
    env.push(ZfToken::Number(r));
    Ok(false)
}

pub fn LOGN(env: &mut ZfEnv) -> Result<bool, String> {
    let (x, base) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(ZfToken::Number(base.log(x)));
    Ok(false)
}

pub fn POW(env: &mut ZfEnv) -> Result<bool, String> {
    let (y, x) = (pop_as!(env, Number), pop_as!(env, Number));
    env.push(ZfToken::Number(x.powf(y)));
    Ok(false)
}

pub fn FFMT(env: &mut ZfEnv) -> Result<bool, String> {
    let (prec, x) = (pop_as!(env, Number) as usize, pop_as!(env, Number));
    env.push(ZfToken::String(format!("{:.1$}", x, prec)));
    Ok(false)
}

// --- string/table stuff ---

pub fn TALLY(env: &mut ZfEnv) -> Result<bool, String> {
    let len = match env.peek()? {
        ZfToken::String(s) => s.len(),
        ZfToken::Table(t)  => t.len(),
        x => return Err(format!("{:?} is not indexable", x)),
    };
    env.push(ZfToken::Number(len as f64));
    Ok(false)
}

pub fn AT(env: &mut ZfEnv) -> Result<bool, String> {
    let index = pop_as!(env, Number);
    let item = match env.peek()? {
        ZfToken::String(s) => match s.chars().nth(index as usize) {
            Some(c) => ZfToken::Number(c as u32 as f64),
            None => return Err(format!("index out of range: {} >= {}",
                    index as usize, s.len())),
        },
        ZfToken::Table(t)  => match t.get(&ZfToken::Number(index)) {
            Some(t) => t.clone(),
            None => return Err(format!("index {} nonexistant", index as usize)),
        },
        x => return Err(format!("{:?} is not indexable", x)),
    };
    env.push(item);
    Ok(false)
}
