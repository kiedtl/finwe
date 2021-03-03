use crate::*;

macro_rules! pop {
    ($e:ident) => {
        match $e.pile.pop() {
            Some(e) => e,
            None => return Err(format!("stack underflow")),
        }
    }
}

macro_rules! pop_as {
    ($e:ident, $t:ident) => {{
        let popped = pop!($e);
        match popped {
            ZfToken::$t(v) => v,
            _ => return Err(format!("expected {}, got {:?}",
                    stringify!($t), popped)),
        }
    }}
}

#[allow(non_snake_case)]
pub fn FETCH(env: &mut ZfEnv) -> Result<bool, String> {
    let var = pop_as!(env, String);
    if env.vars.contains_key(&var) {
        env.pile.push(env.vars[&var].clone());
        Ok(false)
    } else {
        Err(format!("unknown variable {}", var))
    }
}

#[allow(non_snake_case)]
pub fn STORE(env: &mut ZfEnv) -> Result<bool, String> {
    env.vars.insert(pop_as!(env, String), pop!(env));
    Ok(false)
}

/// cond --
#[allow(non_snake_case)]
pub fn CRET(env: &mut ZfEnv) -> Result<bool, String> {
    if Into::<bool>::into(&pop!(env)) {
        env.rs.pop();
        if env.rs.len() > 0 {
            let l = env.rs.len() - 1;
            env.rs[l].1 += 1;
        }
        Ok(true)
    } else {
        Ok(false)
    }
}

/// address --
#[allow(non_snake_case)]
pub fn CJUMP(env: &mut ZfEnv) -> Result<bool, String> {
    let addr = pop_as!(env, Address);
    let cond = pop!(env);

    if Into::<bool>::into(&cond) {
        let tosidx = env.rs.len() - 1;
        env.rs[tosidx].1 = addr;
        Ok(true)
    } else {
        Ok(false)
    }
}

/// cond? quote --
#[allow(non_snake_case)]
pub fn IF(env: &mut ZfEnv) -> Result<bool, String> {
    let func = pop_as!(env, Symbol);

    if Into::<bool>::into(&pop!(env)) {
        env.rs.push((func, 0));
        Ok(true)
    } else {
        Ok(false)
    }
}

/// -- d
#[allow(non_snake_case)]
pub fn DEPTH(env: &mut ZfEnv) -> Result<bool, String> {
    env.pile.push(ZfToken::Number(env.pile.len() as f64));
    Ok(false)
}

/// a b c i=2 -- a b c b
#[allow(non_snake_case)]
pub fn PICK(env: &mut ZfEnv) -> Result<bool, String> {
    let i = pop_as!(env, Number) as usize;

    if (i + 1) > env.pile.len() {
        return Err(format!("stack underflow ({} > {})",
            (i + 1), env.pile.len()));
    }

    let v = env.pile[env.pile.len()-1-i].clone();
    env.pile.push(v);
    Ok(false)
}

/// a b c i=1 -- a c b
#[allow(non_snake_case)]
pub fn ROLL(env: &mut ZfEnv) -> Result<bool, String> {
    let mut i = pop_as!(env, Number) as usize;

    let mut stuff = Vec::new();
    while i > 0 {
        stuff.push(pop!(env));
        i -= 1;
    }
    let needle = pop!(env);
    for thing in stuff.iter().rev() {
        env.pile.push(thing.clone());
    }
    env.pile.push(needle);

    Ok(false)
}

/// a --
#[allow(non_snake_case)]
pub fn DROP(env: &mut ZfEnv) -> Result<bool, String> {
    let _ = pop!(env);
    Ok(false)
}

/// a -- c
#[allow(non_snake_case)]
pub fn NOT(env: &mut ZfEnv) -> Result<bool, String> {
    let c = !Into::<bool>::into(&pop!(env));
    env.pile.push(ZfToken::Number(if c {1f64} else {0f64}));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn CMP(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    if a == b {
        env.pile.push(ZfToken::Number( 0f64));
    } else if a > b {
        env.pile.push(ZfToken::Number( 1f64));
    } else if a < b {
        env.pile.push(ZfToken::Number(-1f64));
    }
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn PLUS(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.pile.push(ZfToken::Number(a + b));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn SUB(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.pile.push(ZfToken::Number(a - b));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn MUL(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.pile.push(ZfToken::Number(a * b));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn DMOD(env: &mut ZfEnv) -> Result<bool, String> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.pile.push(ZfToken::Number(a % b));
    env.pile.push(ZfToken::Number(a / b));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn bAND(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.pile.push(ZfToken::Number((a & b) as f64));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn bOR(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.pile.push(ZfToken::Number((a | b) as f64));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn bXOR(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.pile.push(ZfToken::Number((a ^ b) as f64));
    Ok(false)
}

/// a -- c
#[allow(non_snake_case)]
pub fn bNOT(env: &mut ZfEnv) -> Result<bool, String> {
    let a = pop_as!(env, Number) as usize;
    env.pile.push(ZfToken::Number((!a) as f64));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn SHL(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.pile.push(ZfToken::Number((a << b) as f64));
    Ok(false)
}

/// a b -- c
#[allow(non_snake_case)]
pub fn SHR(env: &mut ZfEnv) -> Result<bool, String> {
    let b = pop_as!(env, Number) as usize;
    let a = pop_as!(env, Number) as usize;
    env.pile.push(ZfToken::Number((a >> b) as f64));
    Ok(false)
}

/// a --
#[allow(non_snake_case)]
pub fn EMIT(env: &mut ZfEnv) -> Result<bool, String> {
    let val = pop_as!(env, Number);
    let err = format!("emit: {} is not a valid unicode codepoint", val);

    if val < 0.0 { return Err(err); }

    let mut encoded = [0; 4];
    match std::char::from_u32(val as u32) {
        Some(e) => { e.encode_utf8(&mut encoded); },
        None => return Err(err),
    };

    use std::io::Write;
    std::io::stdout().write(&encoded).unwrap();
    std::io::stdout().flush().unwrap();

    Ok(false)
}

#[allow(non_snake_case)]
pub fn WAIT(env: &mut ZfEnv) -> Result<bool, String> {
    let n = pop_as!(env, Number) as u64;
    std::thread::sleep(std::time::Duration::from_millis(n));
    Ok(false)
}

/// --
#[allow(non_snake_case)]
pub fn DBG(env: &mut ZfEnv) -> Result<bool, String> {
    if env.pile.len() > 0 {
        eprint!("{}", env.pile[env.pile.len()-1].fmt(env));
        if env.pile.len() > 1 {
            for thing in env.pile.iter().rev().skip(1) {
                eprint!(", {}", thing.fmt(env));
            }
        }
        eprintln!();
    }
    Ok(false)
}

/// --
#[allow(non_snake_case)]
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
