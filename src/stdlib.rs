use crate::*;

macro_rules! pop {
    ($e:ident) => {
        match $e.pile.pop() {
            Some(e) => e,
            None => return Err("expected value"),
        }
    }
}

macro_rules! pop_as {
    ($e:ident, $($t:ident),+) => {
        match pop!($e) {
            $(ZfToken::$t(v) => v,)+
            _ => return Err("bad type"),
        }
    }
}

/// name func --
#[allow(non_snake_case)]
pub fn PROC(env: &mut ZfEnv) -> Result<(), &str> {
    let quote = pop_as!(env, List);
    let name  = pop_as!(env, String);
    env.dict.insert(name, ZfProc::User(quote));
    Ok(())
}

/// a b -- b a
#[allow(non_snake_case)]
pub fn SWAP(env: &mut ZfEnv) -> Result<(), &str> {
    let (b, a) = (pop!(env), pop!(env));
    env.pile.push(b);
    env.pile.push(a);
    Ok(())
}

/// a b -- a b a
#[allow(non_snake_case)]
pub fn OVER(env: &mut ZfEnv) -> Result<(), &str> {
    let (b, a) = (pop!(env), pop!(env));
    env.pile.push(a.clone());
    env.pile.push(b);
    env.pile.push(a);
    Ok(())
}

/// a -- a a
#[allow(non_snake_case)]
pub fn DUP(env: &mut ZfEnv) -> Result<(), &str> {
    let b = pop!(env);
    env.pile.push(b.clone());
    env.pile.push(b);
    Ok(())
}

/// a --
#[allow(non_snake_case)]
pub fn DROP(env: &mut ZfEnv) -> Result<(), &str> {
    let _ = pop!(env);
    Ok(())
}

/// a b -- c
#[allow(non_snake_case)]
pub fn DMOD(env: &mut ZfEnv) -> Result<(), &str> {
    let (b, a) = (pop_as!(env, Number), pop_as!(env, Number));
    env.pile.push(ZfToken::Number(a % b));
    env.pile.push(ZfToken::Number(a / b));
    Ok(())
}

/// quote --
#[allow(non_snake_case)]
pub fn WHILE(env: &mut ZfEnv) -> Result<(), &str> {
    let quote = pop_as!(env, List);

    loop {
        let val = match env.pile.pop() {
            Some(v) => v,
            None => break,
        };

        if !Into::<bool>::into(val) {
            break;
        } else {
            run(&quote, env);
        }
    }

    Ok(())
}

/// var -- val
#[allow(non_snake_case)]
pub fn FETCH(env: &mut ZfEnv) -> Result<(), &str> {
    let var = pop_as!(env, String);
    if env.vars.contains_key(&var) {
        env.pile.push(env.vars[&var].clone());
    } else {
        return Err("unknown variable");
    }
    Ok(())
}

/// val var --
#[allow(non_snake_case)]
pub fn STORE(env: &mut ZfEnv) -> Result<(), &str> {
    let var = pop_as!(env, String);
    env.vars.insert(var, pop!(env));
    Ok(())
}

/// a --
#[allow(non_snake_case)]
pub fn io_TOS(env: &mut ZfEnv) -> Result<(), &str> {
    println!("{}", pop!(env));
    Ok(())
}

/// --
#[allow(non_snake_case)]
pub fn io_CR(_env: &mut ZfEnv) -> Result<(), &str> {
    println!("");
    Ok(())
}

/// --
#[allow(non_snake_case)]
pub fn DBG(env: &mut ZfEnv) -> Result<(), &str> {
    eprintln!("{:?}", env.pile);
    Ok(())
}
