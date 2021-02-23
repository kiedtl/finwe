use crate::*;

/// f --
#[allow(non_snake_case)]
pub fn PROC(env: &mut ZfEnv) {
    let name;
    if let Token::String(n) = env.pile.pop().unwrap() {
        name = n.to_lowercase();
    } else {
        eprintln!("expected string");
        return;
    }

    match env.pile.pop().unwrap() {
        Token::List(q) => {
            env.dict.insert(name, ZfProc::User(q.clone()));
        },
        _ => eprintln!("expected quote"),
    }
}

/// a b -- b a
#[allow(non_snake_case)]
pub fn SWAP(env: &mut ZfEnv) {
    let b = env.pile.pop().unwrap();
    let a = env.pile.pop().unwrap();
    env.pile.push(b);
    env.pile.push(a);
}

/// a b -- a b a
#[allow(non_snake_case)]
pub fn OVER(env: &mut ZfEnv) {
    let b = env.pile.pop().unwrap();
    let a = env.pile.pop().unwrap();
    env.pile.push(a.clone());
    env.pile.push(b);
    env.pile.push(a);
}

/// a -- a a
#[allow(non_snake_case)]
pub fn DUP(env: &mut ZfEnv) {
    let b = env.pile.pop().unwrap();
    env.pile.push(b.clone());
    env.pile.push(b);
}

/// a --
#[allow(non_snake_case)]
pub fn DROP(env: &mut ZfEnv) {
    let _ = env.pile.pop().unwrap();
}

/// a b -- c
#[allow(non_snake_case)]
pub fn DMOD(env: &mut ZfEnv) {
    let (mut b, mut a) = (0f64, 0f64);
    if let Token::Number(_b) = env.pile.pop().unwrap() {
        b = _b;
    }
    if let Token::Number(_a) = env.pile.pop().unwrap() {
        a = _a;
    }

    env.pile.push(Token::Number(a % b));
    env.pile.push(Token::Number(a / b));
}

/// quote --
#[allow(non_snake_case)]
pub fn WHILE(env: &mut ZfEnv) {
    let quote;
    if let Token::List(q) = env.pile.pop().unwrap() {
        quote = q;
    } else {
        eprintln!("expected quotation");
        return;
    }

    while env.pile.pop().unwrap().into() {
        run(&quote, env);
    }
}

/// var -- val
#[allow(non_snake_case)]
pub fn FETCH(env: &mut ZfEnv) {
    let var;
    if let Token::String(_v) = env.pile.pop().unwrap() {
        var = _v;
    } else {
        eprintln!("expected string");
        return;
    }
    if env.vars.contains_key(&var) {
        env.pile.push(env.vars[&var].clone());
    } else {
        eprintln!("unknown variable {}", var);
    }
}

/// val var --
#[allow(non_snake_case)]
pub fn STORE(env: &mut ZfEnv) {
    let var;
    if let Token::String(_v) = env.pile.pop().unwrap() {
        var = _v;
    } else {
        eprintln!("expected string");
        return;
    }
    env.vars.insert(var, env.pile.pop().unwrap());
}

/// a --
#[allow(non_snake_case)]
pub fn io_TOS(env: &mut ZfEnv) {
    println!("{}", env.pile[env.pile.len()-1]);
}

/// --
#[allow(non_snake_case)]
pub fn io_CR(_env: &mut ZfEnv) {
    println!("");
}

/// --
#[allow(non_snake_case)]
pub fn DBG(env: &mut ZfEnv) {
    eprintln!("{:?}", env.pile);
}
