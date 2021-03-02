// pritty print errorws

use crate::*;

fn _pritty(e: &ZfEnv, c: &ZfToken) -> String {
    match c {
        ZfToken::Nop        => format!("<nop>"),
        ZfToken::Number(i)  => format!("{}", i),
        ZfToken::String(s)  => format!("{:?}", s),
        ZfToken::Symbol(s)  => format!("<symb {} '{}'>", s, e.dict[*s].0),
        ZfToken::SymbRef(s) => format!("<ref {} '{}'>", s, e.dict[*s].0),
        ZfToken::Address(i) => format!("<addr {}>", i),
    }
}

pub fn stacktrace(e: &mut ZfEnv) {
    eprintln!("stack backtrace:");
    for frame in e.rs.iter().rev() {
        let name = &e.dict[frame.0].0;
        eprintln!("  => {:12}[{}] at {}", name, frame.0, frame.1);
    }

    eprintln!("data stack:");
    let mut ctr = 1;
    for item in e.pile.iter().rev() {
        if ctr > 9 {
            break;
        }

        eprintln!("  {}. {}", ctr, _pritty(e, &item));
        ctr += 1;
    }

    eprintln!("execution context:");
    let curframe = e.rs.len() - 1;
    if let ZfProc::User(code) = &e.dict[e.rs[curframe].0].1 {
        eprint!("{}", _pritty(e, &code[0]));
        if code.len() > 1 {
            for token in &code[1..] {
                eprint!(", {}", _pritty(e, token));
            }
        }
        eprintln!();
    }
}
