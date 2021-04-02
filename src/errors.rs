// pritty print errorws

use crate::*;

pub fn stacktrace(e: &mut ZfEnv) {
    eprintln!("stack backtrace:");
    for frame in e.rs.iter().rev() {
        let name = &e.dict[frame.dictid].0;
        eprintln!("  => {:12}[{}] at {}", name, frame.dictid, frame.ip);
    }

    eprintln!("data stack:");
    let mut ctr = 1;
    for item in e.pile.iter().rev() {
        if ctr > 9 {
            break;
        }

        eprintln!("  {}. {}", ctr, item.fmt(e));
        ctr += 1;
    }

    eprintln!("execution context:");
    let curframe = e.rs.len() - 1;
    if let ZfProc::User(code) = &e.dict[e.rs[curframe].dictid].1 {
        eprint!("{}", &code[0].fmt(e));
        if code.len() > 1 {
            for token in &code[1..] {
                eprint!(", {}", token.fmt(e));
            }
        }
        eprintln!();
    }
}
