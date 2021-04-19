// pritty print errorws

use crate::vm::*;

pub fn stacktrace(e: &mut VM) {
    eprintln!("stack backtrace:");
    for frame in e.rs.iter().rev() {
        let name = &e.dict[frame.dictid].0;
        eprintln!("  => {:12}[{}] at {}", name, frame.dictid, frame.ip);
    }

    eprintln!("execution context:");
    let curframe = e.rs.len() - 1;
    if let Word::User(code) = &e.dict[e.rs[curframe].dictid].1 {
        eprint!("{}", &code[0].fmt(e));
        if code.len() > 1 {
            for token in &code[1..] {
                eprint!(", {}", token.fmt(e));
            }
        }
        eprintln!();
    }
}
