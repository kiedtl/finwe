// pritty print errorws

use crate::*;

pub fn stacktrace(e: &mut ZfEnv) {
    for frame in e.rs.iter().rev() {
        let name = &e.dict[frame.0].0;
        eprintln!("=> {:12}[{}] at {}", name, frame.0, frame.1);
    }
}
