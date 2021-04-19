// The goal is for this project to be less than 800 loc in length (not including
// std/builtin.zf and tests).

#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate pest;

#[macro_use]
mod utils;

mod vm;
mod ratios;
mod parser;
mod errors;
mod stdlib;
mod random;

use std::io::{self, Read};
use crate::vm::*;

fn main() {
    let mut env = vm::VM::new();

    for (word_name, word_ref) in &stdlib::STDLIB_WORDS {
        env.dict.push((word_name.to_string(), Word::Builtin(word_ref.clone())));
    }

    let parsed = parser::parse(include_str!("std/builtin.zf"));
    let compiled = parser::compile(&mut env, parsed.unwrap());
    let stdlib = env.addword("main".to_owned(), compiled.unwrap());
    env.run(stdlib).unwrap();

    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).unwrap();

    let parsed = match parser::parse(&buffer) {
        Ok(tokens) => tokens,
        Err(error) => { eprintln!("{}", error); return; },
    };
    let compiled = parser::compile(&mut env, parsed);
    let main = env.addword("main".to_owned(), compiled.unwrap());

    match env.run(main) {
        Ok(()) => (),
        Err(e) => {
            eprintln!("error: {}", e);
            errors::stacktrace(&mut env);
            std::process::exit(1);
        },
    }
}
