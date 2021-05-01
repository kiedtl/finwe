# zf

This is a toy concatenative stack-based language made to experiment with
various ideas, syntaxes, and language features:

- A cleaner syntax with less symbol soup. (Yes, yes, I know, this goal is
  nowhere near being reached.)
- Infinite alternate stacks as global variables.
- Aggressive word inlining, where possible.
- Vectors, with all the power and misery of APL.
- Table "templates" that can be used like `struct`ures.
- Slightly more advanced control-flow.
- Stack "guards" tell the compiler what a word expects on the stack and
  what it returns, enabling static analysis.
- Parallel compilation.
- A few others I've forgotten.

## Examples

See `examples/*.zf` and `src/std/builtin.zf`.

## TODO

- Fix stack-name collisions.
- A better name! `zF` isn't very creative.
- Add:
  - `.t` (tables)
  - `panic`
  - `err` and `ok`
  - `key`
  - `import`
  - `>>-` (reset-stack)
- Docs for functions
- Escape sequences for characters and strings
- zF equivalents for the following:
  - retro-describe (RETRO)
  - retro-document (RETRO)
- Show code context on syntax/runtime error.
- Flesh out stack guards.
- Rust-like expression metadata.
  - Something like `%[inline always] word blah [[ ]]`
  - The following metadata:
    - `impure` (none)
    - `for` (`linux`, `windows`, `darwin`)
    - `alias` (`<name>`)
    - `export`
    - `test`
- Tail-call optimizations
- Research other optimizations
- Static analysis on stack usage to catch stack underflows
- Tools:
  - A visualizer as a debugger!
  - Tool to check and generate stack guards for a word

### DONE

- if/until/times/match
- break statement for until
- remove `again`
- Add:
  - `ret`
  - `.b` (boolean)
  - `.s` (strings)
  - `log`
  - `.f` (floats)
  - `ackermann`
  - `prime?`
  - `romans`
- Vim syntax
- Move `?do` to builtin.zf
