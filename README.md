# zf

This is a toy concatenative stack-based language made to experiment with
various ideas, syntaxes, and language features:

- A cleaner syntax with less symbol soup.
- Infinite alternate stacks as global variables.
- Aggressive word inlining, where possible.
- Lua-like array-in-tables, with all the power and misery of APL.
  - Table "templates" that can be used like `struct`ures.
- Slightly more advanced control-flow.
- Stack "guards" that act as a runtime `assert` on stack usage.
- Parallel compilation.
- A few others I've forgotten.

## Examples

See `examples/*.zf` and `src/std/builtin.zf`.

## TODO

- A better name! `zF` isn't very creative.
- Add:
  - `.t` (tables)
  - `panic`
  - `err` and `ok`
  - `key`
  - `import`
  - `>>-`
- Docs for functions
- Escape sequences for characters and strings
- zF equivalents for the following:
  - retro-describe (RETRO)
  - retro-document (RETRO)
- Show code context on syntax/runtime error.
- Flesh out stack guards.
- Rust-like expression metadata.
  - Something like `%[inline always] word blah [[ ]]`
- A visualizer as a debugger!

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
