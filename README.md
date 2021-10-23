# zf

This is a toy concatenative stack-based language made to experiment with
various ideas, syntaxes, and language features:

- A cleaner syntax with less symbol soup. (Yes, yes, I know, this goal is
  nowhere near being reached.)
- Infinite alternate stacks as global variables.
- Aggressive word inlining, where possible.
- Vectors, with all the power and misery of APL.
- Table "templates" that restrict what fields are available on a table
  instance, and provide default fields and methods.
- Slightly more advanced control-flow.
- Stack "guards" tell the compiler what a word expects on the stack and
  what it returns, enabling static analysis.
- Parallel compilation.
- Parallel execution with alternative stacks.
- Word attributes that serve as hints to the optimizer.
  - For example: a `%[double-call-is-nop]` attribute that directs
    the optimizer to remove duplicate adjacent calls to a function.
    (This can be utilized to filter out `not not` calls, for
    instance.
- A few others I've forgotten.

## Examples

See `examples/*.zf` and `src/std/builtin.zf`.

## TODO

- VM rewrite in Zig
- Remove `()` syntax for comments
- Add `table:key` and `:key` shorthand syntax.
- Change variable syntax
- Remove unnecessary `is?` and `any` keywords.
- Add doc comments.
- A better name! `zF` isn't very creative.
  - "Bureaucrat" (get it, stack of papers)? "Clerk"?
- Fix stack-name collisions.
- Add:
  - Misc stdlib functions
  - `debug` block that only compiles code if in
    debug mode.
  - `while` loop.
  - `iterate` loop.
  - `defer`, `errdefer`(?) blocks that are run on
    the function returning. (Would require making the
    `return` word non-returning)
- Escape sequences for characters and strings
- zF equivalents for the following:
  - retro-describe (RETRO)
  - retro-document (RETRO)
- Show code context on syntax/runtime error.
- Flesh out stack guards.
- Rust-like expression metadata.
  - `%[inline always] word blah [[ ]]`
  - `attr(inline always) word blah [[ ]]`
  - `meta(inline always) word blah [[ ]]`
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
