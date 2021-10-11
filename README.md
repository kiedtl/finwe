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

- New syntax based on S-expressions(?), with VM rewrite in Zig (parser
  might still be in Rust *if* no S-expressions)
  - Need to come up with executable format. Something like:
    ```
    #L main
    #l 121
    P 12
    Z 28
    ```
    translated:
    ```
    (debug info) Label main, line 121:
    push 12
    zjump 28
    ```
- Fix stack-name collisions.
- A better name! `zF` isn't very creative.
  - "Beaurocrat" (get it, stack of papers)? "Clerk"?
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
