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
