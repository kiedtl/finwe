# Finwë

A compiled language for the [Uxn VM](https://wiki.xxiivv.com/site/uxn.html).

## What?

This language is an experimental project that I began to determine how modern,
high-level language features (such as structs, enums and memory allocators)
could be implemented in a constrained environment.

This project is possibly the first stack-based language to implement
compiler-enforced stack safety, i.e. using static analysis to catch underflows,
overflows, type errors, and so on.

Other features include a test harness, memory protection, and generics.

## How?

Consider the following example, which is obviously erroneous. It will not
compile:

```
(use* core)

(word main ( -- ) [
	drop
])
```

When the `drop` word is analysed, its signature is compared against the current
stack state. Because the drop word requires one argument (`Any --`), and the
stack has no items on it at that point, the following compile error is given:

```
   2 |
   3 | (word main ( -- ) [
   4 |  drop
           ^
StackUnderflow: Stack underflow (at index 1)
      at ot.finw:4:5
```

# Why?

My first forays into Uxn were difficult. Like all concatentive languages, one
needs to be able to keep the stack context in their heads while writing code;
Uxn makes this more difficult by introducing 16-bit words, which act on a pair
of byte, treating it as a single item. Thus, a single mistake can lead to some
difficult-to-debug issues.

Finwë was originally designed to abstract away the concept of 16-bit/8-bit
words. In Finwë, all the core words can work with both (are "generic"), taking
`Any` args (instead of `U16`/`U8`). At runtime, they are monomorphized into a
specific flavor -- either 16-bit or 8-bit -- and compiled to Uxn bytecode.

Naturally, allowing generic words necessitates that the compiler know the stack
state at any given point. Tracking this across the program allows for other
improvements, such as catching underflows/overflows, warning about type
mismatches, and so on. Implementing a feature like this had been an idea of mine
for a long time, so this was a welcome opportunity to put it to the test.

<!--
While concatenative/stack-based languages are intriguing, they suffer from being
rather write-only, due to needing to understand the entire stack state at any
given point in a program in order to read it effortlessly.

A language being write-only also implies it is difficult to write.
-->

## Status

Finwë is currently experimental. Much work remains regarding language features,
safety, and especially error reporting.

## Installation

No binary is currently provided, please compile on your own:

```
$ zig build
$ zig-out/bin/finwe myfile.finw             # Compile and run with builtin VM.
$ zig-out/bin/finwe myfile.finw -x out.rom  # Compile to ROM.
$ zig-out/bin/finwe myfile.finw -a func     # Dump assembly for function.
$ zig-out/bin/finwe myfile.finw -d          # Output debug info to .syms file. Requires -x.
```

Note that the standard library must be in the same directory as the file you are
compiling.

## Docs

- [Language overview](doc/language.md)
- [Test harness](doc/test-harness.md)
- [DAMPE Varvara extension](doc/dampe.md)

External:
- [Uxn homepage](https://wiki.xxiivv.com/site/uxn.html)
- [Uxn opcode reference](https://wiki.xxiivv.com/site/uxntal_opcodes.html)
- [Uxntal reference](https://wiki.xxiivv.com/site/uxntal.html)
- [Varvara reference](https://wiki.xxiivv.com/site/varvara.html)

## License

MIT
