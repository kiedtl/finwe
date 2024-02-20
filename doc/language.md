# Bur language

## Literals

```
0          // A byte literal
0xA 0b10   // More byte literals

0s         // A short literal, with an s suffix
0xFFFFs    // More short literals

"string"   // A string, the pointer of which is pushed onto the stack.
           // The string itself will be embedded in the data section.

.Op/Orot   // An enum literal.

[ 1 2 3 ]  // A quote literal.

nil        // Boolean literals.
t          // Equal to 1.
```

## Words

Here's a word that does nothing.

```
(word foo ( -- ) [ ])
```

Here's a word that take a byte and returns a short.

```
(word foo ( U8 -- U16 ) [ (as U16) ])
```

The arity can be left off, but this is strongly discouraged, as it can lead to
miscompilation if the word is accidentally generic (i.e. compiles down to
different bytecode depending on the argument type).

```
(word foo [ (as U16) ])
```

## Arity

Various elements can have a declared arity, such as words, loop conditionals,
and wild statements.

The basic syntax is as follows: `( ARGS -- STACK | RETURN_ARGS -- RETURN_STACK)`

The return-stack portion can be left off: `( ARGS -- STACK )`

The args or the stack can be left off as well: `( ARGS -- )`, `( ARGS )`, `( --
STACK )`

## Types

This is easily the most complex part of Bur.

There are several categories of types: the two basic elementary types (byte and
short), the basic types which add meaning to the elementary types (pointers,
etc), user-defined basic types (enums and single-element structs), composite
types (arrays and structs), `Opaque`, generic types, type expressions, and the
`Type` type.

### Elementary types

- `U8`
- `U16`

Enjoy their pureness. Devine approved.

### Layered types

These types act like either a byte or short -- but they add meaning which aids
readability. Some can be used interchangeably, some cannot.

- `Bool` (byte). `0` is false, `1` is true, everything else is true.
- `I8` (byte). A signed byte.
- `I16` (short). A signed short.
- `Char8` (byte). Interchangeable with `U8`.
- `Char16` (short). Interchangeable with `U16`. Intended to stand in as a
  unicode codepoint, but may be removed later on due to lack of usefulness.
- `Dev8` (byte). A byte-sized device port.
- `Dev16` (byte). A short-sized device port.

Finally, there are pointers, both byte (zero-page) and short-sized.
- Zero-page pointer support is currently incomplete.
- Short-size pointers are fully supported, and have a special syntax: `@type`.

### Devices

TODO (see std/varvara.bur in the meantime)

### Enums

Enums are defined like so:

```
(enum Foo U8 foo bar baz)
```

Note that the enum size (either `U8` or `U16`) is required.

The value can be specified, instead of being assigned at random:

```
(enum Foo U8
    [foo    0xAB]
    bar
    [baz    0xEF]
)
```

Enum literals take the following syntax: `.TYPE/elem`.

For example: `.Foo/foo`, `.Foo/bar`, `.Foo/baz`.

In the future, when Bur gets some basic type inference, the need to specify the
type might go away: `.foo`.


Bur has some builtin enums, such as `Op`. See the `asm()` docs for more info.

### Structs

Structs are defined like so:

```
(struct Foo
    [field1  U8]
    [field2  U8]
)
```

Structs larger than two bytes cannot be placed onto the stack, and can only
exist in memory.

Structs smaller than or equal to two bytes can be created on the stack with the
`make()` builtin:

```
(struct Foo [foo U8] [bar U8])

3 // foo field
4 // bar field
(make Foo)
```

Both structs on the stack and structs in memory can be accessed with `:field`.

### Arrays

Arrays cannot be placed onto the stack at all. Their purpose is solely to give a
type to data and pointers.

```
(let foo [U8 256]) // Foo is an array of U8, with a size of 256

(struct Foo
    [field1      U8]
    [field2 [U16 12] // Arrays can be part of structs
)

@[U16 12] // Array pointer. NOT a slice.
```

### `Opaque`

Opaque is a type-erased object. By itself, it cannot be used in any context,
because the size is unknown -- it could be a byte, a short, or an array of 500
shorts.

`@Opaque` is equivalent to `void *` in C.

### Generic types

Generic types allow for declaring generic words, which are then monomorphized.

```
(word foo ( Any -- Any ) [
    // do stuff that can be done to either shorts or bytes
])
```

Nearly all the core words are generic, which allows for using them with both
shorts and bytes (though not a mix).

- `Any`: takes any type, short or byte sized.
- `Any8`: takes any byte-sized type.
- `Any16`: takes any short-sized type.
- `AnyPtr`: takes any pointer, zero-page or otherwise.
- `AnyPtr16`: takes any 16-bit pointer.
- `AnyDev`: takes any device port. (Recall that device ports are always byte-sized.)

Finally, there is the type reference: `$<number>`. They allow referencing the
concrete type a generic was "filled in" with:

```
// This makes it clear that the function can take any argument, and then
// return an element of that type.
(word foo (Any -- $0) [
    // stuff
])

// This is the (simplified) definition of the `rot` word.
// It takes three arguments of any type, and then returns three elements of
// those types, in a shuffled order.
//
// (The real `rot` definition is a bit more complicated to ensure that
// the elements are all of the same size -- either bytes or shorts, not a mix.)
//
(word rot (Any Any Any -- $1 $0 $2) [
    // inline assembly
])

// Although this was used as an example previously, it is actually incorrect,
// because putting a generic in the stack is invalid. It means the function
// could return literally anything, which is obviously wrong.
(word foo (Any -- Any) [
    // stuff
])
```

Note that number order is: `( ... $2 $1 $0 -- )`.

More powerful generic types are available through type expressions.

### Type expressions

- `(AnySz <type>)`: Generic. Matches any type with the same size as its arg.
  - Example: `(AnySz U16)` matches any short-sized type.
  - Why not just use `Any8` or `Any16`? This is useful with type references,
    when you take multiple generic types but need them to be a uniform size.
    E.g. `(AnySz $0) (AnySz $0) Any --` instead of just `Any Any Any --`.
- `(USz <type>)`: Resolves to either U8 or U16 depending on the arg size.
- `(ISz <type>)`: Resolves to either I8 or I16 depending on the arg size.
- `(AnyOf <type>)`: Takes any type that's a derivative of a struct template. See
  the struct template section for more info.
- `(AnySet <type1> <type2> <type2>)`: Generic. Matches any one of its args.
- `(Child <type1>)`: Resolves to the child type of a pointer or array arg.
  - E.g. `(Child @U8)` -> `U8`
- `(Of <type> <args...>)`: Creates an instance of a struct template. See the
  struct template section for more info.
- `(FieldType <type> <name>)`: Resolves to the field type of its struct
  argument.
- `(Omit <type> <name>)`: Creates a new struct from its arg, that omits a field.
- `@(Fn <arity>)`: Generic. Matches any function pointer with a specific arity.
  - Makes no sense to use on its own -- must be a pointer.

### `Type`

Sometimes, you need the caller of a generic function to specify a type, without
actually passing an arg of that type. Rather than re-inventing turbofish, Bur
uses `Type` arguments and the `of()` builtin.

```
(word foo ( U8 @U8 Type -- (Some_Expression $0) ) [
    // stuff
])

(of foo @Char8)
```

These special `Type` arguments do not appear on the stack at all. They live
entirely in Bur's head.

### Struct templates

Struct templates allow for defining "generics" as they are called in other
languages.

For example, we can define a generic vector type like so:

```
(struct Vector (Any)
    [capacity U16]
    [len U16]
    [items @[$0]]
)
```

Let's break this down.
- The struct takes a generic argument, `Any`. Later, any type references will be
  filled in with the argument that is passed.
- The `items` field, a pointer to an array, is defined as `@[$0]`, or a pointer
  to an array of whatever the argument was.

Then, we can instantiate any amount of actual, concrete types based on the type
template, with the `Of()` type expression:

```
(Of Vector U8)
(Of Vector I16)
(Of Vector @(Fn (U8 -- U8)))
(Of Vector @Opaque)
```

And, we can have functions that operate on any derivative of the `Vector`
template, with the `AnyOf()` type expression:

```
(word last-item ( @(AnyOf Vector) -- (Child (FieldType (Child $0) items)) ) [
    // stuff
])
```

For an example of how this is used, see `std/vec.bur`.

### Type aliases

```
(typealias AnySigned (AnySet I8 I16))
```

Note that they are currently very limited, as they cannot contain type
references.

## Quotes

Quotes are anonymous functions, and quote literals compile down to a reference
to that anonymous function. In that way, they are very much unlike Tal's
lambdas.

Please note: the brackets in `(when [ body ])` etc do not denote a quote, it is
simply a reuse of syntax.

```
[ (-- U8 U8 U8) 0 1 2 ]

// Equivalent to:
(word _anonymous (-- U8 U8 U8) [ 0 1 2 ]
&_anonymous

// (There's no such syntax as &func, just for demonstration purposes)
```

- Non-empty quotes are required to specify an arity.
- The type of a quote pointer on the stack is `@(Fn <arity>)`.
- Quotes can be executed with the `do` core word.

## Wild blocks

Sometimes you need to bypass the analyser to do magic, usually with inline
assembly. In these cases, the `wild` block can be used:

```
(wild (<before> -- <after>) [
    // crazy stuff
])
```

This works by forcing the analyser to "forget" what happened in the block,
erasing the end result with the result of the arity applied to the previous
stack state. It does *not* work by bypassing the analyser entirely. Thus,
several wild blocks may be necessary.

For example, the `swap-sb` word, which swaps a short and a byte, works by
"chopping" the short up and rotating, twice:

```
(word swap-sb (Any16 Any8 -- $0 $1) [
	(wild (--) [ (asm "" .Op/Orot) ])
	(wild (--) [ (asm "" .Op/Orot) ])
	(wild ($1 $0 -- $0 $1) [])
])
```

The first two wild blocks encase the assembly, and make the analyser pretend
that nothing happened.

The third wild block sets the analyser's stack state to the correct state.

Currently anything can occur in a wild block. In the future, this *may* be
changed to only allow assembly, as miscompilations can result from calling
generic functions.

## When/else

When blocks are Bur's conditionals. Behold:

```
// some boolean value is on the stack

(when [ 0 ] [ 1 ]) // 0 if bool was t, 1 if nil
```

When blocks must not introduce stack branching -- i.e. where the stack depth or
types could change depending on runtime control flow. This means that

- A when clause without an else block must not change the stack depth/types.
- A when clause with an else block must make the same change, if any, as the else
  block.

Examples of stack branching:

```
// Invalid: when-block is (--) but else-block is (-- U8)
(when [] [ 0 ])

// Same issue
(when [ 0 ] [])

// Invalid: when block is (-- U16) but else-block is (-- U8)
(when [ 0s ] [ 0 ])

// Same changes made to stack depth, ok
(when [ 0s ] [ 0s ])

// Invalid: when block is (-- @U8) but else-block is (-- U16)
// (Yes, this is rejected even though both @U8 and U16 are short-sized
(when [ 0s (as @U8) ] [ 0s ])

// Same changes made to stack depth, ok
(when [ 0 ] [ 0 ])
```

## Loops

There are currently two loop kinds: `until` and `while`. They both work in the
same general way, with a loop conditional and loop body. (The `until` loop is
more efficient.)

```
0 (until [ 9 = ] [
    print nl
    1+
]) drop
```

Bur will automatically duplicate whatever arguments the loop conditional needs,
so that the stack is unchanged for the loop body. By default, with no arity
specified, the loop conditional is assumed to only use the TOS. This can be
changed by specifying an arity:

- `(until [ (U8 -- Bool) ...`: loop conditional takes a byte. (default)
- `(until [ (U8 U8 -- Bool) ...`: loop conditional takes two bytes.
- `(until [ (U16 U8 -- Bool) ...`: loop conditional takes a short and a byte.
- `(until [ (U16 U16 -- Bool) ...`: loop conditional takes two shorts.
- `(until [ (-- Bool) ...`: loop conditional takes no args.

Notes:

- The loop conditional must always return a single bool.
- In the future, `break`/`continue` syntax will be added. For now no such
  mechanism exists.
- The body may not introduce stack branching (like when clauses).
- The `until` loop is more efficient than the `while`, so prefer `until` as long
  as it doesn't cause larger/inefficient code.

## Cond blocks

Conds are where-clauses on steroids. Behold:

```
(word print (Dir -- ) [
	(cond
		[ .Dir/n  = ] [ "N " ]
		[ .Dir/s  = ] [ "S " ]
		[ .Dir/e  = ] [ "E " ]
		[ .Dir/w  = ] [ "W " ]
		[ .Dir/ne = ] [ "NE" ]
		[ .Dir/nw = ] [ "NW" ]
		[ .Dir/se = ] [ "SE" ]
		[ .Dir/sw = ] [ "SW" ]
	)
	print-string
	drop
])
```

Notes:
- Like loop conditionals, Bur will automatically insert code to duplicate the
  conditional block's arguments.
- Rules against stack branching apply -- stack effects for branches must be the
  same.
- Cond conditional blocks may require different args.

## `r` blocks

Return-blocks cause the code to operate on the return stack. Example:

```
+             // Adds on the working stack
(r +)         // Adds on the return stack
(r [ + + + ]) // Adds several times on the return stack
```

Related are the `STH` core words:

```
move          // Equivalent to STH/STH2
copy          // Equivalent to STHk/STH2k
(r move)      // Equivalent to STHr/STH2r
(r copy)      // Equivalent to STHkr/STH2kr
```

Notes:

- It is strongly recommended to only use `r` blocks on core words and other
  small, well-understood definitions. This feature has not been well-tested and
  may lead to stack corruption and miscompilations. Additionally, some language
  builtins are not available in this block.
- This works under the hood by inlining whatever is being called and then
  toggling the `r` bit on the generated bytecode. Thus calling a large function
  is discouraged.
- Prefer using `(r move) <func> move` where possible.

## Casting

There are several kinds of casting:

- `as()` builtin, which a short being turned into a byte or vice versa.

```
0s (as U8)       // U16 -> U8 (compiles down to a NIP)
0  (as U16)      // U8 -> U16 (compiles down to a LIT 00 SWP)
0s (as @Opaque)  // U16 -> @Opaque (compiles down to absolutely nothing)
```

- `as()` builtin, where no change is being made on the stack, only the types are
  modified.

```
// Cast multiple stuff at once...
0 1 2 3 (as Char8 Char8 Char8 Char8)

// ...this can only be done where no actual change is made
0s 1s 2s 3
(as Char8 Char8 Char8 Char8) // Invalid
```

- `split()` builtin, where a short-sized type is split into two byte-sized
  types. No actual change is made on the stack.

```
0xFFFFs (split U8 U8) // Boom. Stack effect was: (U16 -> U8 U8)
```

Notes:
- No bytecode is emitted for `as()` without actual changes and `split()`.

## Inline assembly

Syntax: `(asm "<flags>" .Op/opcode)`

Where `<flags>` are:

- `s`: short mode
- `k`: keep mode
- `r`: return-stack mode
- `g`: generic mode

Generic mode causes the analyser to fill in whether it's short-mode or not,
depending on the type of TOS. This, along with generic functions, is how most of
the core words can work with both shorts and bytes without needing separate
functions for each.

Opcodes are what you'd expect: `Orot`, `Oswp`, `Opop`, etc. For a few opcodes
(such as `Ojmp` and friends, or `Oraw`, `Olit`, etc), support is not added and
will probably never be added (due to lack of a use-case).

## Indexing

Indexes are used to index into pointers or arrays. The indice can be
compile-time known, a TOS short, a TOS byte, or a SOS short/byte.

```
// Declare an array
(let foo [U16 12])

@foo :0     // 0th element
@foo 1 :    // 1st element
2 @foo :    // 2nd element
```

Notes:
- Using a `U8` as an index causes casting to `U16` under the hood, so prefer
  `U16` directly where possible. (Naturally, `<u8-index> <ptr>` will lead to the
  most inefficient code.
- If indexing an array with a known length and with a known index, Bur can
  perform bounds checking. Thus, prefer `:<index>` where possible.

## Variables and static data

Simply declare variables like so:

```
(let name <type>)
```

... and use them:

```
@name // Get a pointer
$name // Memory load (LDA/LDA2)
```

If using `$name` syntax on an array, only the first memory is loaded.

Currently, variables are never inlined, though that will change in a future
release.
