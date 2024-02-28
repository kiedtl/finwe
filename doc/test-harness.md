# Test harness

Bur has an inbuilt test harness that can be used to write unit tests. With the
`-t` flag, Bur will compile all the tests it can find into a single ROM, and
then executes each test as a function while ensuring any assertions yield the
desired value.

## Defining test function

Simply use `test` to define a test function, that contains assertions.

Tests from other modules are included when they are `use`'d. Unfortunately that
means that any module that import `std`/`varvara`/etc will have the tests from
those modules run as well.

In the future, tests from other modules will have to be explicitly imported.

```
(test "name of test" [
    1 (should eq 1)
])
```

```
// Tests from my_module.bur imported

(use my_module)
```

## Assertions

Asserts take the form of `(should <what> <value?>)`, where `<value>` is
optional.

`<what>` is:

- `eq`: TOS must equal `<value>`, or the next value on the stack.
- `neq`: TOS must not equal `<value>`, or the next value on the stack.
- `stdout-eq`: When a test is run, `stdout`/`stderr` is captured and printed out
  at the end if no `(should stdout-eq "value")` is executed. If it is executed,
  the output is compared to the string given.

## Other automatic checks

An assertion is automatically added to the end of each test function, asserting
that the test is completed and doesn't erroneously return early.

The test harness also implicitly asserts that assertions not contained in a
conditional or loop execute, also to catch early returns.
