# Typed Serialization

The [library itself is here](src/TypedSerialization.hs), and [example use-case is here](app/Main.hs).

Everything can be run with `cabal run`.

This library allows us to defer type checking of expressions to run time. At run time, there is a separate "type-checking" step which validates the given expression before any data is processed. This gives us the guarantee that data processing itself can't fail, no matter the input.

This would be useful for value passing inside the Flow Algebra. Algebra needs a way to pass typed values around (green connections in the [Graph DSL proposal](https://scriveab.atlassian.net/wiki/spaces/EN/pages/3088646145/8+-+Graph+DSL)). We want to type-check algebra graphs specified by users ahead of execution. TypedSerialization library allows us to work with values in validated graphs without worrying about any serialization failures.

## Unresolved Questions

* Does this already exist somewhere?
* Can we get rid of some of the type parameters? Using this library as-is, the algebra would gain another two type parameters. `Flow e c l k s t` doesn't look good.
* Is there another way to transform types to values with `Eq`, perhaps supported by the compiler?
