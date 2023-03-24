---
chapter: 6
name: Type Classes
sample: https://media.pragprog.com/titles/rshaskell/typeclasses.pdf
summary: "
When you first learned about types in Haskell, you learned about how to use
parametric polymorphism to create functions that implement the same algorithm
regardless of the type of value they are working on. Although parametric
polymorphism is a powerful tool, you will often find that you need to provide
different implementations of a function based on the input type, while still
remaining polymorphic. Type classes are Haskell's approach to providing this
form of ad-hoc polymorphism.
"
---

### Type Classes

When you first learned about types in Haskell, you learned about how to use
parametric polymorphism to create functions that implement the same algorithm
regardless of the type of value they are working on. Although parametric
polymorphism is a powerful tool, you will often find that you need to provide
different implementations of a function based on the input type, while still
remaining polymorphic. Type classes are Haskell's approach to providing this
form of ad-hoc polymorphism.

You'll learn how to write polymorphic functions that depend on a specific
implementation for a given type. You'll also learn how to create your own type
classes, and to use a GHC language extension that lets you more explicitly tell
the compiler how to decide which behavior to use for a polymorphic function
call.

You'll be able to create new type classes that describe the behavior of types,
and implement the type classes for types that you define.

In the next several chapters you'll learn about some key concepts in Haskell
like how to make use of IO and how to use things like Monads and
Functors. Understanding how these things work requires a good understanding of
typeclasses.
