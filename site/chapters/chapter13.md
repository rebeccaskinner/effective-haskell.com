---
chapter: 13
name: Building Applications with Many Effects
summary: "
In this chapter you'll learn about how to compose monadic computations using
Monad transformers. You'll learn about some common Monad transformers provided
by the MTL and Transformers libraries, and learn how to make use of tagless
final encodings to use Monad transformers effectively.
"
---

### Building Applications with Many Effects
So far in this book you've learned about different individual Monads, like IO
actions, lists of values, and optional values. As you start to write more
sophisticated applications, it's frequently desirable to compose the properties
of these individual Monads to build up types with new behaviors that combine the
properties of individual monadic actions. This technique is used in a huge
number of real world Haskell applications, and it will enable you to read and
write real world professional Haskell applications.

In this chapter you'll learn about how to compose monadic computations using
Monad transformers. You'll learn about some common Monad transformers provided
by the MTL and Transformers libraries, and learn how to make use of tagless
final encodings to use Monad transformers effectively.

You'll be able to build new Monads that combine different sorts of monadic
computations, like having a read-only environment, dealing with exception
handling, and performing IO.

In the next chapter you will learn how to use Monad transformers to create a
core application Monad out of an MTL stack, using the Monad transformers that
you learn about in this chapter.
