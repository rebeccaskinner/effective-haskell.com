---
chapter: 10
name: Mutable Data in the Real World
sample: https://media.pragprog.com/titles/rshaskell/mutabledata.pdf
summary: "
IO actions give us a way to represent computations that interact with the
real-world. One of the important ways that we can interact with the real world
that we haven't yet looked at is how to read and write real-world state. In this
chapter you'll learn how to deal with persistent mutable values in the real
world using IO references.
"
---

### Mutable Data in the Real World

IO actions give us a way to represent computations that interact with the
real-world. One of the important ways that we can interact with the real world
that we haven't yet looked at is how to read and write real-world state. In this
chapter you'll learn how to deal with persistent mutable values in the real
world using IO references.

In this chapter you'll learn how to build programs that interact with real-world
data that can change over time as your program runs. You'll also learn more
about how Haskell programs run.

After you've finished this chapter you'll know how to write functions that have
persistent data that you can update over time.

Mutable data can be useful for building metrics and logging systems, writing
certain efficient algorithms, and for debugging and testing your code. These
things will all be important as you start building larger and more sophisticated
programs.
