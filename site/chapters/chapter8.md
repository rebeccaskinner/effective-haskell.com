---
chapter: 8
name: Working with the Local System
summary: "
Whether you're writing desktop applications or cloud native services, at one
point or another you're going to need to interact with the environment that your
program is running in. Learning how to interact with the local system will let
you write more useful programs that work with files, environment variables, and
accept user input.
"
---

### Working with the Local System

Whether you're writing desktop applications or cloud native services, at one
point or another you're going to need to interact with the environment that your
program is running in. Learning how to interact with the local system will let
you write more useful programs that work with files, environment variables, and
accept user input.

You'll learn how to write an interactive application that reads data from
standard input, reads files from disk, and makes use of environment variables to
configure how the program runs. Importantly, in this chapter you'll learn
patterns for how to think about IO heavy pure functional programs.

After you've read this chapter you'll be able to write traditional command line
applications that deal with input and output. You'll also be able to think
through how to structure programs to handle different types of IO while
maximizing the benefits that you get from working in a pure functional language.

In the next chapter you'll build on the knowledge you've gained learning how to
use IO to work with the local system when you learn about the Functor,
Applicative, and Monad typeclasses. These typeclasses form the basis for how IO
works in Haskell. By starting with a concrete understanding of one instance of
how these typeclasses work, you'll be better prepared to extend that knowledge
to the more general case.
