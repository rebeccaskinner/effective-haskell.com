---
chapter: 7
name: Understanding IO
summary: "
Performing IO is essential to the function of almost any application.  Even a
simple _hello world_ application must be able to perform IO to run.  Haskell's
approach to IO is significantly different than most other languages. The way
that Haskell approaches IO is a gateway into one of the most useful and
challenging parts of learning Haskell.
"
---

### Understanding IO

Performing IO is essential to the function of almost any application. Even a
simple "hello world" application must be able to perform IO to run. Haskell's
approach to IO is significantly different than most other languages. The way
that Haskell approaches IO is a gateway into one of the most useful and
challenging parts of learning HaHskell.

You'll learn what IO actions are and how to combine them.  You'll also be
introduced to monads and learn some of the most important operations that you
can do with them.

You'll be able to write functions that perform IO, and combine those functions
so that they run in a specific well defined sequence when you run your program.

In the next chapter you'll use what you've learned in this chapter to build a
fully working interactive command line application that reads files from disk,
interacts with the keyboard, and prints information to the screen.
