---
chapter: 9
exercise-id: 3
name: The Extended Functor Family
summary: "
Summary TBD
"
---

## The Extended Functor Family {.problem}

In addition to the standard `Functor` class that you've used in this chapter,
there are other type classes that are related to `Functor` but with somewhat
different behaviors.

#### Bifunctors

A `Bifunctor` is like a `Functor` but even moreso, because a `Bifunctor` let's
you map two different fields. The `Bifunctor` class is defined in
`Data.Bifunctor`. Let's take a look at a definition for it:

```haskell
class Bifunctor f where
  bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

  first :: (a -> c) -> f a b -> f c b
  first f = bimap f id

  second :: (b -> d) -> f a b -> f a d
  second f = bimap id f
```

Try to write an instance of `Bifunctor` for `Either`.

#### Contravariant Functors

The `Contravariant` class from `Data.Functor.Contravariant` in base defines a
`_contravariant_` functor. Although we don't normally refer to them this way,
the `Functor` class that you've been working with so far is a
<emph>covariant</emph> functor. You don't need to worry about the terminology
too much though. You can think of this as a “backwards” functor. Let's
look at it's definition:

```haskell
class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
```

Try to create a new version of the `Function` type that you defined earlier, and
then write an instance of `Contravariant` for it. Can you also create an
instance of `Contravariant` for your original definition of `Function`? Why or
why not?

#### Profunctors

A `Profunctor` is a combination of a `Bifunctor` and a `Contravariant`
functor. `Profunctor` isn't defined in `base`, but you'll see it defined by some
other popular libraries. Like a `Bifunctor` it works on types with two
arguments. Like `Contravariant` functors, the first argument to a `Profunctor`
works “backwards”. Let's take a look at a definition for `Profunctor`:

```haskell
class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

  lmap :: (c -> a) -> f a b -> f c b
  lmap f = dimap f id

  rmap :: (b -> d) -> f a b -> f a d
  rmap f = dimap id f
```

Try to create an instance of `Profunctor` for your original `Function` type. Can
you write a valid instance? Why or why not? How does this differ from trying to
create a instance of `Contravariant` for `Function`?


### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
