---
chapter: 6
exercise-id: 2
name: Adding a Default Null Test
summary: "
Get more comfortable with constraints on type classes and type class instances
by expanding on the work from the previous exercise in the chapter. You'll
modify an existing type class with a new constraint, and then look at how the
new constraint changes the default instances you can write.
"
---

## Adding a Default Null Test  {.problem}

Add a new `Eq` constraint to the definition of `Nullable`:

```haskell
class Eq a => Nullable a
```
With this change in place, create a default implementation of `isNull`.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Remember that your default implementation can refer to other functions defined
by the typeclass.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

With the `Eq` constraint, you can compare values of type `a` using `(==)`.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

A value is null if it's equal to `null`.

</div>
</div>
</details>

</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

This exercise asks us to add an `Eq` constraint to `Nullable` and use that to
allow us to write a default definition of `isNull`. This is a pretty small
change on it's own:

```haskell
class Eq a => Nullable a where
  isNull :: a -> Bool
  isNull = (== null)

  null :: a
```

Unfortunately, changing this definition of our class to add the extra constraint
means that we also need to update most of our instances as well:

```haskell
module EffectiveHaskell.Exercises.Chapter6.DefaultNull where
import Prelude hiding (null)

class Eq a => Nullable a where
  isNull :: a -> Bool
  isNull = (== null)
  null :: a

instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)

instance Eq a => Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []
```

The extra constraint doesn't impact how we're writing our instances, but it does
mean that we won't be able to use the instances in some cases. For example,
before adding the constraint we could use `isNull` to see whether or not we had
an empty list of functions, but functions don't have an `Eq` instance, so we
won't be able to do that anymore. In some cases the additional restriction would
be fine, but it limits the ways that people can use our typeclasses, and doing
so unnecessarily can make our code less reusable. Let's look at a couple of
other approaches we could have used that offer more flexibility.

#### Defaulting with a Helper

In this example, our motivation for adding an `Eq` constraint to the definition
of `Nullable` was so that we could provide a default implementation of
`isNull`. There's a common alternative that gives us almost as much ease-of-use
with a lot more flexibility: helper functions. Let's start looking at how they
work by creating a new function called `isNullHelper`:

```haskell
isNullHelper :: (Eq a, Nullable a) => a -> Bool
isNullHelper = (== null)
```

This function puts the same constraints on `a` that we would have in the default
`isNull` implementation we wrote, but it lives outside of the type class. That
means that we can drop the constraint at the type class level, but make use of
it for particular instances when it makes sense. Let's look at a concrete
example. First, we'll return to our original definition of `Nullable`:

```haskell
class Nullable a where
  isNull :: a -> Bool
  null :: a
```

Next, let's take a look at how we might use our new helper function. We'll start
by revisiting our `Nullable` instance for `[a]`. In our original definition of
`isNull` for lists, we didn't look at the values inside of the list at all- only
whether the list itself was empty. That makes the definition of `isNull` for
lists a good candidate to use the `isNullHelper` function we've just
added. Unfortunately, we can only test lists for equality if we can test the
elements inside the lists for equality, so we'll still need our `Eq` constraint:

```haskell
instance Eq a => Nullable [a] where
  isNull = isNullHelper
  null = []
```

As you can see, although we no longer have a default definition for `isNull`,
we're able to use the helper function so that it's very easy to write a new
instance. This also gives us the flexibility to define instances that work
differently and don't need an equality constraint. For example, let's take a
look at the instance for `Maybe`:

```haskell
instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing
```

In this example we're not actually testing for equality at all. If we do have a
value, we defer to whatever definition of `isNull` is provided by `a`.

This approach gives us some flexibility around the constraints on instances of
our typeclass, while still saving someone work in the common case that they can
rely on equality testing. It's not without drawbacks though. The main drawback
is that someone using our module might be confused and try to call
`isNullHelper` directly, even when it's behavior would differ from the
definition of `isNull`. That could be a source of bugs. There's another option
that we can use, but it requires that we add a new language extension.

#### Using DefaultSignatures

The `DefaultSignatures` extension gives us another way to solve the
problem. In this chapter you saw how this extension allows you to add a default
value to a typeclass that has narrower constraints than the type defined by the
class. In this case, we can use the extension to provide a default
implementation of `isNull` only when the `Nullable` value has an `Eq`
instance. Let's take a look:

```haskell
{-# LANGUAGE DefaultSignatures #-}
module EffectiveHaskell.Exercises.Chapter6.DefaultSignaturesNull where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool

  default isNull :: Eq a => a -> Bool
  isNull = (== null)
  null :: a
```

With `DefaultSignatures` enabled we're able to add a default definition of
`isNull` that works by comparing the input value to `null`, just like
`isNullHelper` from our earlier example. Like our other examples, we can still
create instances that provide a definition of `isNull`. If the type we're
defining a `Nullable` instance for doesn't have an instance of `Eq` we're
required to provide on:

```haskell
instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)
```

If, on the other hand, we do have an `Eq` constraint then we're free to provide
our own definition of `isNull`:

```haskell
instance (Eq a, Nullable a) => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing
```

Alternatively, we can use the default version:

```haskell
instance Eq a => Nullable [a] where
  null = []
```

In the last exercise you saw that we were able to avoid having an `Eq` instance
when we defined `isNull` for lists thanks to pattern matching. To use the
default instance, we need to add it. This is a good example of the tradeoffs
that you'll want to think about when defining default instances. Ideally, if
you're using `DefaultSignatures` to add constraints to the default
implementation of a function, you'll be adding common constraints that well come
"for free" for at least some implementations.

</div>
</div>
</details>
</div>
