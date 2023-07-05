---
chapter: 6
exercise-id: 1
name: Writing Typeclass Representing Emptiness
summary: "
Practice writing type classes by creating your own class for optional
types. This exercise will give you a chance to cement your understanding of type
classes by defining a new type class and creating several instances for it.
"
---

## Writing Typeclass Representing Emptiness {.problem}

Imagine that we wanted to create a typeclass that represents things that can be
“empty” for some definition of empty that will depend on the particular type. In
this exercise we'll call the typeclass `Nullable` and give it two functions:

- `isNull` should return `True` if a value is “empty”
- `null` should return an “empty” value

```haskell
module Nullable where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a
```

Create instances of this typeclass for:

1. Any `Maybe a` where `a` is `Nullable`
2. Any tuple, `(a,b)` where `a` and `b` are `Nullable`
3. Any list type

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

There's more than one way to create an instance for `Maybe a`. You can pick
whichever definition you like.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Remember: for a type like `Maybe a` if you want to use `null` or `isNull` for
the type `a` you'll need to ensure that `a` has a `Nullable` instance.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

In the instance that you define for `Maybe a` you can ensure `a` has a
`Nullable` instance like this:

```haskell
instance Nullable a => Nullable (Maybe a) where
  -- the body of the instance goes here
```

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

The first part of our exercise presents us with a problem that has more than one
solution. We're asked to write an instance of `Nullable` for a `Maybe`
value. One obvious solution to this problem would be to treat `Nothing` as a
null value, and a `Just` value as non-null:

```haskell
instance Nullable (Maybe a) where
  isNull Nothing = True
  isNull _ = False

  null = Nothing
```

Technically this solution solves the question as asked. Although we don't
require that `a` be nullable, there's nothing to stop the instance from working
in cases where it is nullable. Still, this isn't really in the spirit of the
question, so let's dig in a bit more. Let's start by adding the constraint, then
we can figure out what to do with it:

```haskell
instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull _ = False

  null = Nothing
```

Now that we've added the constraint, we can use `isNull` and `null` for
`a`. One way we can take advantage of this is by being more permissive about
what we consider a null value. Let's take another pass at this implementation:

```haskell
instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a

  null = Nothing
```

This version of our instance lets us account for the fact that even if we have a
`Just` value, it might be something that's still empty.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

The next part of the exercise asks us to write a `Nullable` instance for a
tuple. Unlike the `Maybe` instance we wrote earlier, tuple's don't have any
default value that naturally maps to being empty. Instead, we'll need to fall
back to the definitions of both `a` and `b`. Let's take a look:

```haskell
instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)
```

In this example, we're considering a tuple to be `null` if both elements of the
tuple are `null`. This tells us both how to create a new `null` tuple, and how
to test to see if an existing tuple is `null`.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
The final part of this exercise asks us to write an instance that will work for
any list type. This problem is complementary to the instance we wrote for
`Maybe`. Although we could have written an instance for `Maybe a` that didn't
require `a` to be nullable, we opted to add that requirement so that we could
identify cases where we had a `Just null` value. For our list instance, we are
asked to create a version that works for any `a` whether it's `Nullable` or
not. Let's take a look:

```haskell
instance Nullable [a] where
  isNull [] = True
  isNull _ = False

  null = []
```

Since we have no guarantee that `a` has a `Nullable` instance, we can't consider
its value when we're thinking about what might constitute a null list. With only
the shape of the list to consider, the only sensible choice is to make `null`
and empty list. Thanks to pattern matching, we can write a version of `isNull`
that doesn't require an `Eq` instance for `a`.
</div>
</div>
</details>

</div>
