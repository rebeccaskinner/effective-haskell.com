---
chapter: 6
exercise-id: 3
name: Deriving Nullable
summary: "
Summary TBD
"
---

## Deriving Nullable {.problem}

In the first exercise in this chapter you should have created an instance of
`Nullable` for `Maybe` and list values. There are a few ways that you could have
approached writing these instances, but let's look at some reasonable
definitions you might have used:

```haskell
module DerivingNullable where
import Prelude hiding (null)
import qualified Prelude (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a

instance Nullable [a] where
  isNull = Prelude.null
  null = []

instance Nullable (Maybe a) where
  isNull Nothing = True
  isNull _ = False
  null = Nothing
```

These instances use a fairly intuitive definition of what should be considered
`null`: empty lists are null, as are `Nothing` values. What if we have an
optional list though?

```
λ isNull Nothing
True
λ isNull []
True
λ isNull (Just [])
False
```

In this case it's not clear whether `Just []` should be considered a null value
or not, it depends entirely on the program we are writing. You can even imagine
that we might want different behavior in different parts of the same program.

In this exercise, try to create an API so that a user can make use of `deriving
via` to create `Nullable` instances of their own types. A user should be able to
decide whether `Just []` should be considered a null value or not by selecting
which type they derive their instance from.


### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
If you're having trouble imagining how an API like this might be used, imagine
that you're working with some text data that will be provided by user and you
need to decide whether a user provided a value or not. In some cases, an empty
string might be a valid input when the user doesn't have anything more
meaningful to input. In other cases, you might want to ensure that they've
provided some actual data.

For example, imagine that you wanted to define two types: `OptionalString` and
`OptionalNonEmptyString`. You might start by defining them like this:

```haskell
newtype OptionalString = OptionalString { getString :: Maybe String }
  deriving stock (Eq, Show)

newtype OptionalNonEmptyString = OptionalNonEmptyString { getNonEmptyString :: Maybe String }
  deriving stock (Eq, Show)
```

Instead of writing instances manually, think about how you could provide a way
for users to use `deriving via` with these types.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
You'll need to create a `newtype` for each of the behaviors you want to make
available with `deriving via`, along with a `Nullable` instance for each.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
Trying creating two types with `Nullable` instances. First, create a type named
`BasicNullable` that should have `isNull` return `True` only if there's a
missing value. Next, create one called `TransitiveNullable` with a definition of
`isNull` that will return also true if the inner value is null.
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

We'll start our solution by creating a new module and re-defining
`Nullable`. We'll go ahead and add the `DerivingVia` extension too, since we're
planning to use it later in this exercise:

```haskell
{-# LANGUAGE DerivingVia #-}
module EffectiveHaskell.Exercises.Chapter6.DerivingNullable where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a
```

Next, let's define some types that represent the different "templates” that we
might want to use with `deriving via` to get our behaviors. If you looked at the
hints earlier, you'll know that we're going to create two types: `BasicNullable`
and `TransitiveNullable`. Let's start with `BasicNullable` first.

We want our `BasicNullable` type to represent optional values that are only
considered null if they are truly missing a value. Let's start by creating the
type. We'll also add a `Show` instance to make things easier when we want to
test the code in `ghci` later:

```haskell
newtype BasicNullable a = BasicNullable (Maybe a)
  deriving stock Show
```

We also need to create a `Nullable` instance for `BasicNullable`. For this basic
definition of `Nullable`, we'll consider a `Nothing` value to be null, and
anything else will be non-null:

```haskell
instance Nullable (BasicNullable a) where
  isNull (BasicNullable Nothing) = True
  isNull _ = False
  null = BasicNullable Nothing
```

Next, let's do the same thing for `TransitiveNullable`. Unlike `BasicNullable`,
this instance will only consider something non-null if it contains a non-null
value:

```haskell
newtype TransitiveNullable a = TransitiveNullable (Maybe a)
  deriving stock Show

instance Nullable a => Nullable (TransitiveNullable a) where
  isNull (TransitiveNullable Nothing) = True
  isNull (TransitiveNullable (Just a)) = isNull a
  null = TransitiveNullable Nothing
```

Even though we created `BasicNullable` an `TransitiveNullable` so that we could
use them with `deriving via`, they are still valid ordinary types and we can
test their behavior in `ghci`. Let's run a few tests to make sure everything's
working as we expect. Let's start by testing out the `isNull` instance of
`BasicNullable`:

```
λ isNull $ BasicNullable Nothing
True

λ isNull $ BasicNullable (Just "hello")
False
```

We can see from these examples that `isNull` appears to be working for the
obvious cases, but we should still test that `isNull` correctly returns `False`
when we have `Just` some empty value. One way we can test this is to nest some
`BasicNullable` values:

```
λ isNull $ BasicNullable (Just $ BasicNullable Nothing)
False
```

This is a little weird though. Let's add an instance of `Nullable` for lists so
that we have some values to test with:

```haskell
instance Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []
```

Using these two instances, we can see that while `[]` is null, `BasicNullable
(Just [])` continues to be treated as non-null:

```
λ isNull []
True

λ isNull [1,2,3]
False

λ isNull $ BasicNullable (Just [])
False
```

Now that we've figured out `BasicNullable` let's move on to
`TransitiveNullable`. Just like before, we'll start by creating a new type:

```haskell
newtype TransitiveNullable a = TransitiveNullable (Maybe a)
  deriving stock Show
```

We'll also create a new instance of `Nullable`. Unlike our earlier instance,
we'll need to make sure that whatever type we're holding is also `Nullable`
since we'll need to check to see if the values we're working with are null or
not:

```haskell
instance Nullable a => Nullable (TransitiveNullable a) where
  isNull (TransitiveNullable Nothing) = True
  isNull (TransitiveNullable (Just a)) = isNull a
  null = TransitiveNullable Nothing
```

Like before, we can load this up into `ghci` to test it:

```
λ isNull $ TransitiveNullable (Just "hello")
False
```

Unfortunately, if we try to test the empty case of `TransitiveNullable` the same
way we tested `BasicNullable` we'll get an error:

```
λ isNull $ TransitiveNullable Nothing
<interactive>:20:1: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘isNull’
      prevents the constraint ‘(Nullable a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Nullable (BasicNullable a)
          -- Defined at EffectiveHaskell/Exercises/Chapter6/DerivingNullable.hs:12:10
        instance Nullable OptionalNonEmptyString
          -- Defined at EffectiveHaskell/Exercises/Chapter6/DerivingNullable.hs:35:12
        instance Nullable OptionalString
          -- Defined at EffectiveHaskell/Exercises/Chapter6/DerivingNullable.hs:31:12
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘($)’, namely ‘isNull’
      In the expression: isNull $ TransitiveNullable Nothing
      In an equation for ‘it’: it = isNull $ TransitiveNullable Nothing
```

The problem here is the definition of `Nullable` we defined for
`TransitiveNullable a` relies on the definition of `Nullable` for
`a`. When we use a value like `Just "hello"` the compiler can infer the type of
`a` must be `String`. When we use `Nothing` we're not giving the compiler enough
information to figure out what `a` should be, so it can't pick a `Nullable`
instance. We can help it out by adding a visible type application to
`TransitiveNullable` to tell it what type to use for `a`:

```
λ isNull $ TransitiveNullable @String Nothing
True
```

Alternatively, you can add a type annotation to `Nothing`:

```
λ isNull $ TransitiveNullable (Nothing :: Maybe String)
True
```

Now that we've covered both obviously null and obviously non-null cases, let's
take a look at an example where `TransitiveNullable` and `BasicNullable` should
differ: A value that contains an empty list. Let's test them side-by-side:

```
λ isNull $ TransitiveNullable (Just [])
True

λ isNull $ BasicNullable (Just [])
False
```

As expected, the `TransitiveNullable` instance considers an empty list to be
null, while the `BasicNullable` instance doesn't.

Now that we've created two different types that have our desired `Nullable`
behaviors, how can we use them with `deriving via`? To start with, let's imagine
that we're dealing with some text data. As a specific example, imagine that
you're processing some data submited by a user, and you want to ensure that
you're getting valid data. In some cases, you might have data that could be
empty, while in other cases you want to ensure that there's actual data. For
example, a user signing up for a service might be required to enter a password,
but the "how did you hear about us” field could be left empty.

We'll represent these two types of data with the types `OptionalString` and
`OptionalNonEmptyString`:

```haskell
newtype OptionalString = OptionalString { getString :: Maybe String }
  deriving stock Show

newtype OptionalNonEmptyString = OptionalNonEmptyString { getNonEmptyString :: Maybe String }
  deriving stock Show
```

You can imagine that we could write `Nullable` instances for these two types
that are identical to the `BasicNullable` and `TransitiveNullable` types we just
created, but thanks to `deriving via` we don't need to. Instead, we can add the
`DerivingVia` extension, and use it to select an instance:

```haskell
newtype OptionalString = OptionalString { getString :: Maybe String }
  deriving stock Show
  deriving Nullable via BasicNullable String

newtype OptionalNonEmptyString = OptionalNonEmptyString { getNonEmptyString :: Maybe String }
  deriving stock Show
  deriving Nullable via TransitiveNullable String
```

Let's load this code up into `ghci` and check that it behaves like we'd
expect. Starting with `OptionalString`:

```
λ isNull $ OptionalString (Just "hello")
False

λ isNull $ OptionalString (Just "")
False

λ isNull $ OptionalString Nothing
True
```

As you can see, since we derived the `Nullable` instance for `OptionalString`
from `BasicNullable`, the behavior is the same. Next, let's look at
`OptionalNonEmptyString`:

```
λ isNull $ OptionalNonEmptyString (Just "hello")
False

λ isNull $ OptionalNonEmptyString (Just "")
True

λ isNull $ OptionalNonEmptyString Nothing
True
```

Success! It appears that `OptionalNonEmptyString` is now using the same behavior
as `TransitiveNullable`.

You might have noticed that, in practice, using `deriving via` in this case
didn't buy us much- it seems as though we've actually done more work by creating
the generic types and then deriving our instances from them instead of creating
instances for `OptionalString` and `OptionalNonEmptyString` directly. In this
case it's true, but as soon as we need a second, third, or fourth type that
would have the same boilerplate implementation of a type class, then we'll have
saved ourselves some effort. Realistically, it's not always clear when you'll
want to reuse some definition of a typeclass, so you might find that instead of
creating the generic reusable types to start with, you recognize that you have
the same definition in multiple places and instead factor those out into a
common definition that you can use with `deriving via`.

</div>
</div>
</details>
</div>
