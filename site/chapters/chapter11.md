---
chapter: 11
name: Serializing Heterogenous Data
summary: "
If you write enough programs it's inevitable that you'll realize at some point
that you need to store some heterogenous collection of data. Doing this in
Haskell isn't hard, but the approach is non-obvious, and in learning how to
manage these sorts of hetrogenous collections, you'll also learn some more about
how to better use Haskell's type system.
"
---

### Serializing Heterogenous Data
If you write enough programs it's inevitable that you'll realize at some point
that you need to store some heterogenous collection of data. Doing this in
Haskell isn't hard, but the approach is non-obvious, and in learning how to
manage these sorts of hetrogenous collections, you'll also learn some more about
how to better use Haskell's type system.

In this chapter you'll learn how to construct heterogenous collections of
elements, and you'll build your own hetrogeneous list type using existential
types.

You'll be able to work with heterogeneous collections and create data types with
existential type variables.

In this chapter you'll learn how to create an archive of data that you can
serialize and store on disk using existential types. In the next chapter you'll
continue with this project by learning how to build tools to decode heterogenous
data. In the following chapter you'll learn about mutable data. The information
you learn about existential data types in this chapter will be an important part
of understanding the ideas behind some types of mutable data.
