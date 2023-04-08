---
chapter: 6
name: Using the DefaultSignatures Extension
summary: "
When writing type classes, you'll sometimes find yourself in facing an
unfortunate choice. There are default instances that you can provide for some
functions in your class, but doing so requires an extra constraint. If the
constraint is something common, do you make it a requirement, force users to
write their own functions, or provide some utility function outside of the class
they can use? One solution to this problem is to use the DefaultSignatures
extension to add additional constraints to the default implementation of a
function in your class.
"
---

## Using the DefaultSignatures Extension

The [Default
Signatures](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/default_signatures.html)
extension
