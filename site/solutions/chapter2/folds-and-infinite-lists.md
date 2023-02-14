---
chapter: 2
exercise-id: 5
name: Folds and Infinite Lists
summary: "
The final exercise in chapter 2
"
---

### Folds and Infinite Lists {.problem}

Think about the following two lines of code that use
`map` and `foldr`.  When might
they do the same thing?  When might they differ?  How might that change if you
used `foldl` instead of `foldr`?

```
λ \f g -> foldr g 0 . map f
λ \f g -> foldr (g . f) 0
```
### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>

</details>
</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

</details>
</div>
