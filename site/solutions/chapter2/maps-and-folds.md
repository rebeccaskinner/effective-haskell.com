---
chapter: 2
exercise-id: 4
name: Thinking About Maps and Folds
summary: "
The second exercise in chapter 2
"
---

### Thinking About Maps and Folds {.problem}

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
