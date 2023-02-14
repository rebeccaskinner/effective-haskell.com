---
chapter: 2
exercise-id: 3
name: Implementing concatMap
summary: "
The third exercise in chapter 2
"
---

### Implementing concatMap {.problem}


The `concat` function joins a list of lists into a single
list:

```
concat [[1,2,3],[4,5,6]]
[1,2,3,4,5,6]
```

Prelude provides a function named `concatMap` that can be
naively implemented by composing `concat` and
`map`:

```haskell
concatMap f = concat . map f
```

Try implementing `concatMap` using
`foldl` and `foldr`.  Which one is
better? Why?

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
