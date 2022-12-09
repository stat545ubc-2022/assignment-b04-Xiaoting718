# Assignment B04

Student: Xiaoting Li

# Assignment Option:

Option A -- Strings and functional programming in R:

-   Exercise 1

-   Exercise 2

# Introduction:

1.  `assignment_b04.Rmd` contains the source code of answer for Option A: Exercise 1 and Exercise 2.
2.  `assignment_b04.md` is generated from `assignment_b04.Rmd`, which is more readable on github.
3.  `assignment_b04_files` contains files that are auto-generated.

## New rules for Exercise 2:

1.  For words that begin with consonant sounds or consonant clusters:

    -   All letters before the first vowel are moved to the after of the first vowel, and the addition token `xtl` is added to the end. For example, `hello` will be `ehllo` + `xtl`.

2.  For words that begin with vowel sounds:

    -   The initial vowels and the first consonant or consonant cluster are kept, the rest of letters are removed. A different addition token `lxt` is added to the end of the word. For example, `emma` becomes `emm` + `lxt`.
