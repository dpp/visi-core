# Visi Syntax Problems
2015-01-02

# Space Around Operator Are Required
This is error without space

    x+1
    3*2

Same for other operators. Simple arithmetic such as `x+1` that doesn't allow whitespace are likely to annoy users.

# Mixed Clojure/Visi Array Syntax Problem

There are several problems with array, vector, map, set
Clojure has this syntax

    {:a 7, :b 8}

But visi has this syntax

    {"a" -> 7, "b" -> 8}

The problem is that, if a line is just clojure, it works, but if in visi syntax, it doesn't because parser sees the opening bracket and think it's clojure
on the other hand,

    x = {"a" -> 7, "b" -> 8}

works but

    x = {:a 7, :b 8}

doesn't.

todo. this is error
hhh = {:aa 7 :b 8}
CompilerException java.lang.RuntimeException: Unable to resolve symbol: hhh in this context, compiling:(/tmp/form-init3541756566004116888.clj:1:113)

# Semicolon Syntax Problem

- Visi uses 1 semicolon `;` as line seperators.
- Visi uses 2 semicolons `;;` as line comment.
- The syntax for line comment is lisp convention of using semicolon `;`.
- But for block comment is C, Java convension `/* ... */`

This may be confusing to users. For example,

    (+ 3 4) ; something

works. Where 1 semicolon acts as comment. But

    3 + 4 ; something

doesn't.

Also,

    3 + 4 ;; something

is a comment but

    x = 4; y = 3 ;

... todo

todo; the following should work according to visi grammar, but doesn't

    x = 4;
    y = 3;

error is `Unable to resolve symbol: y in this context`

# Block Comment Doesn't Work
The block comment `/* ... */` doesn't seem to work at all.

# Clojure and Visi Conflicts
The Visi lang probably should be shielded from Clojure,
or have a designed syntax to access Clojure, as Clojure with Java.
Because, otherwise, there's quite a few syntax mishmash. Confusing to use together, hard to implement correctly, and complex to document for user.

# Missing Comma Problem

In an array, a missing comma does not cause error. It should. Example:

    [7, 2 "something"]

    x = [7, 2 "something"]

# Empty Array/Map Problem

1. Empty array is not an accepted Visi syntax. It probably should be.
2. Empty array does not generate any error. It should.

Example:

    x = []

# SetExpr

SetExpr is defined in grammar as isolated rule, having no effect.

# misc

• when in visi notebook, pressing Control+Shift+Enter (as if by mistake), then, Shift+Enter won't eval cell anymore, instead, it'll turn a cell into a inert cell. Findout what's going on here. todo

• in Visi pipe commands, such as {xform, map}, {fold, reduce}, {xform-cot, mapcat, flatmap} etc, can we remove the synonyms?

• the visi syntax, i can't see as excel syntax. e.g. the vector, map, piping. How is it following excel, or, is there a example online of excel that can show me similarity?
e.g. i see here excell formulas
https://support.office.com/en-au/article/Overview-of-formulas-d258ec72-149a-42ac-8eae-b50a667eb491
Example: PRODUCT(C1:C2)

<!-- # Backslash Character Issue -->

<!-- according to java spec http://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-EscapeSequence -->
<!-- “It is a compile-time error if the character following a backslash in an escape sequence is not an ASCII b, t, n, f, r, ", ', \, 0, 1, 2, 3, 4, 5, 6, or 7. The Unicode escape \u is processed earlier (§3.3).” -->

<!-- Clojure's string syntax follow Java. -->
<!-- In Java `3\0` means char 3 followed by the char with ascii code 0 (which is the null char) -->
<!-- So `\9` would be a string with 1 tab char -->
<!-- in Visi, `\9` gets turned into `\\9`. This seems to be by design -->

