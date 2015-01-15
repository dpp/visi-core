# Visi Syntax Problems

# parse error (vp/parse-for-tests " // x = 3")

    (vp/parse-for-tests " // x = 3")

    expected: (= (vp/parse-for-tests " // x = 3") nil)
    actual: clojure.lang.ArityException: Wrong number of args (6) passed to: core/identity


# number quantifier syntax

this works

    1..seconds

this doesn't

    1.seconds

This situation isn't intuitive. Same for other number quantifiers except %. The cause seems to be that the parser eats the dot as float number.

# grammar: Line rule doesn't allow space in front

    (vp/line-parser "x = 4\n")

    Parse error at line 2, column 1:
    
    ^
    Expected one of:
    #"[a-zA-Z][a-zA-Z0-9\-_\?]*"
    "\n"
    "//"
    "/*"
    "\t"
    " "

Not a visi syntax problem. But the grammar 

    Line = ((BlockComment <'\n'>) | LineComment)* (SINK / Def / Source / (EXPRESSION LineEnd*));

should allow this case.

# grammar problem: LineComment in front of Line

    Line = ((BlockComment <'\n'>) | LineComment)* (SINK / Def / Source / (EXPRESSION LineEnd*));

the LineComment in front doesn't make sense.

# misc


<!-- # Backslash Character Issue -->

<!-- according to java spec http://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-EscapeSequence -->
<!-- “It is a compile-time error if the character following a backslash in an escape sequence is not an ASCII b, t, n, f, r, ", ', \, 0, 1, 2, 3, 4, 5, 6, or 7. The Unicode escape \u is processed earlier (§3.3).” -->

<!-- Clojure's string syntax follow Java. -->
<!-- In Java `3\0` means char 3 followed by the char with ascii code 0 (which is the null char) -->
<!-- So `\9` would be a string with 1 tab char -->
<!-- in Visi, `\9` gets turned into `\\9`. This seems to be by design -->

the grammar spec doesn't seems clean. For example:

    <LineEnd> = <'\n'>+ | (<';'> SPACES*)

ideally, it should be just a semicolon. There seems a lot room to improve here. 

example, this

    (insta/parses vp/line-parser "x = 4\n" :partial true )

visi.core-test=> (insta/parses vparser "x = 4\n" :partial true)

StackOverflowError   clojure.lang.PersistentHashMap.hash (PersistentHashMap.java:120)
visi.core-test=> 
