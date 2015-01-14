# Visi Syntax Problems
2015-01-02

# line comment doesn't work

    (vp/parse-for-tests " // 3ttttt")

    (t/is (= (vp/parse-for-tests "//") nil))
    (t/is (= (vp/parse-for-tests "// ") nil))
    (t/is (= (vp/parse-for-tests "// \n") nil))
    (t/is (= (vp/parse-for-tests "// 3") nil))
    (t/is (= (vp/parse-for-tests "1 + 2 // 3 + 3") '(+ 1 2)))
    
    (vp/line-parser "//\n\n" :unhide :all)
    (vp/line-parser "// \n" :unhide :all)
    (vp/line-parser "// 3\n" :unhide :all)
    (vp/line-parser " // 3\n" :unhide :all)
    (vp/line-parser "// x\n" :unhide :all)
    (vp/line-parser "// x =\n" :unhide :all)
    (vp/line-parser "// x = 4\n" :unhide :all)
    
    (insta/parses vp/line-parser "//\n\n" :unhide :all)
    (insta/parses vp/line-parser "//\n\n" :unhide :all :total true) ; infinite loop


# extra space in front of line line doesn't work

    (vp/line-parser " x = 4\n")

    Parse error at line 2, column 1:
    
    ^
    Expected one of:
    #"[a-zA-Z][a-zA-Z0-9\-_\?]*"
    "\n"
    "//"
    "/*"
    "\t"
    " "

# misc


<!-- # Backslash Character Issue -->

<!-- according to java spec http://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-EscapeSequence -->
<!-- “It is a compile-time error if the character following a backslash in an escape sequence is not an ASCII b, t, n, f, r, ", ', \, 0, 1, 2, 3, 4, 5, 6, or 7. The Unicode escape \u is processed earlier (§3.3).” -->

<!-- Clojure's string syntax follow Java. -->
<!-- In Java `3\0` means char 3 followed by the char with ascii code 0 (which is the null char) -->
<!-- So `\9` would be a string with 1 tab char -->
<!-- in Visi, `\9` gets turned into `\\9`. This seems to be by design -->
