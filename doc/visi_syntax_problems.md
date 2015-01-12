# Visi Syntax Problems
2015-01-02


# misc


<!-- # Backslash Character Issue -->

<!-- according to java spec http://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-EscapeSequence -->
<!-- “It is a compile-time error if the character following a backslash in an escape sequence is not an ASCII b, t, n, f, r, ", ', \, 0, 1, 2, 3, 4, 5, 6, or 7. The Unicode escape \u is processed earlier (§3.3).” -->

<!-- Clojure's string syntax follow Java. -->
<!-- In Java `3\0` means char 3 followed by the char with ascii code 0 (which is the null char) -->
<!-- So `\9` would be a string with 1 tab char -->
<!-- in Visi, `\9` gets turned into `\\9`. This seems to be by design -->
