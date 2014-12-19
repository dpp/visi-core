(ns visi.core-test
  (:require [clojure.test :as t]
            [visi.core.parser :as vp]
            ))

;; 2014-12-06 TODO need to replace vp/pre-process-line by something else, preferably a evaluated result

(t/deftest test-parser
           (t/testing
            "Test visi parser. ConstDef"
            (t/is (= (vp/pre-process-line "x = 3") "(def x 3)"))
            (t/is (= (vp/pre-process-line "x  =   3") "(def x 3)"))
            ;; (t/is (= (vp/pre-process-line "x= 3") "(def x 3)"))
            ;; (t/is (= (vp/pre-process-line "x =3") "(def x 3)"))
            ;; (t/is (= (vp/pre-process-line "x=3") "(def x 3)"))

            )

           (t/testing
            "Test visi parser. block comment"
            (t/is (= (vp/pre-process-line "/* x y */") "/* x y */"))
            (t/is (= (vp/pre-process-line "/* x
y */") "/* x
y */")))

           (t/testing
            "Test visi parser. Line comment"
            (t/is (= (vp/pre-process-line "  ;8") ";8")))

           (t/testing
            "Test visi parser. Function definition"
            (t/is (= (vp/pre-process-line "f(x, y) = x + y") "(defn f [x y] (+ x y))"))
            (t/is (= (vp/pre-process-line "f(x) = 3") "(defn f [x] 3)")))

           (t/testing
            "Test visi parser. source syntax"

            (t/is (=
                   (vp/pre-process-line "source x52548")
                   "(visi.runtime/source x52548)"))

            (t/is (=
                   (vp/pre-process-line "source xyz = \"https://example.com/x.txt\"")
                   "(visi.runtime/source xyz \"https://example.com/x.txt\")"))

            (t/is (=
                   (vp/pre-process-line "source 9")
                   "source 9"))

            (t/is (=
                   (vp/pre-process-line "source x49519 = 7")
                   "(visi.runtime/source x49519 7)"))

            ;; todo
            ;; source ‹name› = ‹EXPRESSION›
            ;; needs a lot variations

            )

           (t/testing
            "Test visi parser. sink syntax"
            (t/is (=
                   (vp/pre-process-line "sink x25599 = y52942")
                   "(def x25599 (do (visi.runtime/do-sink (quote x25599) y52942) y52942))"))
            (t/is (=
                   (vp/pre-process-line "sink: x60473 = y90940")
                   "(def x60473 (do (visi.runtime/do-sink (quote x60473) y90940) y90940))")))

           ;; (t/testing
           ;;  "Test visi parser. pipe expression"
           ;;  ;; todo
           ;;  (t/is (=
           ;;         (vp/pre-process-line "info |> map .toLowerCase")
           ;;         "(def lower (.cache (as-> info x__8942__auto__ (visi.runtime/v-map x__8942__auto__ (fn [z__8941__auto__] (.toLowerCase z__8941__auto__))))))"))

           ;;  ;; (def lower
           ;;  ;;      (.cache
           ;;  ;;       (as-> info x__8942__auto__
           ;;  ;;             (visi.runtime/v-map x__8942__auto__
           ;;  ;;                                 (fn [z__8941__auto__]
           ;;  ;;                                     (.toLowerCase z__8941__auto__))))))

           ;;  ;; (t/is (=
           ;;  ;;        (vp/pre-process-line "sins = lower |> filter # (.contains(it, \"sin\") && not(.contains(it, \"sing\")))")
           ;;  ;;        "(def sins (as-> lower x__8942__auto__ (visi.runtime/v-filter x__8942__auto__ (fn [it] (and (.contains it \"sin\") (not (.contains it \"sing\")))))))"))

           ;;  ;; sins-plus-god-or-christ = sins |> filter # begin

           ;;  ;; twit = v/stream-into-watching((v/create-twitter-stream({:duration -> 5000})) |> map .getText |> map calc_sent |> filter # (1 < it.pos || 1 < it.neg) |> reduce | merge-with((+)))

           ;;  ;; lower-bible = (bible |> map .toLowerCase) >> # .cache(it)

           ;;  ;; wc = (lower-bible |> mapcat # .split(it, "\\W+")) >> # v/v-map-to-pair(it, # [it, 1] ) >> # v/v-reduce-by-key(it, (+))

           ;;  )

           (t/testing
            "Test visi parser. sink syntax"
            (t/is (=
                   (vp/pre-process-line "sink x25599 = y52942")
                   "(def x25599 (do (visi.runtime/do-sink (quote x25599) y52942) y52942))"))
            (t/is (=
                   (vp/pre-process-line "sink: x60473 = y90940")
                   "(def x60473 (do (visi.runtime/do-sink (quote x60473) y90940) y90940))")))

           (t/testing
            "Test visi parser. operator"

            (t/is (=
                   (vp/pre-process-line "3 + y")
                   "(+ 3 y)"))

            (t/is (=
                   (vp/pre-process-line "3 - 1")
                   "(- 3 1)"))

            (t/is (=
                   (vp/pre-process-line "x * y")
                   "(* x y)"))

            (t/is (=
                   (vp/pre-process-line "x / y")
                   "(/ x y)"))

            (t/is (=
                   (vp/pre-process-line "3 ^ 2")
                   "(Math/pow 3 2)"))

            (t/is (=
                   (vp/pre-process-line "3 < 2")
                   "(< 3 2)"))

            (t/is (=
                   (vp/pre-process-line "3 > 2")
                   "(> 3 2)"))

            (t/is (=
                   (vp/pre-process-line "3 <= 2")
                   "(<= 3 2)"))

            (t/is (=
                   (vp/pre-process-line "3 >= 2")
                   "(>= 3 2)"))

            (t/is (=
                   (vp/pre-process-line "x != y")
                   (vp/pre-process-line "x <> y")
                   "(not= x y)"))

            (t/is (=
                   (vp/pre-process-line "\"x\" & \"y\"")
                   "(str \"x\" \"y\")"))

            (t/is (=
                   (vp/pre-process-line "x %% y")
                   "(merge x y)"))

            ;; (t/is (=
            ;;        (vp/pre-process-line "{:a 3} %% {:b 4}") ;todo not sure if this actually valid.
            ;;        "(merge {:a 3} {:b 4})"))

            ;;

            )

           (t/testing
            "Test visi parser. vector"

            (t/is (=
                   (vp/pre-process-line "x = [3,4]")
                   "(def x [3 4])"
                   ))

            (t/is (=
                   (vp/pre-process-line "x = []")
                   "x = []")))

           (t/testing
            "Test visi parser. map"

            ;; todo this works
            ;; oo = {"xx" -> 3, "yy" -> 4}
            ;; but this doesn't work
            ;; {"xx" -> 3, "yy" -> 4}
            ;; CompilerException java.lang.RuntimeException: Can't take value of a macro: #'clojure.core/->, compiling:(/tmp/form-init1224820726145481961.clj:1:113)
            ;; one is turned into clojure

            (t/is (=
                   (vp/pre-process-line "x = {\"a\" -> 7 , \"b\" -> 8}")
                   "(def x {\"a\" 7, \"b\" 8})")))

           (t/testing
            "Test visi parser. set"

            (t/is (=
                   (vp/pre-process-line "#{}")
                   "#{}"))

            (t/is (=
                   (vp/pre-process-line "#{x y}")
                   "#{x y}")))

           (t/testing
            "Test visi parser. string"
            (t/is (=
                   (vp/pre-process-line "\"3\"")
                   "\"3\""))

            (t/is (=
                   (vp/pre-process-line "3
4")
                   "3\n4"))

            (t/is (=
                   (vp/pre-process-line "\"😸\"") ; unicode beyond BMP
                   "\"😸\"")))

           (t/testing
            "Test visi parser. number"
            (t/is (=
                   (vp/pre-process-line "3 + +4")
                   "(+ 3 4)"))

            (t/is (=
                   (vp/pre-process-line "3 + -4")
                   "(+ 3 -4)"))

            (t/is (=
                   (vp/pre-process-line "3 + -4.")
                   "(+ 3 -4.0)"))

            (t/is (=
                   (vp/pre-process-line "-3 + -4.")
                   "(+ -3 -4.0)"))

            (t/is (=
                   (vp/pre-process-line "-3 - -4")
                   "(- -3 -4)")))

           (t/testing
            "Test visi parser. number quantifier"
            (t/is (=
                   (vp/pre-process-line "2%")
                   "1/50"))

            (t/is (=
                   (vp/pre-process-line "2.%")
                   "0.02"))

            ;; (t/is (= ; should work but doesn't. ; NumberFormatException Invalid number: 1.seconds clojure.lang.LispReader.readNumber (LispReader.java:256)

            ;;        (vp/pre-process-line "1.seconds")
            ;;        "1000"))
            )

           ;;
           )

;; todo. this is error without space
;; x+1
;; this is a problem

;; todo. this is error without space
;; 3^2
;; this is a problem

;; todo. this is error
;; hhh = {:aa 7 :b 8}
;; CompilerException java.lang.RuntimeException: Unable to resolve symbol: hhh in this context, compiling:(/tmp/form-init3541756566004116888.clj:1:113)

            ;; according to java spec http://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-EscapeSequence
;; It is a compile-time error if the character following a backslash in an escape sequence is not an ASCII b, t, n, f, r, ", ', \, 0, 1, 2, 3, 4, 5, 6, or 7. The Unicode escape \u is processed earlier (§3.3).
            ;; clojure's string syntax follow Java.
            ;; in java "3\0" means char 3 followed by the char with ascii code 0 (which is the null char)
            ;; so "\9" would be a string with 1 tab char
            ;; in visi, "\9" gets turned into "\\9". This seems to be by design
  ;

;; there are several problems with array, vector, map, set
;; basically,
;; clojure has this syntax
;;  {:a 7, :b 8}
;; but visi has this syntax
;;  {"a" -> 7, "b" -> 8}
;; the problem is that, if a line is just clojure, it works, but if in visi syntax, it doesn't because parser sees the opening bracket and think it's clojure
;; on the other hand,
;; x = {"a" -> 7, "b" -> 8}
;; works but
;; x = {:a 7, :b 8}
;; doesn't
;; also, when doing

;; todo
;; the syntax for line comment is lisp convention
;; but for block comment is C, Java convension
;; doesn't seem to be good

;; todo
;; the visi lang probably should be shielded from clojure
;; and have a designed way to access clojure, as clojure with java
;; because, otherwise, there's quite a few syntax mishmash. Confusing to use together and hard to specify to user and implement
