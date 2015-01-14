;; (ns visi.core-test
;;   (:require [clojure.test :as t]
;;             [visi.core.parser :as vp]
;;             ))

(ns visi.core-test
  (:require [clojure.test :as t]
            [visi.core.parser :as vp]
            [visi.core.runtime :as vr]
            [visi.core.util :as vu]
            ))

;; 2014-12-06 TODO need to replace vp/pre-process-line by something else, preferably a evaluated result

(t/deftest test-parser
           (t/testing
            "Test visi parser. ConstDef"
            (t/is (= (vp/parse-for-tests "x = 3") '(def x 3)))
            (t/is (= (vp/parse-for-tests "x  =   3") '(def x 3)))
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
            (t/is (= (vp/parse-for-tests "f(x, y) = x + y") '(defn f [x y] (+ x y))))
            (t/is (= (vp/parse-for-tests "f(x,y) = x+y") '(defn f [x y] (+ x y))))
            (t/is (= (vp/parse-for-tests "f(x)=3") '(defn f [x] 3))))

           (t/testing
            "Test visi parser. source syntax"

            (t/is (=
                   (vp/parse-for-tests "source x52548")
                   '(visi.core.runtime/source x52548)))

            (t/is (=
                   (vp/parse-for-tests "source xyz = \"https://example.com/x.txt\"")
                   '(visi.core.runtime/source xyz "https://example.com/x.txt")))

            ;; (t/is (=
            ;;        (vp/pre-process-line "source 9")
            ;;        "source 9"))

            (t/is (=
                   (vp/parse-for-tests "source x49519 = 7")
                   '(visi.core.runtime/source x49519 7)))

            ;; todo
            ;; source ‹name› = ‹EXPRESSION›
            ;; needs a lot variations

            )

           (t/testing
            "Test visi parser. sink syntax"
            (t/is (=
                   (vp/pre-process-line "sink x25599 = y52942")
                   "(def x25599 (do (visi.core.runtime/do-sink (quote x25599) y52942) y52942))"))
            (t/is (=
                   (vp/pre-process-line "sink: x60473 = y90940")
                   "(def x60473 (do (visi.core.runtime/do-sink (quote x60473) y90940) y90940))")))

           ;; (t/testing
           ;;  "Test visi parser. pipe expression"
           ;;  ;; todo
           ;;  (t/is (=
           ;;         (vp/pre-process-line "info |> map .toLowerCase")
           ;;         "(def lower (.cache (as-> info x__8942__auto__ (visi.core.runtime/v-map x__8942__auto__ (fn [z__8941__auto__] (.toLowerCase z__8941__auto__))))))"))

           ;;  ;; (def lower
           ;;  ;;      (.cache
           ;;  ;;       (as-> info x__8942__auto__
           ;;  ;;             (visi.core.runtime/v-map x__8942__auto__
           ;;  ;;                                 (fn [z__8941__auto__]
           ;;  ;;                                     (.toLowerCase z__8941__auto__))))))

           ;;  ;; (t/is (=
           ;;  ;;        (vp/pre-process-line "sins = lower |> filter # (.contains(it, \"sin\") && not(.contains(it, \"sing\")))")
           ;;  ;;        "(def sins (as-> lower x__8942__auto__ (visi.core.runtime/v-filter x__8942__auto__ (fn [it] (and (.contains it \"sin\") (not (.contains it \"sing\")))))))"))

           ;;  ;; sins-plus-god-or-christ = sins |> filter # begin

           ;;  ;; twit = v/stream-into-watching((v/create-twitter-stream({:duration -> 5000})) |> map .getText |> map calc_sent |> filter # (1 < it.pos || 1 < it.neg) |> reduce | merge-with((+)))

           ;;  ;; lower-bible = (bible |> map .toLowerCase) >> # .cache(it)

           ;;  ;; wc = (lower-bible |> mapcat # .split(it, "\\W+")) >> # v/v-map-to-pair(it, # [it, 1] ) >> # v/v-reduce-by-key(it, (+))

           ;;  )

           (t/testing
            "Test visi parser. sink syntax"
            (t/is (=
                   (vp/pre-process-line "sink x25599 = y52942")
                   "(def x25599 (do (visi.core.runtime/do-sink (quote x25599) y52942) y52942))"))
            (t/is (=
                   (vp/pre-process-line "sink: x60473 = y90940")
                   "(def x60473 (do (visi.core.runtime/do-sink (quote x60473) y90940) y90940))")))

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
                   "(def x [])")))

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
