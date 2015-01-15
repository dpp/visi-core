(ns visi.core.parser-test
  (:require [clojure.test :as t]
            [visi.core.parser :as vp]
            [visi.core.runtime :as vr]
            [visi.core.util :as vu]
            [instaparse.core :as insta]
            ))

(t/deftest
  test-parser

  ;; warning: garbage syntax returns nil. todo, see
  (t/testing
      "Test test garbage syntax"
    (t/is (= (vp/parse-for-tests "1 + 2 honuh ++ ** 7 *2 ;8") nil)))

  (t/testing
      "Test number"

    (t/is (=
           (vp/parse-and-eval-for-tests "3 + 4")
           (vp/parse-and-eval-for-tests "3 + +4")
           (vp/parse-and-eval-for-tests "+3 + +4")
           7))

    (t/is (= (vp/parse-for-tests "3 + -4") '(+ 3 -4)))

    (t/is (= (vp/parse-for-tests "3 + -4.") '(+ 3 -4.0)))

    (t/is (= (vp/parse-for-tests "-3 + -4.") '(+ -3 -4.0)))

    (t/is (= (vp/parse-for-tests "-3 - -4") '(- -3 -4))))

  (t/testing
      "Test number quantifier"
    (t/is (= (vp/parse-for-tests "2%") '1/50))
    (t/is (= (vp/parse-for-tests "2.%") '0.02))
    (t/is (= (vp/parse-for-tests "1#seconds") 1000))
    (t/is (= (vp/parse-for-tests "1.#minutes") '60000.0))
    (t/is (= (vp/parse-for-tests "1.0#hours") '3600000.0))
    (t/is (= (vp/parse-for-tests "1#days") 86400000))
    (t/is (= (vp/parse-and-eval-for-tests "1#seconds + 1#minutes") 61000))
    )

  (t/testing
      "Test string"
    (t/is (= (vp/parse-for-tests "\"3\"") '"3"))

    (t/is (=
           (vp/parse-for-tests "\"3
4\"")
           '"3\n4"))

    (t/is (=
           (vp/parse-for-tests "\"😸\"") ; unicode beyond BMP
           '"😸")))

  (t/testing
      "Test operator"

    (t/is (= (vp/parse-for-tests "3 + y") '(+ 3 y)))

    (t/is (= (vp/parse-for-tests "3 - 1") '(- 3 1)))

    (t/is (= (vp/parse-for-tests "x * y") '(* x y)))

    (t/is (= (vp/parse-for-tests "x / y") '(/ x y)))

    (t/is (= (vp/parse-for-tests "3 ^ 2") '(Math/pow 3 2)))

    (t/is (= (vp/parse-for-tests "3 < 2") '(< 3 2)))

    (t/is (= (vp/parse-for-tests "3 > 2") '(> 3 2)))

    (t/is (= (vp/parse-for-tests "3 <= 2") '(<= 3 2)))

    (t/is (= (vp/parse-for-tests "3 >= 2") '(>= 3 2)))

    (t/is (=
           (vp/parse-for-tests "x != y")
           (vp/parse-for-tests "x <> y")
           '(not= x y)))

    (t/is (=
           (vp/parse-for-tests "\"x\" & \"y\"")
           '(str "x" "y")))

    (t/is (= (vp/parse-for-tests "x %% y") '(merge x y)))

    ;; (t/is (=
    ;;        (vp/parse-for-tests "{:a 3} %% {:b 4}") ;todo not sure if this actually valid.
    ;;        '(merge {:a 3} {:b 4})))

    ;;

    )

  (t/testing
      "Test ConstDef constant definition"
    (t/is (= (vp/parse-for-tests "x = 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x=3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x= 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests " x= 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests " x = 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x  =   3") '(def x 3)))
    )

  (t/testing
      "Test block comment"

    (t/is (= (vp/parse-for-tests "/* x y */") nil))

    ;; embedded line
    (t/is (= (vp/parse-for-tests "/* x\ny */") nil))

    ;; embedded multiple lines
    (t/is (= (vp/parse-for-tests "/* \n\ny */") nil))

    ;; embedded /*
    (t/is (= (vp/parse-for-tests "/* x\n\ny /* */") nil))

    ;; embedded */
    (t/is (= (vp/parse-for-tests "/* x\n\ny */ */") nil))

    ;; nested block comment
    (t/is (= (vp/parse-for-tests "/* x\ny /* x\ny  */ */") nil)))

;; FIXME
  (t/testing
      "Test block expression"

    (t/is (= (vp/parse-for-tests "begin
 4
end")
             '(do 4)))

    (t/is (= (vp/parse-for-tests "begin
 4;
end")
             '(do 4)))

    (t/is (= (vp/parse-for-tests "begin
 4;
 7;
end")
             '(do 4 7)))

    ;; (t/is (= (vp/parse-for-tests "begin
    ;; x = 4;
    ;; end")
    ;;           '(do (def x 4))))

    )

  (t/testing
      "Test Line comment"
      (t/is (=
             (vp/parse-for-tests "//")
             (vp/parse-for-tests "// ")
             (vp/parse-for-tests " //")
             (vp/parse-for-tests "// \n")
             nil))

      (t/is (= (vp/parse-for-tests "2// 3") '2))
      (t/is (= (vp/parse-for-tests "1 + 2 // 3 + 3") '(+ 1 2)))
      ;; (t/is (= (vp/parse-for-tests " // x = 3") nil)) ; FIXME this is a java exception
    )

  (t/testing
      "Test Function definition"
    (t/is (= (vp/parse-for-tests "f(x, y) = x + y") '(defn f [x y] (+ x y))))
    (t/is (= (vp/parse-for-tests "f(x,y) = x+y") '(defn f [x y] (+ x y))))
    (t/is (= (vp/parse-for-tests "f(x)=3") '(defn f [x] 3))))

  (t/testing
      "Test source syntax"

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
      "Test sink syntax"
    (t/is (=
           (vp/pre-process-line "sink x25599 = y52942")
           "(def x25599 (do (visi.core.runtime/do-sink (quote x25599) y52942) y52942))"))
    (t/is (=
           (vp/pre-process-line "sink: x60473 = y90940")
           "(def x60473 (do (visi.core.runtime/do-sink (quote x60473) y90940) y90940))")))

  (t/testing
      "Test vector"

    (t/is (= (vp/parse-for-tests "x = [3,4]") '(def x [3 4])))

    (t/is (= (vp/parse-for-tests "x = []") '(def x []))))

  (t/testing
      "Test map data type"

    (t/is (=
           (vp/parse-for-tests "x = {\"a\" -> 7 , \"b\" -> 8}")
           (vp/parse-for-tests "x = {\"a\"-> 7 , \"b\" -> 8}")
           (vp/parse-for-tests "x = {\"a\" ->7 , \"b\" -> 8}")
           (vp/parse-for-tests "x = {\"a\"->7 , \"b\" -> 8}")
           (vp/parse-for-tests "x = {\"a\" -> 7, \"b\" -> 8}")
           (vp/parse-for-tests "x = {\"a\" -> 7 ,\"b\" -> 8}")
           '(def x {"a" 7, "b" 8})))

    (t/is (=
           (vp/parse-for-tests "x = {y -> 7}")
           '(def x {y 7})))

    (t/is (=
           (vp/parse-for-tests "x = {.y -> 7}")
           '(def x {:y 7})))

    ;; todo. need to figure out a proper eval test for this. set a var to map data type, then retrieve one element using the var.
    ;; some' like this
    ;; x = {.y -> 7}; x[.y]
    ;; need to figure out the semantics of the diff of
    ;; {y -> 8}
    ;; {.y -> 8}
    ;; {"y" -> 8}

    ;; (t/is (=
    ;;        (vp/parse-and-eval-for-tests "x = {.y -> 8}")
    ;;        #'visi.core.parser-test/x))

    ;; todo. needed to look into DottedThing
    (t/is (=
           (vp/parse-for-tests "x = {.xx -> 3}")
           '(def x {:xx 3})
           ))

    ;; (vp/parse-for-tests "x = {m -> 7}")
    ;; (vp/parse-for-tests "{m -> 7}")
    ;; (vp/parse-for-tests "{\"m\" -> 7}")


   ;;  MapExpr = SPACES? <'{'> (Pair <','>)* Pair (<','> SPACES?)? <'}'> SPACES?;
   ;; :MapExpr (fn [& x] (into {} x))

    ;; (insta/parse (insta/parser vp/parse-def :start :Line) "{\"xx\" -> 3, \"yy\" -> 4}")

    ;; [:Line
    ;;  [:EXPRESSION
    ;;   [:EXPRESSION2
    ;;    [:MapExpr
    ;;     [:Pair
    ;;      [:EXPRESSION
    ;;       [:EXPRESSION2
    ;;        [:ConstExpr
    ;;         [:StringLit "\"xx\""]]]]
    ;;      [:EXPRESSION
    ;;       [:EXPRESSION2
    ;;        [:ConstExpr
    ;;         [:Number "3"]]]]]
    ;;     [:Pair
    ;;      [:EXPRESSION
    ;;       [:EXPRESSION2
    ;;        [:ConstExpr
    ;;         [:StringLit "\"yy\""]]]]
    ;;      [:EXPRESSION
    ;;       [:EXPRESSION2
    ;;        [:ConstExpr
    ;;         [:Number "4"]]]]]]]]]

    ;; todo this works
    ;; oo = {"xx" -> 3, "yy" -> 4}
    ;; but this doesn't work
    ;; {"xx" -> 3, "yy" -> 4}
    ;; CompilerException java.lang.RuntimeException: Can't take value of a macro: #'clojure.core/->, compiling:(/tmp/form-init1224820726145481961.clj:1:113)
    ;; one is turned into clojure

    )

  (t/testing
      "Test set data type"

    (t/is (= (vp/parse-for-tests "#{}") '#{}))

    (t/is (= (vp/parse-for-tests "#{x, y}") '#{x y})))

  ;; (t/testing
  ;;  "Test pipe expression"
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
      "Test :GetExpression"                 ; todo
    ;; GetExpression = SPACES? IDENTIFIER (<'['> EXPRESSION <']'>)+ SPACES?
    ;; :GetExpression (fn [a & b] `(~'-> ~a ~@(map (fn [z] `(~'get ~z)) b)))

    (t/is (= (vp/parse-for-tests "x[2]") '(-> x (get 2))))

    (t/is (= (vp/parse-for-tests "x = [3,4,5]; x[2]")
             '(clojure.core/let [x [3 4 5]] (-> x (get 2)))))

    ;; todo. need cases for map and other data type

    (t/is (= (vp/parse-for-tests "x=[3]; x[2]")
             '(clojure.core/let [x [3]] (-> x (get 2)))))
    )

  (t/testing
      "Test parser, misc"

    (t/is (= (vp/parse-for-tests "x=[3]; 2")
             '(clojure.core/let [x [3]] 2)))

  ;; :Pair (fn [a b] `[~a ~b])
  ;; Pair = (DottedThing / EXPRESSION) <'->'> EXPRESSION;
    ;; (vp/parse-for-tests "x -> 2")

    )

  (t/testing
      "Test evaluation"
    (t/is (= (vp/parse-and-eval-for-tests
              "x = [1, 2, 3]; map( (+ 1), x)")
             (list 2 3 4))))

  ;;
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch pad

;; (require 'visi.core.parser-test)
;; visi.core.parser-test/vparser

;; (defn vparser
;;   "visi parser."
;;   []
;;   (insta/parser vp/parse-def :start :Line))

;; (defn get-parsetree
;;   "returns the parse tree."
;;   [code]
;;   (insta/parse vparser code))

;; (defn get-transformed-result
;;   "returns."
;;   [parse-tree]
;;   (insta/transform vp/xform-rules parse-tree))

;; ;; get parse tree
;; (insta/parse (insta/parser vp/parse-def :start :Line) "1+2")

;; ;; get transformed result
;; (insta/transform vp/xform-rules (insta/parse (insta/parser vp/parse-def :start :Line) "1+2"))
