(ns visi.core.parser-test
  (:require [clojure.test :as t]
            [visi.core.parser :as vp]
            [visi.core.runtime :as vr]
            [visi.core.util :as vu]
            [instaparse.core :as insta]))

(defmacro deftest-pending [name & body]
  (let [message (str "\n===== " name " is pending.\n")]
    `(t/deftest ~name
       (println ~message))))

(t/deftest
  namespace-stuff
  (t/testing "does the namespace stuff work?")

  (t/is (= 42 (try
                (let [my-ns (clojure.main/with-bindings (ns test.dude) (find-ns 'test.dude))
                      eval-in-ns (fn [x] (clojure.main/with-bindings
                                          (in-ns 'test.dude)
                                          (eval x)))]
                  (some-> (vp/parse-line "x = 8; x + 1" my-ns {}) :res eval-in-ns)
                  (some-> (vp/parse-line "add_one(x) = x + 1" my-ns {}) :res eval-in-ns)
                  (-> (vp/parse-line "add_one(41)" my-ns {}) :res eval-in-ns))
                (finally (remove-ns 'test.dude))))))

(t/deftest
  test-parser
  (t/testing
   "Test test garbage syntax"
    (t/is (= (vp/parse-for-tests "1 + 2 honuh ++ ** 7 *2 ;8") nil)))

  (t/testing
   "Test number"

   (t/is (=
          (vp/parse-and-eval-for-tests "250,000")
          (int  (vp/parse-and-eval-for-tests "250,000.00"))
          250000))

    (t/is (=
           (vp/parse-and-eval-for-tests "3 + 4")
           (vp/parse-and-eval-for-tests "3+4")
           (vp/parse-and-eval-for-tests "3+ 4")
           (vp/parse-and-eval-for-tests "3 +4")
           (vp/parse-and-eval-for-tests "3++4")
           (vp/parse-and-eval-for-tests "+3 + +4")
           7))

    (t/is (= (vp/parse-for-tests "3 + -4") '(clojure.lang.Numbers/add 3 -4)))
    (t/is (= (vp/parse-for-tests "3 + -4.") '(clojure.lang.Numbers/add 3 -4.0)))
    (t/is (= (vp/parse-for-tests "-3 + -4.") '(clojure.lang.Numbers/add -3 -4.0)))
    (t/is (= (vp/parse-for-tests "-3 - -4") '(clojure.lang.Numbers/minus -3 -4))))

  (t/testing
   "Test NumberQualifier"
    (t/is (= (vp/parse-for-tests "2%") 1/50))
    (t/is (= (vp/parse-for-tests "2.%") 0.02))
    (t/is (= (vp/parse-for-tests "1#seconds") 1000))
    (t/is (= (vp/parse-for-tests "1.#minutes") 60000.0))
    (t/is (= (vp/parse-for-tests "1.0#hours") 3600000.0))
    (t/is (= (vp/parse-for-tests "1#days") 86400000))
    (t/is (= (vp/parse-and-eval-for-tests "1#seconds + 1#minutes") 61000)))

  (t/testing
   "Test Keyword"
  ;; keyword has the form 「:‹x›」. It is similar to clojure keyword. In visi syntax, often interchangable in places that allow identifier. It gets turned into clojure keyword.

    (t/is (= (vp/parse-for-tests "x25707:") :x25707))
    (t/is (= (vp/parse-for-tests "p40689:") :p40689)))

  (t/testing
      "Test StringLit." ; todo, test all the backslash special case
    ;; string literal syntax is 「"…"」
    ;; and also the following special forms
    ;; 「#""…""」
    ;; 「#"""…"""」
    ;; 「#''…''」
    ;; 「#'''…'''」
    ;; 「#^^…^^」
    ;; 「#^^^…^^^」
    ;; there must be 2 or more of the same delimiters in the beginning. Delimiter count at the end must be the same as beginning.

    (t/is (= (vp/parse-for-tests "\"3\"") "3"))
    (t/is (=
           (vp/parse-for-tests "\"3\n4\"")
           (vp/parse-for-tests "\"3
4\"")
           '"3\n4"))

    (t/is (=
           (vp/parse-for-tests "\"3\t4\"")
           (vp/parse-for-tests "\"3	4\"")
           '"3\t4"))

    (t/is (=
           (vp/parse-for-tests "#''foo\"dog
moose' cats'' "))
          "foo\"dog\nmoose' cats"
          )

    (t/is (=
           (vp/parse-for-tests "#'''h''\"^^\"^^'''")
           "h''\"^^\"^^")
          )

    (t/is (= (vp/parse-for-tests "\"😸\"") '"😸")); unicode beyond BMP
    )

  (t/testing
      "Test RegexLit"
    ;; regex syntax is this: 「#/‹regex›/」
    ;; the slash can be more than one, and the number of slashes in the end must be the same as beginning
    ;; the slash can also be underscore _ or vertical bar |
    (t/is (=
           (vp/parse-and-eval-for-tests "re_matches( #/a.+/, \"abc\")")
           (vp/parse-and-eval-for-tests "re_matches( #//a.+//, \"abc\")")
           (vp/parse-and-eval-for-tests "re_matches( #///a.+///, \"abc\")")
           "abc"))

    (t/is (=
           (vp/parse-and-eval-for-tests "re_matches( #_a.+_, \"abc\")")
           (vp/parse-and-eval-for-tests "re_matches( #__a.+__, \"abc\")")
           "abc"))

    (t/is (= (vp/parse-and-eval-for-tests "re_seq(#|t|, \"atatat\")") '("t" "t" "t")))
    (t/is (= (vp/parse-and-eval-for-tests "re_seq(#||x/*x||, \"||I like x//x, dude\")") (list  "x//x")))
    
    (t/is (= (vp/parse-and-eval-for-tests "re_matches( #/.文/, \"中文\")") "中文"))
    )

  (t/testing
   "Test operators OprExpression"

    (t/testing
     "Test arithmetic operators. + - * / ^"

      (t/is (=
             (vp/parse-for-tests "3 + y" 'y)
             (vp/parse-for-tests "3 + y" 'y)
             (vp/parse-for-tests "3 +y" 'y)
             '(clojure.lang.Numbers/add 3 y)))

      (t/is (= (vp/parse-for-tests "3 + y" 'y) '(clojure.lang.Numbers/add 3 y)))
      (t/is (= (vp/parse-for-tests "3 - 1") '(clojure.lang.Numbers/minus 3 1)))
      (t/is (= (vp/parse-for-tests "x * y" 'x 'y) '(clojure.lang.Numbers/multiply x y)))
      (t/is (= (vp/parse-for-tests "x / y" 'x 'y) '(clojure.lang.Numbers/divide x y)))
      (t/is (= (vp/parse-for-tests "3 ^ 2") '(java.lang.Math/pow 3 2)))

   ;; test int/float
      (t/is (= (vp/parse-and-eval-for-tests "3 +2 /4.")
               (vp/parse-and-eval-for-tests "3 +2. /4")
               (vp/parse-and-eval-for-tests "3. +2 /4")
               3.5))

   ;; test precedence. todo: need a full precedence test on all visi operators, including things like merge %% and string join operator &
      (t/is (= (vp/parse-and-eval-for-tests "3 +2 *2") 7))
      (t/is (= (vp/parse-and-eval-for-tests "2 *2 +3") 7))
      (t/is (= (vp/parse-and-eval-for-tests "3 +2 /4") 7/2))

      (t/is (= (vp/parse-and-eval-for-tests "3 ^2 /3")
               (vp/parse-and-eval-for-tests "(3 ^2) /3")
               '3.0))

      (t/is (= (vp/parse-and-eval-for-tests "3 ^(2 / 3)") '2.080083823051904))

      (t/is (= (vp/parse-and-eval-for-tests "(3 +2) /4") 5/4)))

    (t/testing
     "Test comparison operators < > <= >= != <> =="

      (t/is (= (vp/parse-for-tests "3 < 2") '(clojure.lang.Numbers/lt 3 2)))
      (t/is (= (vp/parse-for-tests "3 > 2") '(clojure.lang.Numbers/gt 3 2)))
      (t/is (= (vp/parse-for-tests "3 <= 2") '(clojure.lang.Numbers/lte 3 2)))
      (t/is (= (vp/parse-for-tests "3 >= 2") '(clojure.lang.Numbers/gte 3 2)))

      (t/is (=
             (vp/parse-for-tests "x != y" 'x 'y)
             (vp/parse-for-tests "x <> y" 'x 'y)
             '(not= x y)))

      (t/is (=
             (vp/parse-for-tests "x == y" 'x 'y) ; visi syntax may need a negation operator/function
             '(clojure.lang.Util/equiv x y))))

    (t/testing
     "Test logic operators and other operators"
      (t/is (=
             (vp/parse-for-tests "\"x\" & \"y\"") ; join string
             '(str "x" "y")))

      (t/is (= (vp/parse-for-tests "x && y" 'x 'y) '(let* [and x] (if and y and))))
      (t/is (= (vp/parse-for-tests "x || y" 'x 'y) '(let* [or x] (if or or y))))
      (t/is (= (vp/parse-for-tests "x %% y" 'x 'y) '(merge x y)))))

  (t/testing
   "Test ConstDef ConstDef1"
    (t/is (= (vp/parse-for-tests "x = 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x=3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x= 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests " x= 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests " x = 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x  =   3") '(def x 3)))

    (t/is (= (vp/parse-and-eval-for-tests "x = 3; x") 3)))

  (t/testing
   "Test BlockComment."
    (t/is (= (vp/parse-and-eval-for-tests "/* x y */ 3") 3))
    (t/is (= (vp/parse-and-eval-for-tests "/* x\ny */ 3") 3)) ;embedded line
    (t/is (= (vp/parse-and-eval-for-tests "/* \n\ny */ 3") 3)) ;embedded multiple lines
    (t/is (= (vp/parse-and-eval-for-tests "/* x\n\ny /* */ 3") 3)) ;embedded /*
  ;; (t/is (= (vp/parse-and-eval-for-tests "/* x\n\ny */ */ 3") 3)) ;embedded */
    (t/is (= (vp/parse-and-eval-for-tests "/* x\ny /* x\ny  */ */ 3") 3)); nested block comment
;
)

  (t/testing
   "Test BlockExpression."

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

    (t/is (= (vp/parse-for-tests "begin
    x = 4
    x + 1
end")
             '(do (let* [x 4] (clojure.lang.Numbers/add x 1))))))

  (t/testing
   "Test LineComment."
    (t/is (=
           (vp/parse-for-tests "3##")
           (vp/parse-for-tests "3## ")
           (vp/parse-for-tests "3 ##")
           (vp/parse-for-tests "3## 7")
           (vp/parse-for-tests "3 ## x = 4")
           (vp/parse-for-tests "3## \n")
           '3))

    (t/is (= (vp/parse-for-tests "1 + 2 ## 3 + 3") '(clojure.lang.Numbers/add 1 2))))

  (t/testing
   "Test URL"

    (t/is (= (vp/parse-for-tests "\"http://google.com/\"") '"http://google.com/"))
    (t/is (= (vp/parse-for-tests "\"https://google.com/\"") '"https://google.com/"))
    (t/is (= (vp/parse-for-tests "\"ftp://google.com/\"") '"ftp://google.com/"))
    (t/is (= (vp/parse-for-tests "\"file:///home/jane/x.html\"") '"file:///home/jane/x.html"))
    (t/is (= (vp/parse-for-tests "\"twtr://twitter.com/\"") '"twtr://twitter.com/")))

  (t/testing
   "Test FuncDef."
    (t/is (= (vp/parse-for-tests "f(x, y) = x + y")
             '(def f (fn* ([x y]
                           (clojure.lang.Numbers/add x y))))))

    (t/is (= (vp/parse-for-tests "f(x,y) = x +y")
             '(def f (fn* ([x y] (clojure.lang.Numbers/add x y))))))

    (t/is (= (vp/parse-for-tests "f(x)=3") '(def  f (fn* ([x] 3)))))

    (t/testing
     "Test FuncCall"
   ;; FuncCall has the form 「‹f›(‹x1›,‹x2›,…)」
      (t/is (=
             (vp/parse-for-tests "f(x)" 'x 'f)
             (vp/parse-for-tests " f(x)" 'x 'f)
             (vp/parse-for-tests "f(x)" 'x 'f)
             (vp/parse-for-tests "f( x)" 'x 'f)
             (vp/parse-for-tests "f(x )" 'x 'f)
             '(f x)))

      (t/is (=
             (vp/parse-for-tests "g(x,y)" 'x 'y 'g)
             (vp/parse-for-tests "g(x ,y)" 'x 'y 'g)
             (vp/parse-for-tests "g(x, y)" 'x 'y 'g)
             (vp/parse-for-tests "g(x,y )" 'x 'y 'g)
             '(g x y)))

      (t/is (=
             (vp/parse-and-eval-for-tests "f(x, y)=x+y; f(3,4)")
             7))

      (t/is (=
             (vp/parse-for-tests "f(x, y) = x + y; f(3,4)")
             '(let* [f (fn* ([x y] (clojure.lang.Numbers/add x y)))] (f 3 4))))

   ;; todo. find out the role of semicolon in visi. And indentation
      (t/is (=
             (vp/parse-for-tests "f(x) = x + 1; f(3)")
             (vp/parse-for-tests "f(x) = x + 1
  f(3)") ; line return also cause error
             '(let* [f (fn* ([x] (clojure.lang.Numbers/add x 1)))] (f 3))));
;
))

  (t/testing
   "Test Source syntax"

    (t/is (=
           (vp/parse-for-tests "source x")
           '(visi.core.runtime/visi-source x)))

    (t/is (=
           (vp/parse-for-tests "source xyz = \"https://example.com/x.txt\"")
           '(def xyz (visi.core.runtime/build-rdd-from-url (visi.core.runtime/spark-context) "https://example.com/x.txt"))))

    (t/is (=
           (vp/parse-for-tests "source x49519 = 7")
           '(def x49519 7))))

  (t/testing
   "Test SINK syntax"
    (t/is (=
           (vp/parse-for-tests "sink xx = 3")
           (vp/parse-for-tests "sink: xx = 3")
           '(def xx (do (visi.core.runtime/do-sink (quote xx) 3)
                        (visi.core.runtime/visi-realize 3))))))

  (t/testing
   "Test VectorExpr"
    (t/is (=
           (vp/parse-and-eval-for-tests "[3,4]")
           (vp/parse-and-eval-for-tests "[3 ,4]")
           (vp/parse-and-eval-for-tests "[3, 4]")
           (vp/parse-and-eval-for-tests "[3,4,]")
           (vp/parse-and-eval-for-tests "[3, 2 + 2]")
           '[3 4]))

    (t/is (= (vp/parse-and-eval-for-tests "[]") '[])))

  (t/testing
   "Test map data type, the MapExpr"
  ;; Map Expr has the form 「{‹key1› -> ‹value1›, ‹key2› -> ‹value2›, …}」, where the key is 「:‹x›」.  Note, syntaciallly, the key can also be string as in 「"x"」 or 「.x」, but these may not be semantically valid.
  ;; (get-parsetree "{:xx -> 3, :yy -> 4}")
  ;; (get-parsetree "{\"xx\" -> 3, \"yy\" -> 4}")
  ;; (get-parsetree "{.xx -> 3, .yy -> 4}")

    (comment
      "Test Pair syntax"
   ;; Pair = (DottedThing / EXPRESSION) <'->'> EXPRESSION;
   ;; pair has the form 「.‹x› -> ‹expr›」. (The ‹x› might possibly be 「"‹x›"」 too. todo.)
   ;; pair cannot be by itself accordig to grammar. The possible parent of Pair is the MergeExpr and MapExpr. So, test Pair there.
      (comment
        "Test DottedThing";; dotted thing has the form 「.x」.
;; When it is eval'd, it is interpreted as DotFuncExpr.
;; dotted thing cannot be by itself according to grammar spec. The only parent of dotted thing is Pair. So, test for pair instead.
))

  ;; test extra spaces
    (t/is (=
           (vp/parse-for-tests "x = {\"a\"  7 , \"b\" 8}")
           (vp/parse-for-tests "x = {\"a\" 7 , \"b\"  8}")
           (vp/parse-for-tests "x = {\"a\" 7 , \"b\"  8}")
           (vp/parse-for-tests "x = {\"a\" 7 , \"b\"  8}")
           (vp/parse-for-tests "x = {\"a\" 7, \"b\"  8}")
           (vp/parse-for-tests "x = {\"a\" 7 ,\"b\"  8}")
           '(def x {"a" 7, "b" 8})))

    (t/is (= (vp/parse-and-eval-for-tests "{y: -> 7}") '{:y 7}))
    (t/is (= (vp/parse-and-eval-for-tests "{.y -> 7}") '{:y 7}))
    (t/is (= (vp/parse-and-eval-for-tests "{\"y\" -> 7}") '{"y" 7}))

    (t/testing
     "Test FieldExpr. Retrieve map item."
   ;; FieldExpr have this form 「‹x› .‹y›」
   ;; it gets transformed to this form
   ;; (-> x (get :y))
   ;; so, its semantics is clojure function 「get」
   ;; so, it means the FieldExpr is for getting item from visi map datatype

      (t/is (= (vp/parse-for-tests "x .y" 'x)
               '(let* [it x]
                      (if (clojure.core/map? it)
                        (clojure.lang.RT/get it :y)
                        (.y it)))))

      (t/is (=
             (vp/parse-and-eval-for-tests "x = {y: -> 7}; x .y")
             (vp/parse-and-eval-for-tests "x = {y: 7}; x .y")
             (vp/parse-and-eval-for-tests "x = {.y -> 7}; x .y")
             7))

      (t/is (=
             (vp/parse-and-eval-for-tests "x = {y: -> 7}; x .y")
             (vp/parse-and-eval-for-tests "x = {y: 7}; x .y")
             (vp/parse-and-eval-for-tests "x = {.y -> 7}; x .y")
             7)))

    (t/testing
     "Test MergeExpr"

   ;; Merge Expr has the form 「‹expr› %% ‹pair›」,
   ;; where ‹pair› has the form 「‹x› -> ‹y›」.
   ;; 「x %% y」 get transformed into 「(merge x y)」
   ;; note: the right hand side needs not be a clojure list. todo, find out exactly why or what.

   ;; test syntactic validity
      (t/is (= (vp/parse-for-tests "x %% y" 'x 'y) '(merge x y)))

   ;; simple merge
      (t/is (=
             (vp/parse-for-tests "{\"a\" -> 7, \"b\" -> 8} %% \"x\" -> 3")
             '(merge {"a" 7, "b" 8} ["x" 3])))

   ;; test chained merge
      (t/is (=
             (vp/parse-for-tests "{\"a\" -> 7, \"b\" -> 8} %% \"x\" -> 3 %% \"y\" -> 3")
             '(merge (merge  {"a" 7, "b" 8} ["x" 3]) ["y" 3])))

   ;; test merge of 2 maps
      (t/is (=
             (vp/parse-for-tests "{\"a\" -> 7, \"b\" -> 8, \"c\" -> 9 } %% {\"x\" -> 3, \"b\" -> 2 }")
             '(merge {"a" 7, "b" 8, "c" 9} {"x" 3, "b" 2})))

      (t/is (=
             (vp/parse-and-eval-for-tests "{\"a\" -> 7, \"b\" -> 8, \"c\" -> 9 } %% {\"x\" -> 3, \"b\" -> 2 }")
             {"x" 3, "a" 7, "b" 2, "c" 9}))

      (t/is (=
             (vp/parse-for-tests "{.a -> 7, .b -> 8, .c -> 9 } %% {.x -> 3, .b -> 2 }")
             '(merge {:a 7, :b 8, :c 9} {:x 3, :b 2})))

      (t/is (=
             (vp/parse-and-eval-for-tests "{.a -> 7, .b -> 8, .c -> 9 } %% {.x -> 3, .b -> 2 }")
             {:x 3, :a 7, :b 2, :c 9}))))

  (t/testing
   "Test set data type SetExpr"

    (t/is (= (vp/parse-for-tests "#{}") '#{}))

    (t/is (=
           (vp/parse-for-tests "#{x,3,y}" 'x 'y)
           (vp/parse-for-tests "#{x ,3,y}" 'x 'y)
           (vp/parse-for-tests "#{x, 3,y}" 'x 'y)
           (vp/parse-for-tests "#{x,3,y,}" 'x 'y)
           (vp/parse-for-tests "#{x,3,y ,}" 'x 'y)
           (vp/parse-for-tests "#{x,3,y, }" 'x 'y)
           '#{x 3 y}))

    (t/is (=
           (vp/parse-for-tests "#{x,3,y}" 'x 'y)
           (vp/parse-for-tests "#{x ,3,y}" 'x 'y)
           (vp/parse-for-tests "#{x, 3,y}" 'x 'y)
           (vp/parse-for-tests "#{x,3,y,}" 'x 'y)
           (vp/parse-for-tests "#{x,3,y ,}" 'x 'y)
           (vp/parse-for-tests "#{x,3,y, }" 'x 'y)
           '#{x 3 y}))

    (t/is (= (vp/parse-and-eval-for-tests "#{4, 3, 7}") '#{7 3 4}));
)

  (t/testing
   "Test FunctionExpr"

  ;; <FunctionExpr> = HashFunctionExpr / PartialFunction / FunctionExpr1 / DotFuncExpr / Partial1 / Partial2 / Partial3

    (t/testing
     "Test HashFunctionExpr"
   ;; has the form 「|> # ‹x›(‹body›)」, where the ‹body› can contain 「it」, 「it1」, 「it2」, 「it3」. This basically means a lambda function. The ‹x› is a Java static method name, and 「it」 「it1」 means first arg, 「it2」 means 2nd arg, 「it」 means 3rd arg.
      (t/is (= (vp/parse-and-eval-for-tests "100 |> # `Math/log10(it)") '2.0))

      (t/is (= (vp/parse-and-eval-for-tests "qz = |> # `Math/log10(it) ; qz(100)") '2.0))

      (t/is (= (vp/parse-and-eval-for-tests "2 |> # `Math/pow(it, 3)") '8.0))

      (t/is (= (vp/parse-and-eval-for-tests "\"a\" |> # $.codePointAt(it,0)") '97));
)

    (t/testing
     "Test PartialFunction"
   ;; 「| ‹FuncCall›」

   ;; (get-transformed-result "| f(3,4)")
   ;; (partial f 3 4)

   ;; (get-parsetree "subs(\"abcd\", 1 , 3)")
   ;; (get-transformed-result "subs(\"abcd\", 1 , 3)")
   ;; (get-evaled-result "subs(\"abcd\", 1 , 3)") ; bc

   ;; (get-transformed-result "| subs(\"abcd\")")
   ;; (partial subs "abcd")

      (t/is (=
             (vp/parse-and-eval-for-tests "1 |> | subs(\"abcd\")")
             '"bcd"));
)

    (t/testing
     "Test FunctionExpr1"
   ;; function expression has this forms
   ;; ‹x› => ‹expr›
   ;; (‹x1›, ‹x2›, …) => ‹expr›

      (t/is (= (vp/parse-for-tests "x => 4") '(fn* ([x] 4))))
      (t/is (= (vp/parse-for-tests "(x,y) => 4") '(fn* ([x y] 4))))

      (t/is (= (vp/parse-for-tests "(x,y,z) => x + 1")
               (vp/parse-for-tests "(x ,y,z) => x + 1")
               (vp/parse-for-tests "(x, y,z) => x + 1")
               (vp/parse-for-tests "(x , y,z) => x + 1")
               (vp/parse-for-tests "(x , y ,z) => x + 1")
               '(fn* ([x y z] (clojure.lang.Numbers/add x 1)))))

      (t/is (= (vp/parse-and-eval-for-tests "f = (x,y) => x + y; f(3,4)") 7))
      (t/is (= (apply (vp/parse-and-eval-for-tests "x => x + 1") 3 '()) 4)) ; apply
      (t/is (= (vp/parse-and-eval-for-tests "apply((x) => x + 1, [4])") 5));
)

    (t/testing
     "Test DotFuncExpr"
   ;; DotFuncExpr has the form 「.‹x›」.
   ;; 「.‹x›」 get turned into a function of 1 arg, named 「.‹x›」.
   ;; and 「.‹x›(‹y›, …)」 get turned into  「(.‹x› ‹y› …)」.
   ;; this means, if the ‹x› is a java method name, then it works.

      (t/is
       (=
        (vp/parse-for-tests ".x")
        '(fn* ([it] (if (clojure.core/map? it) (clojure.lang.RT/get it :x) (.x it))))))

      (t/is (= (vp/parse-for-tests "$.dogmeat(4, 5)") '(.dogmeat 4 5)))
      (t/is (= (vp/parse-for-tests "dogmeat(4, 5)") '(.dogmeat 4 5)))

      (t/is (= (vp/parse-and-eval-for-tests "$.codePointAt(\"a\", 0)") '97))))

  (t/testing
   "Test pipe commands and expressions"

    (t/testing
     "Test Pipe2Expression"
   ;; 「‹expr› |> ‹FunctionExpr›」 can be chained.

      (t/is (=
             (vp/parse-and-eval-for-tests "3 |> (x) => x + 1")
             '4))

      (t/is (=
             (vp/parse-and-eval-for-tests "3 |> (x) => x + 1 |> (x) => x + 2")
             '6));;
)

    (t/testing
     "Test PipeExpression"
   ;; PipeExpression basically pass a argument to the visi pipecommands
      (t/is (= (vp/parse-and-eval-for-tests "x = [1, 4]; x |> map (+ 2)")
               (vp/parse-and-eval-for-tests " ([1, 4]) |> map (+ 2)")
               '(3 6)))

      (t/is (= (vp/parse-and-eval-for-tests "x = [\"CD\", \"AB\"]; x |> map (x) => $.toLowerCase(x)")
               '("cd" "ab")))

      (t/is (= (vp/parse-and-eval-for-tests "x = [\"CD\", \"AB\"]; x |> map .toLowerCase")
               '("cd" "ab")));; todo. PipeExpression also takes a ParenExpr in front. test that. In that form, it seems to take a function (because ParenExpr is a function)
)

    (t/testing
     "Test PipeFunctionExpression"
   ;; has the form 「|> ‹pipecommands›」. e.g. 「|> map (+ 1)」 it creates a function of 1 arg, this 「‹pipecommands›(arg)」

      (t/is (= (vp/parse-and-eval-for-tests "apply(|> map (+ 1), [[3,4,5]])")
               '(4 5 6))))

    (t/testing
     "Test PipeCommands"
   ;; all pipe commands's syntax must be part of PipeExpression or PipeFunctionExpression

      (t/testing
       "Test Mapcommand"
        (t/is (= (vp/parse-and-eval-for-tests
                  "x = [1, 2, 3]; y = 1; x |> map (+ y)")
                 '(2 3 4)))

        (t/is (= (vp/parse-and-eval-for-tests
                  "x = [1, 2, 3]; y = 1; x |> xform (+ y)")
                 '(2 3 4))))))

  (t/testing
   "Test IfElseExpr"
  ;; IfElseExpr has 3 forms.
  ;; ①  「if( ‹test›, ‹true body› , ‹false body›) 」
  ;;  note: no space after “if”
  ;; ② 「if ‹test› then ‹true body› else ‹else body›」
  ;; and a C-syntax
  ;; ③ 「(‹test› ? ‹true body› : ‹else body›)」

  ;; todo, for forms ② and ③, the ‹else body› also allow (OprExpression / EXPRESSION). Not sure why, perhaps for some precedence?
  ;; note: there's no just “if then” without “else”

  ;; test basic forms
    (t/is (=
           (vp/parse-for-tests "if( 3, 4, 5)")
           (vp/parse-for-tests "if 3 then 4 else 5")
           (vp/parse-for-tests "if 3
 then 4 else 5")
           (vp/parse-for-tests "if 3 then
 4 else 5")
           (vp/parse-for-tests "(3 ? 4 : 5)")
           (vp/parse-for-tests "3 ? 4 : 5")
           '(if 3 4 5)))
  ;; todo. add more test on extra space/newline variations in different places

    (t/is (=
           (vp/parse-and-eval-for-tests "if( 3, 4, 5)")
           (vp/parse-and-eval-for-tests "if 3 then 4 else 5")
           (vp/parse-and-eval-for-tests "(3 ? 4 : 5)")
           '4))

  ;; nested if
    (t/is (=
           (vp/parse-and-eval-for-tests "if( 3, if( 7, 4, 8), 5)")
           (vp/parse-and-eval-for-tests "if 3 then if 3 then 4 else 5 else 5")
           (vp/parse-and-eval-for-tests "(3 ? (7 ? 4 : 8) : 5)")
           '4)))

  (t/testing
   "Test GetExpression"
  ;; has the form 「‹x›[‹y1›]」, 「‹x›[‹y1›][‹y2›]」, 「‹x›[‹y1›][‹y2›][‹y3›]」, …. It can be used on vector data type and map data type

    (t/is (= (vp/parse-for-tests "x[2]" 'x)
             '(clojure.lang.RT/get x 2)))

    (t/is (= (vp/parse-for-tests "x[2][3]" 'x)
             '(clojure.lang.RT/get (clojure.lang.RT/get x 2) 3)))

    (t/is (= (vp/parse-and-eval-for-tests "x=[3]; x[0]")
             (vp/parse-and-eval-for-tests "x=[2,[3]]; x[1][0]")
             (vp/parse-and-eval-for-tests "x = {a: 3, b: 4}; x[a:]")
             '3)))

  (t/testing
   "Test ParenExpr"
  ;; ParenExpr has one of the following form
  ;; Partial1 has this form 「(‹operator›)」
  ;; Partial2 has this form 「(‹expr› ‹operator›) 」
  ;; Partial3 has this form 「(‹operator› ‹expr›)」
  ;; 「(‹expr›)」

  ;; here's the parse tree showing each form

  ;; (get-parsetree "(+)")
  ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ParenExpr [:Partial1 [:Operator [:Op3 "+"]]]]]]]

  ;; (get-parsetree "(3+)")
  ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ParenExpr [:Partial2 [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "3"]]]] [:Operator [:Op3 "+"]]]]]]]

  ;; (get-parsetree "(+3)")
  ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ParenExpr [:Partial3 [:Operator [:Op3 "+"]] [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "3"]]]]]]]]]

  ;; (get-parsetree "(3)")
  ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ParenExpr [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "3"]]]]]]]]

  ;; here's their transformed result
  ;; (get-transformed-result "(+)")
  ;; '+

  ;; (get-transformed-result "(3+)")
  ;; '(fn [x__36__auto__] (+ 3 x__36__auto__))

  ;; (get-transformed-result "(+3)")
  ;; '(fn [x__37__auto__] (+ x__37__auto__ 3))

  ;; (get-transformed-result "(3)")
  ;; '3

  ;; the ParenExpr seems to serve 2 purposes.
  ;; ① when using paren to specify eval order
  ;; ② when in the form 「(‹expr› ‹operator›) 」 , it creates a function that behaves like 「‹expr› ‹operator› ‹arg›」.
  ;; Similarly,  when  in the form 「(‹operator› ‹expr›)」 , it creates a function that behaves like 「‹arg› ‹operator› ‹expr›」.

  ;; here's example for eval order

  ;; (get-parsetree "(3+2)")
  ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ParenExpr [:EXPRESSION [:EXPRESSION2 [:OprExpression [:Op10Exp [:Op9Exp [:Op8Exp [:Op7Exp [:Op6Exp [:Op5Exp [:Op4Exp [:Op3Exp [:Op2Exp [:Op1Exp [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "3"]]]]]] [:Op3 "+"] [:Op2Exp [:Op1Exp [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "2"]]]]]]]]]]]]]]]]]]]]]

  ;; (get-parsetree "(3+2)/4")
  ;; [:Line [:EXPRESSION [:EXPRESSION2 [:OprExpression [:Op10Exp [:Op9Exp [:Op8Exp [:Op7Exp [:Op6Exp [:Op5Exp [:Op4Exp [:Op3Exp [:Op2Exp [:Op1Exp [:EXPRESSION [:EXPRESSION2 [:ParenExpr [:EXPRESSION [:EXPRESSION2 [:OprExpression [:Op10Exp [:Op9Exp [:Op8Exp [:Op7Exp [:Op6Exp [:Op5Exp [:Op4Exp [:Op3Exp [:Op2Exp [:Op1Exp [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "3"]]]]]] [:Op3 "+"] [:Op2Exp [:Op1Exp [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "2"]]]]]]]]]]]]]]]]]]]]] [:Op2 "/"] [:Op1Exp [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "4"]]]]]]]]]]]]]]]]]]

    (t/is (= ((vp/parse-and-eval-for-tests "(+)") 3 4) 7))

    (t/is (= ((vp/parse-and-eval-for-tests "(3 +)") 4) 7))

    (t/is (= ((vp/parse-and-eval-for-tests "(/ 3)") 4) 4/3))

    (t/is (= (vp/parse-and-eval-for-tests "(3)") 3));
)

  (t/testing
   "Test InlineFunc"

  ;; inline func has the form 「‹def›; ‹expr›」
  ;; where the ‹def› is either a constant definition such as 「x=3」 or function definition such as 「f(x) = x+1 」
  ;; inline func is part of grammar rule EXPRESSION

  ;; (get-transformed-result "x = 3; x")
  ;; [:InlineFunc '(def x 3) 'x]

  ;; (get-transformed-result "x = 3; y = 4; 5")
  ;; [:InlineFunc '(def x 3) [:InlineFunc '(def y 4) '5]]

  ;; (get-transformed-result "f(x)=3; x")
  ;; [:InlineFunc '(defn f [x] 3) 'x]

  ;; (get-parsetree "x = 3; x")
  ;; [:Line
  ;;  [:EXPRESSION
  ;;   [:EXPRESSION2
  ;;    [:InlineFunc
  ;;     [:ConstDef
  ;;      [:ConstDef1
  ;;       [:IDENTIFIER "x"]
  ;;       [:EXPRESSION
  ;;        [:EXPRESSION2
  ;;         [:ConstExpr
  ;;          [:Number "3"]]]]]]
  ;;     [:EXPRESSION
  ;;      [:EXPRESSION2
  ;;       [:IDENTIFIER "x"]]]]]]]

  ;; (get-parsetree "f(x)=3; x")
  ;; [:Line
  ;;  [:EXPRESSION
  ;;   [:EXPRESSION2
  ;;    [:InlineFunc
  ;;     [:FuncDef
  ;;      [:IDENTIFIER "f"]
  ;;      [:IDENTIFIER "x"]
  ;;      [:EXPRESSION
  ;;       [:EXPRESSION2
  ;;        [:ConstExpr
  ;;         [:Number "3"]]]]]
  ;;     [:EXPRESSION
  ;;      [:EXPRESSION2
  ;;       [:IDENTIFIER "x"]]]]]]]

  ;; (get-parsetree "f(x)=3; f(4)")
  ;; [:Line
  ;;  [:EXPRESSION
  ;;   [:EXPRESSION2
  ;;    [:InlineFunc
  ;;     [:FuncDef
  ;;      [:IDENTIFIER "f"]
  ;;      [:IDENTIFIER "x"]
  ;;      [:EXPRESSION
  ;;       [:EXPRESSION2
  ;;        [:ConstExpr
  ;;         [:Number "3"]]]]]
  ;;     [:EXPRESSION
  ;;      [:EXPRESSION2
  ;;       [:FuncCall
  ;;        [:IDENTIFIER "f"]
  ;;        [:EXPRESSION
  ;;         [:EXPRESSION2
  ;;          [:ConstExpr
  ;;           [:Number "4"]]]]]]]]]]]

    (t/is (= (vp/parse-for-tests "x = 3; x")
             '(let* [x 3] x)))

    (t/is (= (vp/parse-for-tests "f(x)=3; x", 'x)
             '(let* [f (fn* ([x] 3))] x)))

    (t/is (= (vp/parse-and-eval-for-tests "f(x)=3; f(4)")
             3)))

  (t/testing
   "Test ClojureSymbol"

  ;; ClojureSymbol is like IDENTIFIER. The diff is that identifier only allow alphanumerics, plus dash underline and question mark. But clojure symbol is intended to be clojure identifiers, including dot slash, and other allowed chars of clojure symbol.

    (t/is (= (vp/parse-for-tests "x/y", 'x 'y)
             '(clojure.lang.Numbers/divide x y)))

    (t/is (= (vp/parse-for-tests "x.y/b" 'x 'b)
             '(clojure.lang.Numbers/divide
               (let* [it x]
                     (if (clojure.core/map? it)
                       (clojure.lang.RT/get it :y)
                       (.y it)))
               b)))

    (t/is (= (vp/parse-for-tests "x/y/z" 'x 'y 'z)
             '(clojure.lang.Numbers/divide x (clojure.lang.Numbers/divide y z)))) ; this becomes division

    (t/is (= (vp/parse-for-tests "x / y / z/a" 'x 'y 'z 'a)
             '(clojure.lang.Numbers/divide
               x
               (clojure.lang.Numbers/divide
                y
                (clojure.lang.Numbers/divide z a))))) ; nested division

    (t/is (= (vp/parse-for-tests "x/y(m)" 'x 'y 'm)
             '(clojure.lang.Numbers/divide x (y m))))

    (t/is (= (vp/parse-and-eval-for-tests "`clojure.core/list(3,4,5)")
             '(3 4 5))))

  (t/testing
   "Test parser, misc"

    (t/is (= (vp/parse-for-tests "x=[3]; 2")
             '(let* [x [3]] 2)))

    (t/is (= (vp/parse-and-eval-for-tests
              "x = [1, 2, 3]; map( (+ 1), x)")
             '(2 3 4)))

    (t/is (= (vp/parse-and-eval-for-tests
              "x = [1, 2, 3]; y = 1; map( (+ y), x)")
             '(2 3 4)))

    (t/is (= (vp/parse-and-eval-for-tests
              "x = [1, 2, 3]; y = 1; x |> map (+ y)")
             '(2 3 4)))

    (t/is (= (vp/parse-and-eval-for-tests "for([y, [1, 2, 3]], x = [1, 2, 3]; x |> map (+ y))")
             '((2 3 4) (3 4 5) (4 5 6))));
);;
)

(t/deftest
  test-Sortcommand
  "Test Sortcommand"
  ;; sort command has the form
  ;; 「sort ‹x›」
  ;; 「sort ‹x›,ascending」 (note, no space after comma. (todo FIXME?))
  ;; 「sort ‹x›,declaration」
  ;; the ‹x› is one of IDENTIFIER, Keyword, FunctionExpr.
  ;; sort command is part of pipecommands, meaning, it must be part of PipeExpression or PipeFunctionExpression, meaning, the syntax must have 「… |>」  or 「|>」 in front
  ;; like this「data |> sort func」

  (t/is (=
         (vp/parse-and-eval-for-tests "x = [8, 3, 4]; x |> sort identity")
         (vp/parse-and-eval-for-tests "ff(aa) = identity(aa); x = [8, 3, 4]; x |> sort ff")
         (vp/parse-and-eval-for-tests "ff = identity; x = [8, 3, 4]; x |> sort ff")
         (vp/parse-and-eval-for-tests "([8, 3, 4]) |> sort identity")
         [3,4,8]));; todo. need to look at clojup sorted map, its relation to clojure sort and sort-by functions, and what visi's sort do with map data, and visi's data types
)

(t/deftest
  test-FieldExpr-2
  ;; should this work?
  (t/is (=
         (vp/parse-and-eval-for-tests "({.y -> 7}) |> .y")
         (vp/parse-and-eval-for-tests "apply(.y, [{.y -> 7}])")
         (vp/parse-and-eval-for-tests "invoke(.y, {.y -> 7})")
         7)))

(t/deftest
  func-aliases
  (t/is (= (vp/parse-and-eval-for-tests "join(\",\", [1, 2, 3])")
           "1,2,3"))
  (t/is (= (vp/parse-and-eval-for-tests "if_not(true, 44, 55)")
           55)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch pad

;; 2015-01-24
;; look into the “re$-” part, and visi namespace
;; (vp/parse-and-eval-for-tests "re$-matches( #/a.+/, \"abc\")")
;; find out exactly how visi namespace works. seems “re$” or “re$-” is the visi syntax for calling the regex lib
;; write test for it in the namespace test section

;; todo, write test, or look into the following grammar rules
;; IDENTIFIER, NamespaceName, Namespace, Requires, Import, Load
;; Samplecommand , Dropcommand,
;; FieldField, ForceField, MethodMethod, ForceMethod,
;; Reducecommand.

    ;; "Test Flatmapcommand" ; todo
    ;; 「flatmap ‹x›」
    ;;  name alias: xform-cat, mapcat, flatmap

    ;; (get-parsetree "x = {:a -> 3, :b -> 4}; x |> flatmap :b")
    ;; (get-transformed-result "x = {:a -> 3, :b -> 4}; x |> flatmap :b")
    ;; (vp/parse-and-eval-for-tests "x = {:a -> 3, :b -> 4}; x |> flatmap :b") ; todo

    ;; "Test Groupbycommand" ; todo
    ;; 「group ‹x›」
    ;; 「group by ‹x›」
    ;; (get-parsetree "x = {:a -> 3, :b -> 4}; x |> group :b")
    ;; (get-transformed-result "x = {:a -> 3, :b -> 4}; x |> group :b")
    ;; (vp/parse-and-eval-for-tests "x = {:a -> 3, :b -> 4}; x |> group :b") ; todo

    ;; "Test Filtercommand" ;todo
    ;; 「filter ‹x›」
    ;; (get-parsetree "x = {:a -> 3, :b -> 4}; x |> filter :b")
    ;; (get-transformed-result "x = {:a -> 3, :b -> 4}; x |> filter :b")
    ;; (vp/parse-and-eval-for-tests "x = {:a -> 3, :b -> 4}; x |> filter :b") ; not implemented? todo.

  ;; "Test Foldcommand" ; todo

  ;; 「fold ‹IDENTIFIER›」
  ;; 「fold ‹FunctionExpr›」

  ;; 「fold ‹vector› -> ‹FunctionExpr›」
  ;; 「fold ‹map› -> ‹FunctionExpr›」
  ;; 「fold ‹ParenExpr› -> ‹FunctionExpr›」
  ;; 「fold ‹ConstExpr› -> ‹FunctionExpr›」

  ;; (get-parsetree "|> fold x")
  ;; [:Line [:EXPRESSION [:PipeFunctionExpression [:Foldcommand [:IDENTIFIER "x"]]]]]

  ;; (get-parsetree "|> fold [4, 3, 2] -> (x,y) => 4 ")
