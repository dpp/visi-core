(ns visi.core.parser-test
  (:require [clojure.test :as t]
            [visi.core.parser :as vp]
            [visi.core.runtime :as vr]
            [visi.core.util :as vu]
            [instaparse.core :as insta]
            ))

(def vparser (insta/parser vp/parse-def :start :Line))
(defn get-parsetree [code] (insta/parse vparser code))
(defn get-transformed-result [code] (insta/transform vp/xform-rules (insta/parse vparser code)))
(defn get-evaled-result [code] (eval (insta/transform vp/xform-rules (insta/parse vparser code))))

(t/deftest
 test-parser

 ;; todo. warning: garbage syntax fed to vp/parse-for-tests returns nil. Need to find a way that also check parser error, either write a function using instaparse, or retrieve from vp/parse-line. This needs to be applied to all tests.
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
  (t/is (= (vp/parse-and-eval-for-tests "1#seconds + 1#minutes") 61000)))

 (t/testing
  "Test Keyword"
  ;; keyword has the form 「:‹x›」. It is similar to clojure keyword. In visi syntax, often interchangable in places that allow identifier. It gets turned into clojure keyword.

  (t/is (= (vp/parse-for-tests ":x25707") ':x25707))
  (t/is (= (vp/parse-for-tests ":p40689") ':p40689))

  ;; note: keywords cannot happen together, such as 「:x :y」, unlike Clojure.
  ;; (get-parsetree ":x :y") ; parse error
  ;; (get-parsetree "x y") ; parse error
  ;; (get-parsetree "3 4") ; parse error
  ;; (get-parsetree "\"a\" \"b\"") ; parse error

  )

 (t/testing
  "Test string StringLit." ; todo, test all the backslash special case
  (t/is (= (vp/parse-for-tests "\"3\"") '"3"))
  (t/is (= (vp/parse-for-tests "\"3\n4\"") '"3\n4"))
  (t/is (= (vp/parse-for-tests "\"😸\"") '"😸"))) ;unicode beyond BMP

 (t/testing
  "Test RegexLit"
  ;; FIXME: the visi regex syntax doesn't allow slash. This means, user cannot search any string that contains slash, possibly unless they use embedded Unicode char syntax

  (t/is (= (vp/parse-and-eval-for-tests "re-matches( #/a.+/, \"abc\")") "abc" ))
  (t/is (= (vp/parse-and-eval-for-tests "re-matches( #/a/, \"b\")") nil ))
  (t/is (= (vp/parse-and-eval-for-tests "re-matches( #/.文/, \"中文\")") "中文" )))

 (t/testing
  "Test operators OprExpression"

  (t/testing
   "Test arithmetic operators. + - * / ^"

   (t/is (=
          (vp/parse-for-tests "3 + y")
          (vp/parse-for-tests "3+ y")
          (vp/parse-for-tests "3 +y")
          '(+ 3 y)))

   (t/is (= (vp/parse-for-tests "3 + y") '(+ 3 y)))
   (t/is (= (vp/parse-for-tests "3 - 1") '(- 3 1)))
   (t/is (= (vp/parse-for-tests "x * y") '(* x y)))
   (t/is (= (vp/parse-for-tests "x / y") '(/ x y)))
   (t/is (= (vp/parse-for-tests "3 ^ 2") '(Math/pow 3 2)))

   ;; test int/float
   (t/is (= (vp/parse-and-eval-for-tests "3+2/4.")
            (vp/parse-and-eval-for-tests "3+2./4")
            (vp/parse-and-eval-for-tests "3.+2/4")
            3.5))

   ;; test precedence. todo: need a full precedence test on all visi operators, including things like merge %% and string join operator &
   (t/is (= (vp/parse-and-eval-for-tests "3+2*2") 7))
   (t/is (= (vp/parse-and-eval-for-tests "2*2+3") 7))
   (t/is (= (vp/parse-and-eval-for-tests "3+2/4") 7/2))
   (t/is (= (vp/parse-and-eval-for-tests "(3+2)/4") 5/4)))

  (t/testing
   "Test comparison operators < > <= >= != <> =="

   (t/is (= (vp/parse-for-tests "3 < 2") '(< 3 2)))
   (t/is (= (vp/parse-for-tests "3 > 2") '(> 3 2)))
   (t/is (= (vp/parse-for-tests "3 <= 2") '(<= 3 2)))
   (t/is (= (vp/parse-for-tests "3 >= 2") '(>= 3 2)))

   (t/is (=
          (vp/parse-for-tests "x != y")
          (vp/parse-for-tests "x <> y")
          '(not= x y)))

   (t/is (=
          (vp/parse-for-tests "x == y") ; visi syntax may need a negation operator/function
          '(= x y))))

  (t/testing
   "Test logic operators and other operators"
   (t/is (=
          (vp/parse-for-tests "\"x\" & \"y\"") ; join string
          '(str "x" "y")))

   (t/is (= (vp/parse-for-tests "x && y") '(and x y)))
   (t/is (= (vp/parse-for-tests "x || y") '(or x y)))
   (t/is (= (vp/parse-for-tests "x %% y") '(merge x y)))))

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

  (t/is (= (vp/parse-for-tests "/* x y */") nil))
  (t/is (= (vp/parse-for-tests "/* x\ny */") nil)) ;embedded line
  (t/is (= (vp/parse-for-tests "/* \n\ny */") nil)) ;embedded multiple lines
  (t/is (= (vp/parse-for-tests "/* x\n\ny /* */") nil)) ;embedded /*
  (t/is (= (vp/parse-for-tests "/* x\n\ny */ */") nil)) ;embedded */
  (t/is (= (vp/parse-for-tests "/* x\ny /* x\ny  */ */") nil))) ;nested block comment

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
x = 4;
  end")
           '(do (def x 4)))))

 (t/testing
  "Test LineComment."
  (t/is (=
         (vp/parse-for-tests "##")
         (vp/parse-for-tests "## ")
         (vp/parse-for-tests " ##")
         (vp/parse-for-tests "## \n")
         nil))

  (t/is (= (vp/parse-for-tests "2## 3") '2))
  (t/is (= (vp/parse-for-tests "1 + 2 ## 3 + 3") '(+ 1 2)))
  (t/is (= (vp/parse-for-tests " ## x = 3") nil)) ; todo. need to check parse error instead of nil
  )

 (t/testing
  "Test URL"

  (t/is (= (vp/parse-for-tests "\"http://google.com/\"") '"http://google.com/"))
  (t/is (= (vp/parse-for-tests "\"https://google.com/\"") '"https://google.com/"))
  (t/is (= (vp/parse-for-tests "\"ftp://google.com/\"") '"ftp://google.com/"))
  (t/is (= (vp/parse-for-tests "\"file:///home/jane/x.html\"") '"file:///home/jane/x.html"))
  (t/is (= (vp/parse-for-tests "\"twtr://twitter.com/\"") '"twtr://twitter.com/")))

 (t/testing
  "Test FuncDef."
  (t/is (= (vp/parse-for-tests "f(x, y) = x + y") '(defn f [x y] (+ x y))))
  (t/is (= (vp/parse-for-tests "f(x,y) = x+y") '(defn f [x y] (+ x y))))
  (t/is (= (vp/parse-for-tests "f(x)=3") '(defn f [x] 3)))

  (t/testing
   "Test FuncCall"

   (t/is (=
          (vp/parse-for-tests "f(x)")
          (vp/parse-for-tests " f(x)")
          (vp/parse-for-tests "f (x)")
          (vp/parse-for-tests "f( x)")
          (vp/parse-for-tests "f(x )")
          '(f x)))

   (t/is (=
          (vp/parse-for-tests "g(x,y)")
          (vp/parse-for-tests "g(x ,y)")
          (vp/parse-for-tests "g(x, y)")
          (vp/parse-for-tests "g(x,y )")
          '(g x y)))

   (t/is (=
          (vp/parse-and-eval-for-tests "f(x, y) = x + y; f(3,4)")
          '7 ))

   (t/is (=
          (vp/parse-for-tests "f(x, y) = x + y; f(3,4)")
          '(clojure.core/let [f (clojure.core/fn [x y] (+ x y))] (f 3 4))))

   (t/is (=
          (vp/parse-for-tests "f(x, y) = x + y;f(3,4)")
          '(clojure.core/let [f (clojure.core/fn [x y] (+ x y))] (f 3 4)))) ; missing a space after semicolon cause error

   ))

 (t/testing
  "Test Source syntax" ; todo

  (t/is (=
         (vp/parse-for-tests "source x52548")
         '(visi.core.runtime/source x52548)))

  (t/is (=
         (vp/parse-for-tests "source xyz = \"https://example.com/x.txt\"")
         '(visi.core.runtime/source xyz "https://example.com/x.txt")))

  (t/is (=
         (vp/pre-process-line "source 9")
         "source 9")) ; todo. needs error reporting

  (t/is (=
         (vp/parse-for-tests "source x49519 = 7")
         '(visi.core.runtime/source x49519 7))))

 (t/testing
  "Test SINK syntax" ; todo

  (t/is (=
         (vp/pre-process-line "sink x25599 = y52942")
         "(def x25599 (do (visi.core.runtime/do-sink (quote x25599) y52942) y52942))"))
  (t/is (=
         (vp/pre-process-line "sink: x60473 = y90940")
         "(def x60473 (do (visi.core.runtime/do-sink (quote x60473) y90940) y90940))")))

 (t/testing
  "Test VectorExpr"

  (t/is (= (vp/parse-for-tests "x = [3,4]") '(def x [3 4])))

  (t/is (= (vp/parse-for-tests "x = []") '(def x []))))

 (t/testing
  "Test map data type, the MapExpr"
  ;; Map Expr has the form 「{‹key1› -> ‹value1›, ‹key2› -> ‹value2›, …}」, where the key is any of 「"‹x›"」, 「".‹x›"」, and possibly something else. Most likely, only 「"‹x›"」 is semantically valid form for key
  ;; (get-parsetree "{\"xx\" -> 3, \"yy\" -> 4}")
  ;; (get-parsetree "{.xx -> 3, .yy -> 4}")

  (comment
   "Test Pair syntax"
   ;; Pair = (DottedThing / EXPRESSION) <'->'> EXPRESSION;
   ;; pair has the form 「.‹x› -> ‹expr›」. (The ‹x› might possibly be 「"‹x›"」 too. todo.)
   ;; pair cannot be by itself accordig to grammar. The possible parent of Pair is the MergeExpr and MapExpr. So, test Pair there.
   (comment
    "Test DottedThing"
    ;; dotted thing has the form 「.x」.
    ;; When it is eval'd, it is interpreted as DotFuncExpr.
    ;; dotted thing cannot be by itself according to grammar spec. The only parent of dotted thing is Pair. So, test for pair instead.
    ))

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

  (t/is (=
         (vp/parse-for-tests "x = {\"y\" -> 7}")
         '(def x {"y" 7})))

  ;; todo.
  ;; find the proper semantically valid form, one of
  ;; {y -> 8}
  ;; {.y -> 8}
  ;; {"y" -> 8}
  ;; see test for FieldExpr

  ;; todo. set visi map to var, then retrieve a field. see FieldExpr test

  (t/testing
   "Test FieldExpr. Retrieve map item."
   ;; FieldExpr have this form 「‹x› .‹y›」
   ;; it gets transformed to this form
   ;; (-> x (get :y))
   ;; so, its semantics is clojure function 「get」
   ;; so, it means the FieldExpr is for getting item from visi map datatype

   (t/is (= (vp/parse-for-tests "x .y") '(-> x (get :y))))

   ;; this doesn't make sense
   (t/is (= (vp/parse-for-tests "x = {\"y\" -> 7}; x .y")
            '(clojure.core/let [x {"y" 7}] (-> x (get :y)))))

   ;; question: find out just exactly what's a key in visi's map data type. FieldExpr implies that it is .key , but MapExpr allows both .key and "key". But only the .key form makes sense when used with FieldExpr
   (t/is (= (vp/parse-for-tests "x = {.y -> 7}; x .y")
            '(clojure.core/let [x {:y 7}] (-> x (get :y)))))
   ;; quostion: why is this in a “let”? isn't assgnment global?

   (t/is (= (vp/parse-and-eval-for-tests "x = {.y -> 7}; x .y")
            '7)))

  (t/testing
   "Test MergeExpr"

   ;; Merge Expr has the form 「‹expr› %% ‹pair›」,
   ;; where ‹pair› has the form 「‹x› -> ‹y›」.
   ;; 「x %% y」 get transformed into 「(merge x y)」
   ;; note: the right hand side needs not be a clojure list. todo, find out exactly why or what.

   ;; test syntactic validity
   (t/is (= (vp/parse-for-tests "x %% y") '(merge x y)))

   ;; simple merge
   (t/is (=
          (vp/parse-for-tests "{\"a\" -> 7, \"b\" -> 8} %% \"x\" -> 3")
          '(merge {"a" 7, "b" 8} ["x" 3])))

   ;; test chained merge
   (t/is (=
          (vp/parse-for-tests "{\"a\" -> 7, \"b\" -> 8} %% \"x\" -> 3 %% \"y\" -> 3")
          '(merge (merge {"a" 7, "b" 8} ["x" 3]) ["y" 3])))

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
  "Test set data type SetExpr" ; todo

  (t/is (= (vp/parse-for-tests "#{}") '#{}))

  (t/is (= (vp/parse-for-tests "#{x, y}") '#{x y})))

 (t/testing
  "Test FunctionExpr"

  ;; <FunctionExpr> = HashFunctionExpr / PartialFunction / FunctionExpr1 / DotFuncExpr / Partial1 / Partial2 / Partial3

  (t/testing "Test HashFunctionExpr" ;todo
             )

  (t/testing "Test PartialFunction" ;todo
             ;; Partial1 ;; Partial2 ;; Partial3
             )

  (t/testing
   "Test FunctionExpr1"
   ;; function expression has this forms
   ;; ‹x› => ‹expr›
   ;; (‹x1›, ‹x2›, …) => ‹expr›

   (t/is (= (vp/parse-for-tests "f => 4") '(fn [f] 4)))
   (t/is (= (vp/parse-for-tests "(x,y) => 4") '(fn [x y] 4)))

   (t/is (= (vp/parse-for-tests "(x,y,z) => x + 1")
            (vp/parse-for-tests "(x ,y,z) => x + 1")
            (vp/parse-for-tests "(x, y,z) => x + 1")
            (vp/parse-for-tests "(x , y,z) => x + 1")
            (vp/parse-for-tests "(x , y ,z) => x + 1")
            '(fn [x y z] (+ x 1))))

   (t/is (= (vp/parse-and-eval-for-tests "f = (x,y) => x + y; f(3,4)") 7 ))
   ;; todo. find out if there a way to apply function in visi, without assignment first

   )

  (t/testing
   "Test DotFuncExpr"
   ;; DotFuncExpr has the form 「.‹x›」.
   ;; 「.‹x›」 get turned into a function of 1 arg, named 「.‹x›」.
   ;; and 「.‹x›(‹y›, …)」 get turned into  「(.‹x› ‹y› …)」.
   ;; this means, if the ‹x› is a java method name, then it works.

   (t/is
    (let [form (vp/parse-for-tests ".x")]
      (comment '(fn [z__29__auto__] (.x z__29__auto__))
               ;; todo. find a better way to match form
               )
      (= (first form) 'fn )
      (vector? (second form))
      (seq? (nth (vp/parse-for-tests ".x") 2))))

   (t/is (= (vp/parse-for-tests ".x (4, 5)") '(.x 4 5)))

   (t/is (= (vp/parse-and-eval-for-tests ".codePointAt (\"a\", 0)") '97 ))))

 (t/testing
  "Test Mapcommand" ; todo. incomplete understanding

  ;; map command is one of the pipe commands. By itself, is not a valid syntax. Its syntactic ancestor that is a valid visi form is is expression.

  ;; Mapcommand = (<'xform'> | <'map'>) SPACES (IDENTIFIER | Keyword | FunctionExpr)

  ;; sample syntax
  (t/is (= (vp/parse-and-eval-for-tests
            "x = [1, 2, 3]; y = 1; x |> map (+ y)")
           '(2 3 4)))

  (t/is (= (vp/parse-and-eval-for-tests
            "x = [1, 2, 3]; y = 1; x |> xform (+ y)")
           '(2 3 4))))

 (t/testing
  "Test pipe commands and expressions"

  ;; simplified grammar
  ;; Pipe2Expression = EXPRESSION2 >>  (FunctionExpr / EXPRESSION2)
  ;; PipeExpression = (ParenExpr / IDENTIFIER)  |> PipeCommands
  ;; PipeFunctionExpression = |> PipeCommands

  (t/testing
   "Test PipeExpression" ; todo. work on Mapcommand first

   ;; (get-parsetree "x |> map f")
   ;; [:Line [:EXPRESSION [:PipeExpression [:IDENTIFIER "x"] [:Mapcommand [:IDENTIFIER "f"]]]]]

   ;; (get-parsetree "x |> map (+ y)")
   ;; [:Line [:EXPRESSION [:PipeExpression [:IDENTIFIER "x"] [:Mapcommand [:Partial3 [:Operator [:Op3 "+"]] [:EXPRESSION [:EXPRESSION2 [:IDENTIFIER "y"]]]]]]]]

   ;; (vp/parse-for-tests "x |> map (+ y)")
   ;; (as-> x x__30__auto__
   ;;       (visi.core.runtime/v-map x__30__auto__
   ;;                                (fn [x__37__auto__]
   ;;                                  (+ x__37__auto__ y))))

   ;; sample syntax
   (t/is (= (vp/parse-and-eval-for-tests
             "x = [1, 2, 3]; y = 1; x |> map (+ y)")
            '(2 3 4)))

   ;; (t/is (=
   ;;        (vp/pre-process-line "info |> map .toLowerCase")
   ;;        "(def lower (.cache (as-> info x__8942__auto__ (visi.core.runtime/v-map x__8942__auto__ (fn [z__8941__auto__] (.toLowerCase z__8941__auto__))))))"))

   )

  (t/testing
   "Test Pipe2Expression" ; todo
   )

  (t/testing
   "Test PipeFunctionExpression" ; todo. work on PipeCommands first. PipeCommands is made of several commands. PipeCommands itself doesn't have a transform rule. PipeCommands's parent is: PipeExpression, PipeFunctionExpression

   ;; PipeFunctionExpression = (SPACES* <'|>'> SPACES PipeCommands )+
   ;; :PipeFunctionExpression (fn [& pipeline]
   ;;                           (let [x `x#
   ;;                                 y `y#]
   ;;                             `(fn [~y] (~'as-> ~y ~x ~@(map #(% x) pipeline)))))

   ;; parent grammar rule is EXPRESSION, and that's it
   ;; todo. find something that allow EXPRESSION to find a syntax that covers this

   ;; (get-parsetree "|> x")                  ; parse error
   ;; (get-parsetree "3 |> x")                ; parse error
   ;; (get-parsetree "3 |> map")              ; parse error

   ;; todo find a way to match form
   (t/is (not=
          (vp/parse-for-tests "x |> map (+ y)")
          nil
          ;; '(as-> x x__30__auto__ (visi.core.runtime/v-map x__30__auto__ (fn [x__34__auto__] (+ x__34__auto__ y))))
          ))

   (t/is (not=
          (vp/parse-for-tests "x |> map f")
          nil
          ;; '(as-> x x__30__auto__ (visi.core.runtime/v-map x__30__auto__ (fn [x__34__auto__] (+ x__34__auto__ y))))
          ))))

 (t/testing
  "Test IfElseExpr"

  ;; :IfElseExpr (fn [test a b] `(~'if ~test ~a ~b))

  ;; IfElseExpr has 3 forms.
  ;; ①  「if( ‹test›, ‹true body› , ‹false body›) 」
  ;;  note: no space after “if”
  ;; ② 「if ‹test› then ‹true body› else ‹else body›」
  ;; and a C-syntax
  ;; ③ 「(‹test› ? ‹true body› : ‹else body›)」

  ;; todo, for forms ② and ③, the ‹else body› also allow (OprExpression / EXPRESSION). Not sure why, perhaps for some precedence quirk?
  ;; note: there's no just “if then” without “else”

  ;; test basic forms
  (t/is (=
         (vp/parse-for-tests "if( 3, 4, 5)")
         (vp/parse-for-tests "if 3 then 4 else 5")
         (vp/parse-for-tests "if 3
then 4 else 5") ; FIXME
         (vp/parse-for-tests "if 3 then
 4 else 5")
         (vp/parse-for-tests "(3 ? 4 : 5)")
         '(if 3 4 5)))
  ;; todo. add more test on extra space/newline variations in different places

  (t/is (=
         (vp/parse-and-eval-for-tests "if( 3, 4, 5)")
         (vp/parse-and-eval-for-tests "if 3 then 4 else 5")
         (vp/parse-and-eval-for-tests "(3 ? 4 : 5)")
         '4
         )))

 (t/testing
  "Test GetExpression" ; todo
  ;; GetExpression = SPACES? IDENTIFIER (<'['> EXPRESSION <']'>)+ SPACES?
  ;; :GetExpression (fn [a & b] `(~'-> ~a ~@(map (fn [z] `(~'get ~z)) b)))

  (t/is (= (vp/parse-for-tests "x[2]") '(-> x (get 2))))

  (t/is (= (vp/parse-for-tests "x = [3,4,5]; x[2]")
           '(clojure.core/let [x [3 4 5]] (-> x (get 2)))))

  ;; todo. double check if this can also be used for map or other data type
  (t/is (= (vp/parse-for-tests "x=[3]; x[2]")
           '(clojure.core/let [x [3]] (-> x (get 2))))))

 (t/testing "Test Foldcommand" ; todo
            )

 (t/testing "Test Flatmapcommand" ; todo
            )

 (t/testing "Test Filtercommand" ; todo
            )

 (t/testing "Test Groupbycommand" ; todo
            )

 (t/testing "Test Sortcommand" ; todo
            )

 (t/testing "Test ParenExpr" ; todo
            ;; ParenExpr = Partial1 / Partial2 / Partial3 / (SPACES? <'('> EXPRESSION <')'> SPACES?);
            )

 (t/testing "Test InlineFunc" ; todo
            )

 (t/testing "Test ClojureSymbol" ; todo

            ;; (get-parsetree "x")
            ;; [:Line [:EXPRESSION [:EXPRESSION2 [:IDENTIFIER "x"]]]]
            ;; (get-parsetree "list")
            ;; [:Line [:EXPRESSION [:EXPRESSION2 [:IDENTIFIER "list"]]]]
            ;; (get-parsetree "clojure.core/list")
            ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ClojureSymbol "clojure.core/list"]]]]

            (t/is (= (vp/parse-for-tests "clojure.core/list")
                     'clojure.core/list
                     )))

 (t/testing
  "Test parser, misc"

  (t/is (= (vp/parse-for-tests "x=[3]; 2")
           '(clojure.core/let [x [3]] 2)))

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
           '((2 3 4) (3 4 5) (4 5 6)))))

 ;;
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch pad

(comment ; visi code examples

;; calc_tax(income) = income * if(income > 5000, 40%, 20%)

;; ff(x) =
;;   if x > 5000 then
;;     40%
;;   else
;;     20%

;; ff(x) = x * tax_rate(x)

  ;; find where Math/cos came from in visi
;; Math/cos(Math/PI / 3) ## expression
;; cos_third_pi = Math/cos(Math/PI / 3) ## declaration

 ;; assign a lambda to a var
;; plus_one = x => x + 1

 ;; normal function def
;; plus_one(x) = x + 1

 ;; a more complex function def
 ;; note the  “if then else else” and nested if

    ;; test_income(income) =
    ;;   mag = Math/log10(income) ## the magnitude of the income
    ;;   if mag < 3 then "low"
    ;;   else if mag < 5 then "med"
    ;;   else "high"

;;; big chunk code example.
 ;; note the use of map, and reduce. Also, the merge-with, the 「(+)」, the lambda, the map.
 ;; note the use of  「‹var name›.sum」 to retrieve.
;; Study in detail

    ;; data = [1000, 10, 250000, 33] ## The data set

    ;; mapped = map(identity, data) ## Don't change the elements

    ;; reduced = reduce((acc, data) =>
    ;;                   merge-with((+), ## Merge the accumulator and the current value
    ;;                    acc, ## The accumulator
    ;;                   {:cnt -> 1, ## The data to add to the accumulator
    ;;                    :sum -> data}),
    ;;                   {}, ## Starting value for the acculumator
    ;;                   mapped) ## The data to reduce

    ;; average = reduced.sum / reduced.cnt

;; the above can also be written as

    ;; data = [1000, 10, 250000, 33] ## The data set

    ;; reduced = data |>
    ;;           map # Math/log10(it) |>
    ;;           reduce {} -> (acc, data) => merge-with((+), acc, {:cnt -> 1,
    ;;                                                             :sum -> data})

    ;; average = reduced.sum / reduced.cnt

)

(comment ;; random old sample visi code

   ;; (def lower
   ;;      (.cache
   ;;       (as-> info x__8942__auto__
   ;;             (visi.core.runtime/v-map x__8942__auto__
   ;;                                 (fn [z__8941__auto__]
   ;;                                     (.toLowerCase z__8941__auto__))))))

   ;; (t/is (=
   ;;        (vp/pre-process-line "sins = lower |> filter # (.contains(it, \"sin\") && not(.contains(it, \"sing\")))")
   ;;        "(def sins (as-> lower x__8942__auto__ (visi.core.runtime/v-filter x__8942__auto__ (fn [it] (and (.contains it \"sin\") (not (.contains it \"sing\")))))))"))

   ;; sins-plus-god-or-christ = sins |> filter # begin

   ;; twit = v/stream-into-watching((v/create-twitter-stream({:duration -> 5000})) |> map .getText |> map calc_sent |> filter # (1 < it.pos || 1 < it.neg) |> reduce | merge-with((+)))

   ;; lower-bible = (bible |> map .toLowerCase) >> # .cache(it)

   ;; wc = (lower-bible |> mapcat # .split(it, "\\W+")) >> # v/v-map-to-pair(it, # [it, 1] ) >> # v/v-reduce-by-key(it, (+))

)
