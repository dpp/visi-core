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
    (t/is (= (vp/parse-and-eval-for-tests "1#seconds + 1#minutes") 61000))
    )

  (t/testing
      "Test :Keyword"

    ;; visi keyword is similar to clojure keyword. In visi syntax, often interchangable in places that allow identifier. It gets turned into clojure keyword.

    ;; Keyword = <':'> IDENTIFIER;
    ;; :Keyword keyword

    ;; (get-parsetree ":x")
    ;; [:Line [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Keyword [:IDENTIFIER "x"]]]]]]
    ;; (get-transformed-result ":x")
    ;; :x
    ;; (keyword? (get-transformed-result ":x")) ; true

    (t/is (= (vp/parse-for-tests ":x25707") ':x25707))
    (t/is (= (vp/parse-for-tests ":p40689") ':p40689))

    ;; (get-parsetree ":x :y") ; parse error
    ;; (get-parsetree "x y") ; parse error
    ;; (get-parsetree "3 4") ; parse error
    ;; (get-parsetree "\"a\" \"b\"") ; parse error

    )

  (t/testing
      "Test string StringLit."          ; todo, test all the backslash special case

    ;; StringLit = #'\"(?:(?:[\\\\]\")|[^\"])*?\"'
    ;; :StringLit (fn [x] (let [c (count x)]
    ;;                      (-> x
    ;;                          (.substring 1 (- c 1))
    ;;                          (.replace "\\\"" "\"")
    ;;                          (.replace "\\n" "\n")
    ;;                          (.replace "\\\\" "\\")
    ;;                          (.replace "\\t" "\t"))))

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

    )

  (t/testing
      "Test constant definition ConstDef."
    (t/is (= (vp/parse-for-tests "x = 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x=3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x= 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests " x= 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests " x = 3") '(def x 3)))
    (t/is (= (vp/parse-for-tests "x  =   3") '(def x 3)))
    )

  (t/testing
      "Test block comment BlockComment."

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

  (t/testing
      "Test block expression BlockExpression."

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
;;              '(do (def x 4))))

    )

  (t/testing
      "Test Line comment LineComment."
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
    (t/is (= (vp/parse-for-tests "\"twtr://twitter.com/\"") '"twtr://twitter.com/"))

    )

  (t/testing
      "Test Function definition FuncDef."
    (t/is (= (vp/parse-for-tests "f(x, y) = x + y") '(defn f [x y] (+ x y))))
    (t/is (= (vp/parse-for-tests "f(x,y) = x+y") '(defn f [x y] (+ x y))))
    (t/is (= (vp/parse-for-tests "f(x)=3") '(defn f [x] 3))))

  (t/testing
      "Test source syntax"              ; todo

    ;; Source = <START_OF_LINE> <'source'> SPACES IDENTIFIER SPACES? (<'='> SPACES? (URL | EXPRESSION))? LineEnd+;
    ;; :Source (fn
    ;;           ([x] `(visi.core.runtime/source ~x))
    ;;           ([x v] `(visi.core.runtime/source ~x ~v)))

    (t/is (=
           (vp/parse-for-tests "source x52548")
           '(visi.core.runtime/source x52548)))

    (t/is (=
           (vp/parse-for-tests "source xyz = \"https://example.com/x.txt\"")
           '(visi.core.runtime/source xyz "https://example.com/x.txt")))

    (t/is (=
           (vp/pre-process-line "source 9")
           "source 9"))                 ; todo. needs error reporting

    (t/is (=
           (vp/parse-for-tests "source x49519 = 7")
           '(visi.core.runtime/source x49519 7)))

    )

  (t/testing
      "Test sink syntax"                ; todo

  ;; SINK = <START_OF_LINE> (<'sink:'> | <'sink'> ) SPACES IDENTIFIER SPACES? <'='> EXPRESSION <'\n'>;
   ;; :SINK (fn [name expression]
   ;;         (let [name (symbol name)
   ;;               res `(~'def ~name
   ;;                      (do
   ;;                        (visi.core.runtime/do-sink (quote  ~name) ~expression)
   ;;                        ~expression))]
   ;;           (insert-meta res)
   ;;           ))

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
      "Test map data type, the MapExpr"

    (comment
      ;; spec
      ;; MapExpr = SPACES? <'{'> (Pair <','>)* Pair (<','> SPACES?)? <'}'> SPACES?;
      ;; :MapExpr (fn [& x] (into {} x))

      ;; grammar parents:
      ;; Foldcommand = (<'fold'> | <'reduce'>) SPACES (((IDENTIFIER | ConstExpr | ParenExpr | MapExpr | VectorExpr) SPACES <'->'> SPACES)? (IDENTIFIER | FunctionExpr))
      ;; EXPRESSION2 = BlockExpression / GetExpression / IfElseExpr / FuncCall / ParenExpr /  ConstExpr / FieldExpr / FunctionExpr / MapExpr / VectorExpr / SetExpr / (SPACES? (IDENTIFIER | ClojureSymbol) SPACES?) / InlineFunc / MergeExpr / OprExpression

      ;; a MapExpr is basically of the form 「{‹key1› -> ‹value1›, ‹key2› -> ‹value2›, …}」, where the key is any of 「"‹x›"」, 「".‹x›"」, and possibly something else. Most likely, only 「"‹x›"」 is semantically valid form for key

      ;; (get-parsetree "{\"xx\" -> 3, \"yy\" -> 4}")
      ;;      [:Line
      ;;       [:EXPRESSION
      ;;        [:EXPRESSION2
      ;;         [:MapExpr
      ;;          [:Pair
      ;;           [:EXPRESSION
      ;;            [:EXPRESSION2
      ;;             [:ConstExpr
      ;;              [:StringLit "\"xx\""]]]]
      ;;           [:EXPRESSION
      ;;            [:EXPRESSION2
      ;;             [:ConstExpr
      ;;              [:Number "3"]]]]]
      ;;          [:Pair
      ;;           [:EXPRESSION
      ;;            [:EXPRESSION2
      ;;             [:ConstExpr
      ;;              [:StringLit "\"yy\""]]]]
      ;;           [:EXPRESSION
      ;;            [:EXPRESSION2
      ;;             [:ConstExpr
      ;;              [:Number "4"]]]]]]]]]
      ;; (get-parsetree "{.xx -> 3, .yy -> 4}")
      ;; [:Line [:EXPRESSION [:EXPRESSION2 [:MapExpr [:Pair [:DottedThing [:IDENTIFIER "xx"]] [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "3"]]]]] [:Pair [:DottedThing [:IDENTIFIER "yy"]] [:EXPRESSION [:EXPRESSION2 [:ConstExpr [:Number "4"]]]]]]]]]

      )

    (comment
      "Test Pair"

      ;; Pair = (DottedThing / EXPRESSION) <'->'> EXPRESSION;
      ;; :Pair (fn [a b] `[~a ~b])
      ;; MergeExpr = EXPRESSION (SPACES <'%%'> SPACES Pair)+;
      ;; MapExpr = SPACES? <'{'> (Pair <','>)* Pair (<','> SPACES?)? <'}'> SPACES?;

      ;; pair cannot be by itself accordig to grammar. 「(get-parsetree ".x ->2")」 is parser error. The possible parent of Pair is the merge expr and map expr. So, test Pair there.

      (comment
        "Test DottedThing"

        ;; DottedThing = SPACES? <'.'> IDENTIFIER SPACES?
        ;; Pair = (DottedThing / EXPRESSION) <'->'> EXPRESSION;
        ;; :DottedThing keyword

        ;; DottedThing cannot be by itself according to grammar spec. The only parent of dotted thing is Pair. So, test for pair instead.
        ;; when dotted thing 「.x」 is evaled, it is interpreted as DotFuncExpr.
        ;; that is,
        ;; (vp/parse-for-tests ".x")
        ;; it becomes
        ;; (fn [z__29__auto__] (.x z__29__auto__))
        )
      )

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
           '(def x {"y" 7})
           ))

    ;; todo.
    ;; find the proper semantically valid form, one of
    ;; {y -> 8}
    ;; {.y -> 8}
    ;; {"y" -> 8}
    ;; see test for FieldExpr

    ;; todo. set visi map to var, then retrieve a field. see FieldExpr test

    (t/testing
        "Test FieldExpr. Retrieve map item."

      (comment
        ;; FieldExpr = SPACES? IDENTIFIER (SPACES? <'.'> IDENTIFIER)+ SPACES?;
        ;; :FieldExpr (fn [root & others] `(~'-> ~root ~@(map (fn [x] `(~'get ~(keyword x))) others)))

        ;; FieldExpr have this form 「‹x› .‹y›」
        ;; its parse tree is this
        ;; (get-parsetree "x .y")
        ;; [:Line
        ;;  [:EXPRESSION
        ;;   [:EXPRESSION2
        ;;    [:FieldExpr
        ;;     [:IDENTIFIER "x"]
        ;;     [:IDENTIFIER "y"]]]]]
        ;; it gets transformed to this form
        ;; (-> x (get :y))
        ;; so, its semantics is clojure function 「get」
        ;; so, it means the FieldExpr is for getting item from visi map datatype

        )

      (t/is (= (vp/parse-for-tests "x .y") '(-> x (get :y))))

      ;; (get-parsetree "x .\"y\"")              ; Parse error

      ;; this doesn't make sense
      (t/is (= (vp/parse-for-tests "x = {\"y\" -> 7}; x .y")
               '(clojure.core/let [x {"y" 7}] (-> x (get :y)))))

      ;; question: just exactly what's the form of a key in visi's map data type? FieldExpr implies that it is .key , but MapExpr allows both .key and "key". But only the .key form makes sense when used with FieldExpr
      (t/is (= (vp/parse-for-tests "x = {.y -> 7}; x .y")
               '(clojure.core/let [x {:y 7}] (-> x (get :y)))))
      ;; quostion: why is this in a “let”? isn't assgnment global?

      (t/is (= (vp/parse-and-eval-for-tests "x = {.y -> 7}; x .y")
               '7))

      )

    (t/testing
        "Test MergeExpr"

      ;; MergeExpr = EXPRESSION (SPACES <'%%'> SPACES Pair)+;
      ;; :MergeExpr (fn [core & others] `(~'merge ~core ~@others))

      ;; test syntactic validity
      (t/is (= (vp/parse-for-tests "x %% y")
               '(merge x y)))

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
             {:x 3, :a 7, :b 2, :c 9}))

      ;; 2015-01-16 todo not sure if this actually valid.
      ;; (t/is (=
      ;;        (vp/parse-for-tests "{:a 3} %% {:b 4}")
      ;;        '(merge {:a 3} {:b 4})))

      )

    )

  (t/testing
      "Test set data type"              ; todo

    (t/is (= (vp/parse-for-tests "#{}") '#{}))

    (t/is (= (vp/parse-for-tests "#{x, y}") '#{x y})))

  (t/testing
      "Test FunctionExpr1"

    ;; it has this form
    ;; ‹x› => ‹expr›
    ;; (‹x1›, ‹x2›, …) => ‹expr›

    ;; (get-transformed-result "f => 4")
    ;; (fn [f] 4)

    ;; (get-transformed-result "(x,y) => 4")
    ;; (fn [x y] 4)

    ;; (get-transformed-result "(x,y,z) => 4")
    ;; (fn [x y z] 4)

    ;; (get-transformed-result "(x,y,z) => x + 1")
    ;; (fn [x y z] (+ x 1))

    ;; syntax parent is FunctionExpr

    ;; (get-parsetree "f => 4")
    ;; [:Line
    ;;  [:EXPRESSION
    ;;   [:EXPRESSION2
    ;;    [:FunctionExpr1
    ;;     [:IDENTIFIER "f"]
    ;;     [:EXPRESSION
    ;;      [:EXPRESSION2
    ;;       [:ConstExpr
    ;;        [:Number "4"]]]]]]]]

    ;; (get-parsetree "(x) => 4")
    ;; [:Line
    ;;  [:EXPRESSION
    ;;   [:EXPRESSION2
    ;;    [:FunctionExpr1
    ;;     [:IDENTIFIER "x"]
    ;;     [:EXPRESSION
    ;;      [:EXPRESSION2
    ;;       [:ConstExpr
    ;;        [:Number "4"]]]]]]]]

    ;; ;; (get-parsetree "(x,y) => 4")

    ;; [:Line
    ;;  [:EXPRESSION
    ;;   [:EXPRESSION2
    ;;    [:FunctionExpr1
    ;;     [:IDENTIFIER "x"]
    ;;     [:IDENTIFIER "y"]
    ;;     [:EXPRESSION
    ;;      [:EXPRESSION2
    ;;       [:ConstExpr
    ;;        [:Number "4"]]]]]]]]

    ;; (get-evaled-result "(x,y,z) => 4")
    ;; #<parser_test$eval1483$fn__1484 visi.core.parser_test$eval1483$fn__1484@34cdfeca>

    ;; so, FunctionExpr1 seems to be a lambda form.

    (t/is (= (vp/parse-for-tests "f => 4") '(fn [f] 4) ))
    (t/is (= (vp/parse-for-tests "(x,y) => 4") '(fn [x y] 4) ))

    (t/is (= (vp/parse-for-tests "(x,y,z) => x + 1")  
             (vp/parse-for-tests "(x ,y,z) => x + 1") 
             (vp/parse-for-tests "(x, y,z) => x + 1")  
             (vp/parse-for-tests "(x , y,z) => x + 1")  
             (vp/parse-for-tests "(x , y ,z) => x + 1") 
             '(fn [x y z] (+ x 1)) ))

    ;; todo. find out what's the visi syntax to apply this result. add a eval test

    )

  (t/testing
      "Test map command Mapcommand"     ; todo.

    ;; Mapcommand = (<'xform'> | <'map'>) SPACES (IDENTIFIER | Keyword | FunctionExpr)
    ;; :Mapcommand (fn [x] (fn [inside] `(~'visi.core.runtime/v-map ~inside ~x )))
    ;; parent
    ;; <PipeCommands> = Mapcommand | Flatmapcommand | Filtercommand | Zipcommand | Dropcommand | Sortcommand | Samplecommand | Foldcommand | Productcommand | Groupbycommand
    ;; children:
    ;; <FunctionExpr> = HashFunctionExpr / PartialFunction / FunctionExpr1 / DotFuncExpr / Partial1 / Partial2 / Partial3

    ;; (get-parsetree "map (+ 2)")         ; parse error
    ;; (get-parsetree "map f")             ; parse error
    ;; (get-parsetree "map f x")           ; parse error

    ;; sample syntax
    (t/is (= (vp/parse-and-eval-for-tests
              "x = [1, 2, 3]; y = 1; x |> map (+ y)")
             '(2 3 4)))

    )

  (t/testing
   "Test pipe expression PipeExpression" ; todo. work on Mapcommand first

   ;; PipeExpression = (ParenExpr / IDENTIFIER) (SPACES <'|>'> SPACES PipeCommands )+
   ;; :PipeExpression (fn [root & pipeline]
   ;;                   (let [x `x#]
   ;;                     `(~'as-> ~root ~x ~@(map #(% x) pipeline))))

   ;; (get-parsetree "[1, 2, 3] |> map (+ 2)") ; parse error

   ;; sample syntax
   (t/is (= (vp/parse-and-eval-for-tests
              "x = [1, 2, 3]; y = 1; x |> map (+ y)")
             '(2 3 4)))

   ;; (t/is (=
   ;;        (vp/pre-process-line "info |> map .toLowerCase")
   ;;        "(def lower (.cache (as-> info x__8942__auto__ (visi.core.runtime/v-map x__8942__auto__ (fn [z__8941__auto__] (.toLowerCase z__8941__auto__))))))"))

   )

  (t/testing
      "Test DotFuncExpr"                ; todo
    ;; DotFuncExpr is supposed to be just a function name.
    ;; todo: find out if it's a java method name, a clojure f name, or visi's builtin function
    ;; but, basically, 「.‹x›」 get turned into a function of 1 arg, named 「.‹x›」.
    ;; and 「.‹x›(‹y›, …)」 get turned into  「(.‹x› ‹y› …)」.
    ;; this means, if the ‹x› is a java method name, then it works.

    ;; DotFuncExpr = SPACES? <'.'> IDENTIFIER

    ;; :DotFuncExpr (fn [x]
    ;;                (let [z `z#]
    ;;                  `(~'fn [~z]
    ;;                     (~(symbol
    ;;                        (str "." x)) ~z))))

    ;; <FunctionExpr> = HashFunctionExpr / PartialFunction / FunctionExpr1 / DotFuncExpr / Partial1 / Partial2 / Partial3
    ;; PartialFunction = SPACES? <'|'> SPACES FuncCall
    ;; HashFunctionExpr = SPACES? <'#'> SPACES (DotFuncExpr / EXPRESSION2)

    ;; (get-parsetree ".x")
    ;; [:Line
    ;;  [:EXPRESSION
    ;;   [:EXPRESSION2
    ;;    [:DotFuncExpr
    ;;     [:IDENTIFIER "x"]]]]]

    ;; todo. find a way to match form
    ;; (vp/parse-for-tests ".x")
    ;; '(fn [z__29__auto__] (.x z__29__auto__)

    (t/is (=
           (vp/parse-for-tests ".x (4, 5)")
           '(.x 4 5)
           ))

    (t/is (=
           (vp/parse-and-eval-for-tests ".codePointAt (\"a\", 0)")
           '97
           ))

    )

  (t/testing
      "Test GetExpression"                 ; todo
    ;; GetExpression = SPACES? IDENTIFIER (<'['> EXPRESSION <']'>)+ SPACES?
    ;; :GetExpression (fn [a & b] `(~'-> ~a ~@(map (fn [z] `(~'get ~z)) b)))

    (t/is (= (vp/parse-for-tests "x[2]") '(-> x (get 2))))

    (t/is (= (vp/parse-for-tests "x = [3,4,5]; x[2]")
             '(clojure.core/let [x [3 4 5]] (-> x (get 2)))))

    ;; todo. double check if this can also be used for map or other data type
    (t/is (= (vp/parse-for-tests "x=[3]; x[2]")
             '(clojure.core/let [x [3]] (-> x (get 2)))))
    )

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
             '((2 3 4) (3 4 5) (4 5 6))))

    )

  ;;
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scratch pad

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

