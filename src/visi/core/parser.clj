(ns visi.core.parser
  (:require
   [visi.core.util :as vu]
   [instaparse.core :as insta]
   [clojure.string :as cljstr]
   ))

;; note:
;; the only functions called outside this file is

(defn insert-meta
  [res]
  (cons (first res)
        (cons
         (vary-meta (second res) merge {:source `(quote  ~res)})
         (drop 2 res))))

(defn- deal-with-identifier
  "x is a string.
Returns a symbol of x. If x is any of nil, true, false, return the string as is."
  [x]
  (let [sx (symbol x)]
    (cond
     (= "nil" x) nil
     (= "true" x) true
     (= "false" x) false
     :else sx)))

(def parse-def
  "Visi grammar in ebnf-like instaparse format."
  "
  Lines = (Line (<'\n'>)*)*;

  Line = ((BlockComment <'\n'>) | LineComment)* (SINK / Def / Source / (EXPRESSION LineEnd*));

  <Def> = <START_OF_LINE> (ConstDef | FuncDef);

  <LineEnd> = <'\n'>+ | (<';'> SPACES*)

  ConstDef = ConstDef1 ;

  ConstDef1 = IDENTIFIER SPACES <'='> SPACES? EXPRESSION SPACES? LineEnd;

  FuncDef = IDENTIFIER SPACES? <'('> SPACES? (IDENTIFIER SPACES? <','> SPACES?)*
            IDENTIFIER SPACES? <','>? SPACES? <')'> SPACES?
            <'='> EXPRESSION SPACES? LineEnd;

  SINK = <START_OF_LINE> (<'sink:'> | <'sink'> ) SPACES IDENTIFIER SPACES? <'='> EXPRESSION
         <'\n'>;

  Source = <START_OF_LINE> <'source'> SPACES IDENTIFIER SPACES? (<'='> SPACES? (URL | EXPRESSION))? LineEnd+;

  START_OF_LINE = #'^' ;

  <SPACES> = (<'\n'> SPACES) / (SPACES <'\n'> SPACES) /
             (SPACES? LineComment SPACES?) / (SPACES? BlockComment SPACES?) /
             <(' ' | '\t')+>;

  LineComment = (SPACES? <';;'> (#'[^\n]')*);

  BlockComment = <'/*'> (BlockComment | (!'*/' AnyChar))* <'*/'>;

  <AnyChar> = #'.' | '\n';

  IDENTIFIER = #'[a-zA-Z][a-zA-Z0-9\\-_\\?]*';

  ClojureSymbol = #'([a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*)\\/[a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*' |
  #'\\.[a-zA-Z][a-zA-Z0-9\\-_\\?]*';

  BlockExpression = SPACES? <'begin'> (SPACES |  LineEnd)* (EXPRESSION LineEnd SPACES*)* EXPRESSION LineEnd* SPACES* <'end'> LineEnd?;

  Op10Exp =(Op9Exp SPACES Op10 SPACES Op9Exp) / Op9Exp;

  Op9Exp = (Op8Exp SPACES Op9 SPACES Op8Exp) / Op8Exp;

  Op8Exp = (Op7Exp  SPACES Op8 SPACES Op7Exp) / Op7Exp;

  Op7Exp = (Op6Exp SPACES Op7 SPACES Op6Exp) / Op6Exp;

  Op6Exp = (Op5Exp  SPACES Op6 SPACES Op5Exp) / Op5Exp;

  Op5Exp = (Op4Exp  SPACES Op5 SPACES Op4Exp) / Op4Exp;

  Op4Exp = (Op3Exp  SPACES Op4 SPACES Op3Exp) / Op3Exp;

  Op3Exp = (Op2Exp SPACES Op3 SPACES Op2Exp) / Op2Exp;

  Op2Exp = (Op1Exp SPACES Op2 SPACES Op1Exp) / Op1Exp;

  Op1Exp = (EXPRESSION SPACES Op1 SPACES EXPRESSION) / EXPRESSION;

  Op10 = NeverMatch;

  Op9 = NeverMatch;

  Op8 = '&&' | '||' | '%%';

  Op7 = NeverMatch;

  Op6 = NeverMatch;

  Op5 = '==' | '<>' | '!=';

  Op4 = '>' | '<' | '>=' | '<=' ;

  Op3 = '+' | '-' | '&';

  Op2 = '*' | '/';

  Op1 = '^';

  NeverMatch = #'([&][+][@][%]){100000,}';

  Pipe2Expression = EXPRESSION2  (SPACES <'>>'> SPACES (FunctionExpr / EXPRESSION2))+;

  PipeExpression = (ParenExpr / IDENTIFIER) (SPACES <'|>'> SPACES PipeCommands )+

  <PipeCommands> = Mapcommand | Flatmapcommand | Filtercommand |
                   Zipcommand | Dropcommand | Sortcommand |
                   Samplecommand | Foldcommand | Productcommand |
                   Groupbycommand

  Mapcommand = (<'xform'> | <'map'>) SPACES (IDENTIFIER | Keyword | FunctionExpr)

  Flatmapcommand = (<'xform-cat'> | <'mapcat'> | <'flatmap'>) SPACES (IDENTIFIER | Keyword | FunctionExpr)

  Filtercommand = (<'filter'>) SPACES (IDENTIFIER | Keyword | FunctionExpr)

  Sortcommand = (<'sort'>) SPACES (IDENTIFIER | Keyword | FunctionExpr)
                  (SPACES <','> ('ascending' | 'descending'))?

  Groupbycommand = (<'group by'> / <'group'> ) SPACES (IDENTIFIER | Keyword | FunctionExpr)

  Zipcommand = (<'join'> | <'zip'>) SPACES (IDENTIFIER |
                      (<'('> (SPACES? IDENTIFIER SPACES? <','> SPACES?)+ <')'> ))

  Productcommand = (<'product'> ) SPACES (IDENTIFIER |
                      (<'('> (SPACES? IDENTIFIER SPACES? <','> SPACES?)+ <')'> ))

  Dropcommand = (<'drop'>) SPACES (IDENTIFIER | ConstExpr | ParenExpr)

  Samplecommand = (<'sample'>) SPACES (IDENTIFIER | ConstExpr | ParenExpr)

  Foldcommand = (<'fold'> | <'reduce'>) SPACES
                (((IDENTIFIER | ConstExpr | ParenExpr | MapExpr | VectorExpr)
                   SPACES <'->'> SPACES)? (IDENTIFIER | FunctionExpr))

  OprExpression = SPACES? Op10Exp SPACES?

  InlineFunc = SPACES? (ConstDef | FuncDef)+ SPACES EXPRESSION

  GetExpression = SPACES? IDENTIFIER (<'['> EXPRESSION <']'>)+ SPACES?

  EXPRESSION = EXPRESSION2 / Pipe2Expression / PipeExpression

  EXPRESSION2 = BlockExpression / GetExpression /
  IfElseExpr / FuncCall / ParenExpr /  ConstExpr /
  FieldExpr / FunctionExpr / MapExpr / VectorExpr /
  (SPACES? (IDENTIFIER | ClojureSymbol) SPACES?) /
  InlineFunc / MergeExpr / OprExpression

  MergeExpr = EXPRESSION (SPACES <'%%'> SPACES Pair)+;

  FuncCall = SPACES? (IDENTIFIER | ClojureSymbol) SPACES?
             <'('> (EXPRESSION <','>)*
                   (EXPRESSION <','>?)? SPACES? <')'> SPACES?;

  Partial1 = (SPACES? <'('> SPACES? Operator SPACES? <')'> SPACES?)

  Partial2 = (SPACES? <'('> SPACES? EXPRESSION SPACES Operator SPACES? <')'> SPACES?)

  Partial3 = (SPACES? <'('> SPACES? Operator SPACES EXPRESSION SPACES? <')'> SPACES?)

  ParenExpr = Partial1 / Partial2 / Partial3 / (SPACES? <'('> EXPRESSION <')'> SPACES?);

  Keyword = <':'> IDENTIFIER;

  IfElseExpr = (SPACES? <'if'> SPACES EXPRESSION SPACES <'then'>
               SPACES EXPRESSION SPACES <'else'> SPACES (OprExpression / EXPRESSION)) /
               (EXPRESSION SPACES <'?'> SPACES EXPRESSION SPACES <':'> SPACES (OprExpression / EXPRESSION));

  ConstExpr = SPACES? (Number | Keyword | StringLit | RegexLit) SPACES?;

  RegexLit = #'#/[^/]*?/';

  FieldExpr = SPACES? IDENTIFIER (SPACES? <'.'> IDENTIFIER)+ SPACES?;

  <FunctionExpr> = HashFunctionExpr / PartialFunction / FunctionExpr1 / DotFuncExpr / Partial1 / Partial2 / Partial3

  PartialFunction = SPACES? <'|'> SPACES FuncCall

  HashFunctionExpr = SPACES? <'#'> SPACES (DotFuncExpr / EXPRESSION2)

  DotFuncExpr = SPACES? <'.'> IDENTIFIER

  FunctionExpr1 = SPACES? (IDENTIFIER | (<'('> SPACES? (IDENTIFIER SPACES?
                 <','> SPACES?)*  IDENTIFIER SPACES? <','>? SPACES? <')'> ) )
                 SPACES? <'=>'> SPACES? EXPRESSION SPACES?;

  URL = #'\\b(https?|ftp|file|twtr)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]'

  Number = #'(\\-|\\+)?([0-9]+\\.[0-9]*|[0-9]*\\.[0-9]+|[0-9]+)' NumberQualifier?;

  NumberQualifier = ('%' | '.minutes' | '.hours' | '.seconds' | '.days');

  MapExpr = SPACES? <'{'> (Pair <','>)* Pair (<','> SPACES?)? <'}'> SPACES?;

  VectorExpr = SPACES? <'['> (EXPRESSION <','>)* EXPRESSION (<','> SPACES?)? <']'> SPACES?;

  SetExpr = SPACES? <'#{'> (EXPRESSION <','>)* EXPRESSION (<','> SPACES?)? <'}'> SPACES?;

  DottedThing = SPACES? <'.'> IDENTIFIER SPACES?

  Pair = (DottedThing / EXPRESSION) <'->'> EXPRESSION;

  StringLit = #'\"(?:(?:[\\\\]\")|[^\"])*?\"'

  Operator = Op1 | Op2 | Op3 | Op4 | Op5 | Op6 | Op7 | Op8 | Op9 | Op10;
  " )

  ;; #'\\\"(?:(?:[\\][\\\"])|[^\\\"])*?\\\'"

;; TODO visi-parse is called by visi-parse, but visi-parse isn't called anywhere.
(def the-parser
  "Returns a visi parser that starts with grammar rule `Lines'"
  (insta/parser parse-def
                :start :Lines ))

(def line-parser
  "Returns a visi parser that starts with grammar rule `Lines'. This parser will return a parse tree even if the input isn't valid. The error will be embedded in part of parse tree."
  (insta/parser parse-def
                :start :Line
                ;; :output-format :enlive
                :total true))

(defn- process-inner
"takes 2 args: defs core.
Called like this:
 :InlineFunc (fn [& x] (process-inner (drop-last x) (last x)))
todo: documentation incomplete.
"
  [defs core]
  (if (empty? defs)
    core
    (let [a (first defs)
          o (rest defs)]
      `(let [~(second a)
             ~(if (= 'defn (first a))
                `(fn ~@(-> a rest rest))
                (nth a 2))]
         ~(process-inner o core)))
    ))

(def multipliers
"A map for converting seconds, minutes, hours, days, into miliseconds. Also, % converts to 1/100.
  Keys are strings.
  e.g. (get multipliers \".seconds\" ) returns 1000."
  {"%" 1/100
   ".seconds" 1000
   ".minutes" (* 60 1000)
   ".hours" (* 60 60 1000)
   ".days" (* 24 60 60 1000)})

(def op-lookup
  "A map for converting operators as string to a corresponding symbol of Clojure.
  Keys are strings. e.g.
  \"+\" becomes '+
  \"^\" becomes 'Math/pow"
  {"+" '+
   "-" '-
   "*" '*
   "/" '/
   "%%" 'merge
   "^" 'Math/pow
   "&&" 'and
   "&" 'str
   "||" 'or
   "==" '=
   "<>" 'not=
   "!=" 'not=
   ">" '>
   ">=" '>=
   "<=" '<=
   "<" '<})

(def do-opr
  "A function that evaluate operator expression.
  if input is 1 arg, return it as is.
  if input is 3 args of the form `a [_ op] b', evaluate it.
  todo: documentation incomplete.
  "
  (fn
    ([x] x)
    ([a [_ op] b] `(~(op-lookup  op) ~a ~b))
    ;; ([[_ a op b]] `(~op ~a ~b))
    ;;([a [_ op] b] `(~(op-lookup  op) ~a ~b))
    ;;([[_ a [_ op] b]] `(~(op-lookup op) ~a ~b))
    ))

;; todo: this function is not called anywhere
(defn- expand-vars
  "todo"
  [[name & other]]
  (if (empty? other)
    name
    `[~name ~(expand-vars other)]))

(def xform-rules
  "A map for instaparse's transform. This is used to evaluate parse tree.
  Keys are keywords, values are functions.
  This is called like this:
  (instaparse.core/transform xform-rules parsetree)"
  {:Line (fn [x] x)
   :BlockExpression (fn [& x] `(do ~@x))
   :Number (fn
             ([x] (read-string x))
             ([x [_ qual]]
                (let [x (read-string x)]
                  (* x (get multipliers qual)))))

   :GetExpression (fn [a & b] `(~'-> ~a ~@(map (fn [z] `(~'get ~z)) b)))

   :Pair (fn [a b] `[~a ~b])

   :MapExpr (fn [& x] (into {} x))

   :URL identity

   :FieldExpr (fn [root & others]
                `(~'-> ~root ~@(map (fn [x] `(~'get ~(keyword x))) others)))

   :SINK (fn [name expression]
           (let [name (symbol name)
                 res `(~'def ~name
                        (do
                          (visi.runtime/do-sink (quote  ~name) ~expression)
                          ~expression))]
             (insert-meta  res)
             ))

   :Source (fn
             ([x] `(visi.runtime/source ~x))
             ([x v] `(visi.runtime/source ~x ~v)))

   :MergeExpr (fn [core & others] `(~'merge ~core ~@others))

   :FunctionExpr1 (fn [& all]
                   (let [vars (drop-last all)
                         expr (last all)]
                     `(~'fn [~@vars] ~expr)))

   :DotFuncExpr (fn [x]
                  (let [z `z#]
                    `(~'fn [~z] (~(symbol (str "." x)) ~z))))

   :PipeExpression (fn [root & pipeline]
                     (let [x `x#]
                       `(~'as-> ~root ~x ~@(map #(% x) pipeline))))

   :Mapcommand (fn [x] (fn [inside] `(~'visi.runtime/v-map ~inside ~x )))

   :Foldcommand (fn
                  ([x]
                     (fn [inside] `(~'visi.runtime/v-reduce ~inside ~x)))
                  ([x y]
                     (fn [inside] `(~'visi.runtime/v-fold ~inside ~x ~y))))

   :Flatmapcommand (fn [x] (fn [inside] `(~'visi.runtime/v-flat-map ~inside ~x )))

   :Filtercommand (fn [x] (fn [inside] `(~'visi.runtime/v-filter ~inside ~x )))

   :Groupbycommand (fn [x] (fn [inside] `(~'visi.runtime/v-group-by ~inside ~x )))

   :Sortcommand (fn
                  ([x] (fn [inside] `(~'visi.runtime/v-sort-by ~inside ~x true)))
                  ([x order] (fn [inside] `(~'visi.runtime/v-sort-by ~inside ~x (= order 'ascending')))))

   :StringLit (fn [x] (let [c (count x)]
                        (-> x
                            (.substring 1 (- c 1))
                            (.replace "\\\"" "\"")
                            (.replace "\\n" "\n")
                            (.replace "\\\\" "\\")
                            (.replace "\\t" "\t"))))

   :RegexLit (fn [x] (let [c (count x)]
                        (-> x
                            (.substring 2 (- c 1))
                            java.util.regex.Pattern/compile
                            )))

   :Keyword keyword
   :DottedThing keyword

   :ConstExpr identity
   :ParenExpr identity
   :FuncCall (fn [func & params] `(~func ~@params))
   :VectorExpr (fn [& x] `[~@x])

   :SetExpr (fn [& x] `#{~@x})

   :InlineFunc (fn [& x] (process-inner (drop-last x) (last x)))
   :FuncDef (fn [func & others]
              (let [params (-> others drop-last)
                    exp (last others)
                    res
                    `(~'defn ~func [~@params] ~exp)]
                (insert-meta res)))

   :IfElseExpr (fn [test a b] `(~'if ~test ~a ~b))
   :OprExpression identity
   :Op2Exp do-opr
   :Op3Exp do-opr
   :Op4Exp do-opr
   :Op1Exp do-opr
   :Op5Exp do-opr
   :Op6Exp do-opr
   :Op7Exp do-opr
   :Op8Exp do-opr
   :Op9Exp do-opr
   :Op10Exp do-opr

   :ConstDef identity

   :ConstDef1 (fn [a b]
                (let [res `(~'def ~a ~b)]
                  (insert-meta res)
                  ))

   :Pipe2Expression (fn [nub & others]
                      (let [x `x#]
                        `(~'as-> ~nub ~x ~@(map (fn [y] `(~y ~x)) others))))

   :HashFunctionExpr (fn [x] `(~'fn [~'it] ~x))

  :PartialFunction (fn [x] (cons 'partial x))

   :Partial1 (fn [x] (-> x second second op-lookup))

   :Partial2 (fn [v x]
               (let [x2 `x#]
                 `(~'fn [~x2] (~(-> x second second op-lookup) ~v ~x2))))

   :Partial3 (fn [x v]
               (let [x2 `x#]
                 `(~'fn [~x2] (~(-> x second second op-lookup) ~x2 ~v ))))

   :EXPRESSION identity
   :EXPRESSION2 identity
   :IDENTIFIER deal-with-identifier
   :ClojureSymbol symbol
   })

(defn post-process
  "Process the Instaparse output into Clojure if we can successfully parse the item.
  Takes 2 args: parse-tree, namespace
  parse-tree is Instaparse's parse tree.
  Returns a map. If failed, returns
  {:failed true :error parse-tree}
  else, return
  {:failed false :res parse-result}"
  [parse-tree namespace]
  (if (instance? instaparse.gll.Failure parse-tree)
    {:failed true :error parse-tree}
    {:failed false :res
     (let [q (insta/transform xform-rules parse-tree)]
       q
       )}
    ))

(defn parse-line
  "Parse 1 line of visi code.
  Arguments: the-line, namespace
  the-line is a string.
  namespace is a symbol (or string?) TODO
  If namespace not given, defaults to nil.
  Returns a map. If failed, returns
  {:failed true, :error parse-tree}
  else, return
  {:failed false, :res parse-result}"
  ([the-line] (parse-line the-line nil))
  ([the-line namespace]
   (-> the-line .trim (str "\n") line-parser (post-process namespace))))



(defn pre-process-line
  "Looks at the line... if it looks like Clojure, pass it through, but if it
  looks like Visi, parse it and return the Clojure code as one string.
  Argument is a string.
  Returns a string.
For example, \"x = 3\" returns \"(def x 3)\".
For example, \"(+ 3 7)\" returns \"(+ 3 7)\".
"
  [code]

  (let [code (.trim code)]
    (if
        (and (< 0 (count code))
             (not (#{"(" "{" "["} (.substring code 0 1))))
      (let [res (parse-line code)]
        (if (:res res)
          (-> res :res pr-str)
          code))
      code)
    ))

(defn notebook-to-strings
  "Takes a single String blob in the Gorilla Notebook format and
converts it into a Vector of individual lines"
  [notebook]
  (->>
   notebook
   cljstr/split-lines
   (reduce
    (fn [info x]
      (if (:in info)
        (if (= ";; @@" x)
          (assoc info :in false)
          (let [ln (:lines info)]
            (assoc info :lines (conj (-> ln butlast vec)
                                     (conj (last ln) x)))))
        (if (= ";; @@" x)
          {:in true :lines (conj (:lines info) [])}
          info))) {:in false :lines []})
   :lines
   (mapv #(->> % (clojure.string/join "\n") .trim))
   (filterv #(not (= 0 (count %))))
   ))

;; TODO this function is not called anywhere
(defn process-notebook
  "Turn a Gorilla notebook into a Clojure file"
  [notebook]
  (->> notebook
       notebook-to-strings
       (map pre-process-line)
       (cljstr/join "\n\n")))

;; register a hook for the current file if the REPL stuff is loaded
