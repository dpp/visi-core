;; (c) Copyright 2014-2015 David Pollak (@dpp, feeder.of.the.bears at gmail)
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns visi.core.parser
  (:require
   [visi.core.util :as vu]
   [instaparse.core :as insta]
   [clojure.string :as cljstr]
   [clojure.tools.analyzer.jvm :as ca]
   [clojure.tools.analyzer.passes.jvm.emit-form :as e]
   [clojure.tools.analyzer.passes.emit-form :as ef]))

(def ^:dynamic *current-line* nil)

(defmacro thread-it
  [a b]
  `(let [~'it ~a] ~b))

(defn insert-meta
  "Takes 1 argument: res. res is a Clojure form, e.g. `(def x 4).
Returns a new res, but with metadata attached to the second element of res.
the new metadata is metadata of res, plus a new pair {:source res}"
  ([res] (insert-meta res {}))
  ([res extras]
   (binding [*print-meta* true]
     (cons (first res)
           (cons
            (vary-meta (second res) merge {:visi-source *current-line*
                                           :clj-source (pr-str res)} extras)
            (drop 2 res))))))

(def multipliers
  "A map for converting units into milliseconds.
Example:
 (get multipliers \".seconds\" )
returns 1000."
  {"%" 1/100
   "#seconds" 1000
   "#minutes" (* 60 1000)
   "#hours" (* 60 60 1000)
   "#days" (* 24 60 60 1000)})

(defn- process-inner
  "Used to transform visi syntax for :InlineFunc grammar rule.
 x = 3; x
becomes
 [:InlineFunc '(def x 3) 'x]
and
 (fn [& x] (process-inner (drop-last x) (last x)))
is applied, resulting
 (clojure.core/let [x 3] x)"
  [defs core]
  (if (empty? defs)
    core
    (let [a (first defs)
          o (rest defs)]
      `(let [~(second a)
             ~(if (= 'defn (first a))
                `(fn ~@(-> a rest rest))
                (nth a 2))]
         ~(process-inner o core)))))

(def op-lookup
  "A map. For converting operators as string to a corresponding symbol of Clojure.
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
If input is 1 arg, return it as is.
If input is 3 args of the form
    a [_ op] b
transform it to the form
    (clojureFunction a b)
such that it becomes a valid Clojure expression."
  (fn
    ([x] x)
    ([a [_ op] b] `(~(op-lookup  op) ~a ~b))))

(def regexlit
  #"#(_+|/+|\|+){1}+(.+?)\1{1}?")

(def stringlit
  #"(?s:#(\"{2,}|\'{2,}|\^{2,}){1}+(.+?)\1{1}?)"
  )


(def parse-def
  "Visi grammar in Instaparse format."
  (str  "
  Lines = (Line (<'\n'>)*)*;

  Line = Namespace / (<LineComment> <'\n'>) / ((<BlockComment> <'\n'>) | <LineComment>)* (SINK / Def / Source / (EXPRESSION LineEnd*) / EmptyLine);

  EmptyLine = SPACES? LineEnd*;

  NamespaceName = #'([a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*)'

  FullSymbol =  (#'([a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*)\\/[a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*' / #'([a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*)')

  Loadable = <'['> SPACES? FullSymbol SPACES? <','> SPACES? StringLit SPACES? <']'>

  Namespace = <'$package('> SPACES? NamespaceName (SPACES? <','> (Requires | Import | Load))* SPACES? <')'> SPACES? LineEnd;

  Require = NamespaceName (SPACES <'as'> SPACES NamespaceName)?;

  Requires = SPACES? <'require('> (SPACES? Require SPACES? <','>)* (SPACES? Require)? SPACES? <')'>

  Import = SPACES? <'import('> (SPACES? NamespaceName SPACES? <','>)* (SPACES? NamespaceName)?  SPACES? <')'>

  Load = SPACES? <'load('> (SPACES? Loadable SPACES? <','>)* (SPACES? Loadable)? SPACES? <')'>

  <Def> = <START_OF_LINE> (ConstDef | FuncDef);

  <LineEnd> = <'\n'>+ | (<';'>)

  ConstDef = ConstDef1 ;

  ConstDef1 = IDENTIFIER SPACES? <'='> SPACES? EXPRESSION SPACES? LineEnd;

  FuncDef = IDENTIFIER <'('> SPACES? (IDENTIFIER SPACES? <','> SPACES?)*
            IDENTIFIER SPACES? <','>? SPACES? <')'> SPACES?
            <'='> EXPRESSION SPACES? LineEnd;

  SINK = <START_OF_LINE> (<'sink:'> | <'sink'> ) SPACES IDENTIFIER SPACES? <'='> SPACES? EXPRESSION
         <'\n'>;

  Source = <START_OF_LINE> <'source'> SPACES IDENTIFIER SPACES? (<'='> SPACES? (URL | EXPRESSION))? LineEnd+;

  START_OF_LINE = #'^' ;



  <SPACES> = (<'\n'> SPACES) / (SPACES <'\n'> SPACES) /
             (SPACES? <LineComment> SPACES?) / (SPACES? <BlockComment> SPACES?) /
             <#'\\s+'>;

  <LineComment> = (<'##'> (#'[^\n]')*);

  <BlockComment> = <'/*'> (BlockComment | (!'*/' AnyChar))* <'*/'>;

  <AnyChar> = #'.' | '\n';

  IDENTIFIER = #'(?:\\$\\.)?[a-zA-Z](?:[a-zA-Z0-9_\\?]|\\$-)*';

  ClojureSymbol = (<'`'> (#'([a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*)\\/[a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*' |
  #'[a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*')) / #'([a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*)::[a-zA-Z\\-\\*\\+\\!\\_][a-zA-Z0-9\\-\\.\\*\\+\\!\\_]*' ;

  BlockExpression = SPACES? <'begin'> (SPACES | LineEnd)* (EXPRESSION LineEnd SPACES*)* EXPRESSION LineEnd* SPACES* <'end'> LineEnd?;

  Op10Exp =(Op9Exp  Op10 SPACES? Op10Exp) / Op9Exp;

  Op9Exp = (Op8Exp  Op9 SPACES? Op10Exp) / Op8Exp;

  Op8Exp = (Op7Exp  Op8 SPACES? Op10Exp) / Op7Exp;

  Op7Exp = (Op6Exp  Op7 SPACES? Op10Exp) / Op6Exp;

  Op6Exp = (Op5Exp   Op6 SPACES? Op10Exp) / Op5Exp;

  Op5Exp = (Op4Exp   Op5 SPACES? Op10Exp) / Op4Exp;

  Op4Exp = (Op3Exp   Op4 SPACES? Op10Exp) / Op3Exp;

  Op3Exp = (Op2Exp Op3 SPACES? Op10Exp) / Op2Exp;

  Op2Exp = (Op1Exp  Op2 SPACES? Op10Exp) / Op1Exp;

  Op1Exp = (EXPRESSION3  Op1 SPACES? EXPRESSION) / EXPRESSION3;

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

  Pipe2Expression = EXPRESSION2  (SPACES <'|>'> SPACES (FunctionExpr / EXPRESSION2))+;

  Pipe2FunctionExpression =   (SPACES? <'|>'> SPACES (FunctionExpr / EXPRESSION2))+;


  PipeExpression = (ParenExpr / IDENTIFIER) (SPACES <'|>'> SPACES PipeCommands )+

  PipeFunctionExpression = (SPACES* <'|>'> SPACES PipeCommands )+


  <PipeCommands> = Mapcommand | Flatmapcommand | Filtercommand |
                   Zipcommand | Dropcommand | Sortcommand |
                   Samplecommand | Foldcommand | Reducecommand | Productcommand |
                   Groupbycommand

  Mapcommand = (<'xform'> | <'map'>) SPACES (IDENTIFIER / Keyword / FunctionExpr / FuncCall / ParenExpr)

  Flatmapcommand = (<'xform-cat'> | <'mapcat'> | <'flatmap'>) SPACES (IDENTIFIER / Keyword / FunctionExpr / FuncCall / ParenExpr)

  Filtercommand = (<'filter'>) SPACES (IDENTIFIER / Keyword / FunctionExpr / FuncCall / ParenExpr)

  Sortcommand = (<'sort'>) SPACES (IDENTIFIER / Keyword / FunctionExpr / FuncCall / ParenExpr)
                  (SPACES? <','> SPACES? ('ascending' | 'descending'))?

  Groupbycommand = (<'group by'> / <'group'> ) SPACES (IDENTIFIER / Keyword / FunctionExpr / FuncCall / ParenExpr)

  Zipcommand = (<'join'> | <'zip'>) SPACES (IDENTIFIER |
                      (<'('> (SPACES? IDENTIFIER SPACES? <','> SPACES?)+ <')'> ))

  Productcommand = (<'product'> ) SPACES (IDENTIFIER |
                      (<'('> (SPACES? IDENTIFIER SPACES? <','> SPACES?)+ <')'> ))

  Dropcommand = (<'drop'>) SPACES (IDENTIFIER / ConstExpr / ParenExpr / FuncCall / ParenExpr)

  Samplecommand = (<'sample'>) SPACES (IDENTIFIER / ConstExpr / ParenExpr / FuncCall / ParenExpr)

  Reducecommand = (<'reduce'>) SPACES
                (IDENTIFIER / FunctionExpr / FuncCall / ParenExpr)

  Foldcommand = (<'fold'>) SPACES
                (((IDENTIFIER / ConstExpr / ParenExpr / MapExpr / VectorExpr)
                   SPACES <'->'> SPACES)? (IDENTIFIER / FunctionExpr / FuncCall / ParenExpr))

  OprExpression = SPACES? Op10Exp SPACES?

  InlineFunc = SPACES? (ConstDef | FuncDef)+ SPACES EXPRESSION

  GetExpression = SPACES? IDENTIFIER (<'['> EXPRESSION <']'>)+ SPACES?

  EXPRESSION = EXPRESSION2 / PipeExpression / PipeFunctionExpression /
                    Pipe2Expression / Pipe2FunctionExpression

  EXPRESSION2 = (BlockExpression / GetExpression /
  IfElseExpr / FuncCall / ParenExpr / (OprExpression) /
  FieldExpr / FunctionExpr / MapExpr / VectorExpr / SetExpr /
  InlineFunc / ((IDENTIFIER | ClojureSymbol)) / ConstExpr / MergeExpr )

  EXPRESSION3 = SPACES? (BlockExpression / GetExpression /
  IfElseExpr / FuncCall / ParenExpr  / ConstExpr /
  FieldExpr / MapExpr / VectorExpr / SetExpr /
  ((IDENTIFIER | ClojureSymbol)) / MergeExpr) SPACES?


  MergeExpr = EXPRESSION (SPACES? <'%%'> SPACES? Pair)+;

  FuncCall = SPACES? (IDENTIFIER / ClojureSymbol / ParenExpr)
             <'('> (EXPRESSION <','>)*
                   (EXPRESSION <','>?)? SPACES? <')'> SPACES?;

  Partial1 = (SPACES? <'('> SPACES? Operator SPACES? <')'> SPACES?)

  Partial2 = (SPACES? <'('> SPACES? EXPRESSION SPACES? Operator SPACES? <')'> SPACES?)

  Partial3 = (SPACES? <'('> SPACES? Operator SPACES? EXPRESSION SPACES? <')'> SPACES?)

  ParenExpr = Partial1 / Partial2 / Partial3 / (SPACES? <'('> EXPRESSION <')'> SPACES?);

  Keyword =  IDENTIFIER <':'> ;

  IfElseExpr = (SPACES? <'if('> SPACES? EXPRESSION SPACES? <','> EXPRESSION SPACES? <','> EXPRESSION <')'> ) /
               (SPACES? <'if'> SPACES EXPRESSION SPACES <'then'>
               SPACES EXPRESSION SPACES <'else'> SPACES (OprExpression / EXPRESSION)) /
               (EXPRESSION SPACES <'?'> SPACES EXPRESSION SPACES <':'> SPACES (OprExpression / EXPRESSION));

  ConstExpr = SPACES? (Number / Keyword / StringLit / RegexLit) SPACES?;

  RegexLit = #'" regexlit "'

  FieldField =  (SPACES? <'.'> IDENTIFIER)

  ForceField =  (SPACES? <'$.'> IDENTIFIER)

  MethodMethod = (SPACES? <'.'> IDENTIFIER <'('> (SPACES? EXPRESSION SPACES? <','> )* (SPACES? EXPRESSION)? SPACES? <')'>)

  ForceMethod = (SPACES? <'$.'> IDENTIFIER <'('> (SPACES? EXPRESSION SPACES? <','> )* (SPACES? EXPRESSION)? SPACES? <')'>)

  FieldExpr = SPACES? IDENTIFIER (ForceMethod / MethodMethod / FieldField / ForceField)+ SPACES?;

  <FunctionExpr> = FunctionExpr1 / HashFunctionExpr / HashFunctionExpr2 / HashFunctionExpr3 / PartialFunction / DotFuncExpr / Partial1 / Partial2 / Partial3

  PartialFunction = (SPACES? ('|' | '<|') SPACES FuncCall)

  HashFunctionExpr = SPACES? <'#'> SPACES (DotFuncExpr / EXPRESSION2)

  HashFunctionExpr2 = SPACES? <'#2'> SPACES (DotFuncExpr / EXPRESSION2)

  HashFunctionExpr3 = SPACES? <'#3'> SPACES (DotFuncExpr / EXPRESSION2)

  DotFuncExpr = SPACES? <'.'> IDENTIFIER

  FunctionExpr1 = (IDENTIFIER | (<'('> SPACES? (IDENTIFIER SPACES?
                 <','> SPACES?)*  IDENTIFIER SPACES? <','>? SPACES? <')'> ) )
                 SPACES? <'=>'> SPACES? EXPRESSION2

  URL = #'\\b(https?|ftp|file|twtr)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]'

  Number = #'(\\-|\\+)?((([0-9]{1,3}(,[0-9]{3})+)|[0-9]+)\\.[0-9]*|[0-9]*\\.[0-9]+|(([0-9]{1,3}(,[0-9]{3})+)|[0-9]+))' NumberQualifier?;

  NumberQualifier = ('%' | '#minutes' | '#hours' | '#seconds' | '#days');

  MapExpr =(SPACES? <'{'> SPACES? <'}'> SPACES?) /
           (SPACES? <'{'> (Pair <','>)* Pair (<','> SPACES?)? <'}'> SPACES?);

  VectorExpr = SPACES? <'['> SPACES?  <']'> SPACES? |
               SPACES? <'['> (EXPRESSION <','>)* EXPRESSION (<','> SPACES?)? <']'> SPACES?;

  SetExpr = SPACES? <'#{'> SPACES? <'}'> SPACES? | SPACES? <'#{'> (EXPRESSION <','>)* EXPRESSION (<','> SPACES?)? <'}'> SPACES?;

  DottedThing = SPACES? <'.'> IDENTIFIER SPACES?

  Pair = (DottedThing / EXPRESSION) (<'->'> / SPACES) EXPRESSION;

  StringLit = #'" stringlit "' / #'\"(?:(?:[\\\\]\")|[^\"])*?\"'

  Operator = Op1 | Op2 | Op3 | Op4 | Op5 | Op6 | Op7 | Op8 | Op9 | Op10;
  "))

(def line-parser
  "Returns a parser that starts with grammar rule `Line'. This parser will return a parse tree even if the input isn't valid. The error will be embedded in part of parse tree."
  (insta/parser parse-def
                :start :Line
                :total true))

(def xform-rules
  "A map for instaparse's transform. This is used to evaluate parse tree.
  Keys are keywords, values are functions.
  This is called like this:
  (instaparse.core/transform xform-rules parsetree)"
  {:Lines (fn [& x] x)

   :Line (fn [& x] (first  x))

   :BlockExpression (fn [& x] `(do ~@x))

   :Number (fn
             ([x] (read-string (.replace  x "," "")))
             ([x [_ qual]]
              (let [x (read-string (.replace  x "," ""))]
                (* x (get multipliers qual)))))

   :GetExpression (fn [a & b] `(~'-> ~a ~@(map (fn [z] `(~'get ~z)) b)))

   :Pair (fn [a b] `[~a ~b])

   :MapExpr (fn [& x] (into {} x))

   :URL identity

   :FieldField (fn [x] `(visi.core.parser/thread-it
                         (if (clojure.core/map? ~'it)
                           (clojure.core/get ~'it ~(keyword x))
                           (~(symbol (str "." x)) ~'it))))

   :ForceField (fn [x] (symbol (str "." x)))

   :MethodMethod (fn [x & others] `(~(symbol (str x)) ~@others))

   :ForceMethod (fn [x & others] `(~(symbol (str "." x)) ~@others))

   :FieldExpr (fn [root & others]
                `(~'-> ~root ~@others))

   :SINK (fn [name expression]
           (let [name (symbol name)
                 res `(~'def ~name
                             (do
                               (visi.core.runtime/do-sink (quote  ~name) ~expression)
                               (visi.core.runtime/visi-realize ~expression)))]
             (insert-meta res {:visi-sink true})))

   :Source (fn
             ([x] `(visi.core.runtime/visi-source ~x))
             ([x v] `(visi.core.runtime/visi-source ~x ~v)))

   :MergeExpr (fn [core & others] `(~'merge ~core ~@others))

   :FunctionExpr1 (fn [& all]
                    (let [vars (drop-last all)
                          expr (last all)]
                      `(~'fn [~@vars] ~expr)))

   :DotFuncExpr (fn [x] `(~'fn [~'it]
                               (if (clojure.core/map? ~'it)
                                 (clojure.core/get ~'it ~(keyword x))
                                 (~(symbol (str "." x)) ~'it))))

   :PipeExpression (fn [root & pipeline]
                     (let [x `x#]
                       `(~'as-> ~root ~x ~@(map #(% x) pipeline))))

   :PipeFunctionExpression (fn [& pipeline]
                             (let [x `x#
                                   y `y#]
                               `(fn [~y] (~'as-> ~y ~x ~@(map #(% x) pipeline)))))

   :Pipe2FunctionExpression (fn [& pipeline]
                              (let [x `x#
                                    y `y#]
                                `(fn [~y] (~'as-> ~y ~x
                                                  ~@(map (fn [z] `(~z ~x))
                                                         pipeline)))))

   :EmptyLine (fn [& _] nil)

   :Mapcommand (fn [x] (fn [inside] `(~'visi.core.runtime/v-map ~inside ~x)))

   :Foldcommand (fn
                  ([x]
                   (fn [inside] `(~'visi.core.runtime/v-fold ~inside {} ~x)))
                  ([x y]
                   (fn [inside] `(~'visi.core.runtime/v-fold ~inside ~x ~y))))

   :Reducecommand (fn
                    ([x]
                     (fn [inside] `(~'visi.core.runtime/v-reduce ~inside ~x))))


   :Flatmapcommand (fn [x] (fn [inside] `(~'visi.core.runtime/v-flat-map ~inside ~x)))

   :Filtercommand (fn [x] (fn [inside] `(~'visi.core.runtime/v-filter ~inside ~x)))

   :Groupbycommand (fn [x] (fn [inside] `(~'visi.core.runtime/v-group-by ~inside ~x)))

   :Sortcommand (fn
                  ([x] (fn [inside]
                         `(~'visi.core.runtime/v-sort-by
                           ~inside ~x ~'visi.core.runtime/ascending)))
                  ([x order]
                   (fn [inside]
                     `(~'visi.core.runtime/v-sort-by
                       ~inside ~x ~(if (= "ascending" order)
                                     'visi.core.runtime/ascending
                                     'visi.core.runtime/descending)))))

   :StringLit (fn [x]
                (if (.startsWith x "#")
                  (->
                   (re-seq stringlit x)
                   first
                   (nth 2))
                  (let [c (count x)]
                    (-> x
                        (.substring 1 (- c 1))
                        (.replace "\\\"" "\"")
                        (.replace "\\n" "\n")
                        (.replace "\\\\" "\\")
                        (.replace "\\t" "\t")))))

   :RegexLit (fn [x]
               (->
                (re-seq regexlit  x)
                first
                rest
                rest
                first
                java.util.regex.Pattern/compile))

   :Keyword keyword
   :DottedThing keyword

   :ConstExpr identity
   :ParenExpr identity
   :FuncCall (fn [func & params] `(~func ~@params))

   ;; :VectorExpr (fn [& x] `(-> (list ~@x) vec)) ;; better to have a vector literal, but the analyzer barfs on vector lits
   :VectorExpr (fn [& x] `[~@x]) ;; better to have a vector literal, but the analyzer barfs on vector lits

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
                  (insert-meta res)))

   :Pipe2Expression (fn [nub & others]
                      (let [x `x#]
                        `(~'as-> ~nub ~x ~@(map (fn [y] `(~y ~x)) others))))

   :HashFunctionExpr (fn [x] `(~'fn [~'it] ~x))

   :HashFunctionExpr2 (fn [x] `(~'fn [~'it1 ~'it2] ~x))

   :HashFunctionExpr3 (fn [x] `(~'fn [~'it ~'it2 ~'it3] ~x))

   :PartialFunction (fn [type x]  (cons (if (= type "<|") 'visi.core.runtime/pre-partial 'partial) x))

   :Partial1 (fn [x] (-> x second second op-lookup))

   :Require (fn ([x] `(quote ~x))
              ([x y] `(quote [~x :as ~y])))

   :Partial2 (fn [v x]
               (let [x2 `x#]
                 `(~'fn [~x2] (~(-> x second second op-lookup) ~v ~x2))))

   :Partial3 (fn [x v]
               (let [x2 `x#]
                 `(~'fn [~x2] (~(-> x second second op-lookup) ~x2 ~v))))

   :Namespace (fn [ns & x]
                (let [ret
                      `(~'do
                         (~'ns ~ns (:require [cemerick.pomegranate]))
                         ~@(mapcat
                            (fn [z]
                              (when (= :Load (first z))
                                (map
                                 (fn [q]
                                   `(cemerick.pomegranate/add-dependencies
                                     :coordinates '[~q]
                                     :repositories (merge cemerick.pomegranate.aether/maven-central
                                                          {"clojars" "http://clojars.org/repo"})))
                                 (rest z)))) x)

                         ~@(mapcat
                            (fn [z]
                              (when (= :Import (first z))
                                (map
                                 (fn [q]
                                   `(~'clojure.core/import ~q))
                                 (rest z))))
                            x)

                         ~@(mapcat
                            (fn [z]
                              (when (= :Requires (first z))
                                (map
                                 (fn [q]
                                   `(~'clojure.core/require ~q))
                                 (rest z))))
                            x)
                         (~'def ~'$$package$$ {:x (quote ~x)
                                               :ns (quote ~ns)})
                         )]
                  (with-meta ret {:dont-fix true :source `(quote ret)})))

   ;; :Import (fn [& x] `(:import ~@x))

   :NamespaceName symbol

   :Loadable (fn [x y] `[~x ~y])

   :EXPRESSION identity
   :EXPRESSION2 identity
   :EXPRESSION3 identity
   :IDENTIFIER (fn [x] (cond
                         (= "nil" x) nil
                         (= "true" x) true
                         (= "false" x) false
                         :else
                         (-> x (.replace "$-" "-") (.replace "$." ".") symbol)))
   :ClojureSymbol (fn [x] (-> x (.replace "::" "/") symbol))
   :FullSymbol symbol})

;; something that is maybe a class that's not found
;; might very well be a method invocation, so we'll treat it
;; that way
(defmethod ef/-emit-form :maybe-method
  [x opts]
  (let [ret (:form x)]
    (if (or
         (namespace ret)
         (not (re-matches #"[a-zA-Z_$][a-zA-Z\d_$]*" (name ret)))
         (-> ret name (thread-it (<= 1 (.indexOf it ".")))))
      ret
      (->> (str "." ret) symbol))))

(defn fixup-method-calls
  "Uses the JVM analyzer to look at function calls
that are not associated with a namespace and turn them
into method invocations. So, toString(33) becomes
(.toString 33) rather than (toString 33)"
  [code namespace opts]
  (if (or
       (-> code meta :dont-fix)
       (and
        (seq? code)
        (= 'ns (first code))))
    {:failed false :res code}
    (clojure.main/with-bindings
     (in-ns (.name namespace))
     (vu/fix-namespace)
     (try
       (->
        code
        (ca/analyze
         (assoc
          (ca/empty-env)
          :locals
          (->> opts
               :locals
               (map (fn [x]
                      [(-> x name symbol)
                       (if (clojure.core/namespace x)
                         {:op :def :name x :var x :children []}
                         {:op :binding :name x :form x
                          :local :let})]))
               (into {})))
         {:passes-opts
          {:validate/unresolvable-symbol-handler
           (fn [_ _ c]
             (assoc c :op :maybe-method))}})
        (e/emit-form (or (:emit opts) {:qualified-symbols true :hygienic true}))
        (thread-it {:failed false :res it}))
       (catch Exception _
         {:failed false :res code});; if we get an exception, just punt
))))

(defn split-into-lines
  "Split the String into a series of lines grouped by lines that have a leading none-space 1st char"
  [the-str]
  (let [lines (.split the-str "\n")
        lines (remove #(-> % .trim empty?) lines)
        acc (atom [])

        mostly-split
        (mapcat
         (fn [ln]
           (if (.startsWith ln " ")
             (do  (swap! acc conj ln) nil)
             (if (empty? @acc)
               (do  (swap! acc conj ln) nil)
               (let [ret @acc]
                 (reset! acc [ln])
                 [ret]))))
         lines)

        split (conj (vec mostly-split) @acc)]
    (map #(as-> % z (clojure.string/join "\n" z) (str z "\n")) split)))

(defn post-process
  "(post-process parse-tree)
 (post-process parse-tree namespace)
Transform parse-tree into Clojure form, returns a map.
parse-tree is Instaparse's parse tree.
If the parse-tree contains parse error, return
 {:failed true :error transform-result}
else, return
 {:failed false :res transform-result}
transform-result is Clojure form, e.g. `(def x 4)
"
  ([parse-tree namespace opts]
   (let [do-fix-calls
         (fn [x]
           (if (:dont-fix opts)
             {:failed false :res  x}
             (fixup-method-calls x namespace opts)))]
     (if (instance? instaparse.gll.Failure parse-tree)
       {:failed true :error parse-tree}
       (->
        (insta/transform xform-rules parse-tree)
        do-fix-calls)))))

(defn- remove-hygene
  "Removes some of the hygenic symbols... makes for stables tests"
  [x]
  (cond
    (seq? x) (map remove-hygene x)
    (vector? x) (mapv remove-hygene x)
    (and
     (symbol? x)
     (re-seq  #"__\d+__auto__$" (str x)))
    (symbol (clojure.string/replace (str x)  #"__\d+__auto__$"  ""))
    :else x))

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
  ([the-line] (parse-line the-line *ns* {}))
  ([the-line namespace opts]
   (binding [*current-line* the-line]
     (-> the-line .trim (str "\n") line-parser (post-process namespace opts)))))

(defn- pass-seq
  "Passes a seq, otherwise nil"
  [x]
  (if (seq? x) x nil))

(defn parse-multiline
  "Parse all of the visi code
  Arguments: the-line, namespace
  the-line is a string.
  namespace is a symbol (or string?) TODO
  If namespace not given, defaults to nil.
  Returns a map. If failed, returns
  {:failed true, :error parse-tree}
  else, return
  {:failed false, :res parse-result}"
  ([the-file] (parse-multiline the-file *ns* {}))
  ([the-file namespace opts]
   (let [names (atom [])]
     (->>
      the-file
      split-into-lines
      (map line-parser)
      (map #(let [answer (post-process % namespace (merge opts {:locals @names}))]
              (when (some-> answer :res pass-seq first (= 'def))
                (swap! names conj (-> answer :res second)))
              answer))))))

(defn parse-and-eval-multiline
  "parses all the lines. If they are all legal, then
eval all the lines"
  ([the-file] (parse-and-eval-multiline the-file *ns* {}))
  ([the-file namespace opts]
   (let [answers (parse-multiline the-file namespace opts)

         mostly
         (if (every? #(-> % :failed not) answers)
           (->> answers
                (map :res)
                (remove #(nil? %))
                (map eval)
                (remove #(and
                          (not (-> % meta :visi-sink))
                          (instance? clojure.lang.Var %)))
                (map #(if (instance? clojure.lang.Var %)
                        (deref %)
                        %))
                )
           answers
           )]
     (if (= 1 (count mostly)) (first mostly) mostly))))

(defn parse-for-tests
  "Parse the line into an S-expression"
  [code & locals]
  (-> code (parse-line *ns* {:locals locals :emit {}}) :res remove-hygene))

(defn parse-and-eval-for-tests
  "Parse and evaluate the expression. throws if the expression can't be parsed"
  [code]
  (let [res (parse-line code)]
    (if (:res res)
      (-> res :res eval)
      (throw (Exception. (pr-str res))))))

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
      code)))

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
   (filterv #(not (= 0 (count %))))))

;; TODO this function is not called anywhere
(defn process-notebook
  "Turn a Gorilla notebook into a Clojure file"
  [notebook]
  (->> notebook
       notebook-to-strings
       (map pre-process-line)
       (cljstr/join "\n\n")))

;; register a hook for the current file if the REPL stuff is loaded
