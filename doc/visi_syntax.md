# Visi Syntax

# Comments

Line comment start with `;;`

    ;; this is a comment

<!-- Block comment starts with `/*` and ends with `*/` -->
<!-- Example: -->

<!--     /* this is -->
<!--     a multi-line -->
<!--     comment -->
<!--     */ -->

<!-- block comment can be nested. -->

<!-- todo: block comment doesn't work -->

# Strings

String is writen using double quotes.
Example:

    "This is a string."

Single quote, such as `'something'`, is not supported.

To join 2 strings, use the `&` operator.
Example:

    ;; join 2 strings
    "a" & "b" ;; "ab"

## String Escape

Inside a string, the following has special meaning:

* `\n` means newline
* `\t` means tab
* `\\` means \
* `\"` means "

Backslash followed by anything else does not have special meaning. For example: `\9` means `9`

# Number

Example:

    3
    +3
    -3
    3.
    +3.
    -3.
    3.1
    +3.1
    -3.1

Number can be followed by a quantifier.
Example:

    1.seconds  ;; same as 1000
    1.minutes  ;; same as 60 * 1000
    1.hours
    1.days
    1%         ;; same as 1/100
    1.%        ;; same as 0.01

# Arithmetics

    3 + 2   ;; 5
    3 - 2   ;; 1
    3 * 2   ;; 6
    3 ^ 2   ;; exponential. Returns 9.0
    3 / 2   ;; returns an exact fraction 3/2
    3 / 2.0 ;; 1.5

Note: you must have a space between operator and its operand.

# Comparison

Standard comparison operators are supported. Example:

    x < y
    x > y
    x <= y
    x >= y

    x == y
    x != y
    x <> y  ;; same as !=

# Boolean

`true` is builtin value for true.
`false` is builtin value for false.

“and” operator is `&&`
Example:

    x && y

“or” operator is `||`
Example:

    x || y
    1 > 3 || 3 > 1

# URL

URL can be used.
Example:

    http://google.com
    https://google.com
    file://…
    ftp://…
    twtr://…  ;; twitter API

URL is mostly used with `source` and `sink`.

A `source` looks like this

    source ‹name›
    source ‹name› = ‹URL›    ;; set ‹name› to ‹URL› content
    source ‹name› = ‹EXPRESSION›

<!-- todo: what does 「source ‹name›」 do by itself? -->

`source` is used to get content of a URL.
Example:

    source cities = "https://www.census.gov/geo/reference/codes/files/national_county.txt"

A “sink” looks like this

    sink ‹name› = ‹EXPRESSION›

or

    sink: ‹name› = ‹EXPRESSION›

Example:

    sink wc2 = wc

<!-- todo: what does sink do exactly, how it differ from source -->

# Variables

Variable can defined like this:

    x = 4

You must have a space around the equal sign.

# Array

A array (also known as “vector”) is a ordered list of items.
Example:

    [7, 2, "something"]
    x = [3, 4, 5]
    y = [3, 4, 5,]

Elements must be separated by comma.
Extra comma at the end is ok.

To access a element of a array, append a square bracket [] after the expression.  Example:

    x = [3, 4 , 5]
    x[2] ;; returns 5

# Map

A “map” is an unordered set of pairs, each pair is a key and value.
Example:

    myMap = {"xx" -> 3, "yy" -> 4}
    myMap = {"xx" -> 3, "yy" -> 4, }

<!-- todo the key in map can also be a DottedThing -->

Use `%%` to join 2 maps.

Example:

    aa = {"a" -> 7, "b" -> 8}
    bb = {"c" -> 9, "d" -> 2}
    aa %% bb ;; returns {"c" 9, "d" 2, "a" 7, "b" 8}

# Function

Function definition has this syntax:

    ‹name›() = ‹expression›
    ‹name›(‹x›) = ‹expression›
    ‹name›(‹x›, ‹y›, …) = ‹expression›

Example:

    f(x, y) = x + y

# Piping

<!-- EXPRESSION = EXPRESSION2 / Pipe2Expression / PipeExpression -->
<!-- Pipe2Expression = EXPRESSION2  (SPACES <'>>'> SPACES (FunctionExpr / EXPRESSION2))+; -->
<!-- PipeExpression = (ParenExpr / IDENTIFIER) (SPACES <'|>'> SPACES PipeCommands )+ -->

   <!-- :Pipe2Expression (fn [nub & others] -->
   <!--                    (let [x `x#] -->
   <!--                      `(~'as-> ~nub ~x ~@(map (fn [y] `(~y ~x)) others)))) -->

   <!-- :PipeExpression (fn [root & pipeline] -->
   <!--                   (let [x `x#] -->
   <!--                     `(~'as-> ~root ~x ~@(map #(% x) pipeline)))) -->

<!-- (vp/pre-process-line "3 >> map") -->

An expression can be fed into a command or function, much like Linux's pipe `|`.

You can “pipe” is something like

    ‹expression› >> ‹function›
    ‹expression› |> ‹PipeCommands›

<!-- the difference between >> and |> seems to be that one is for piping to function, and the other is for visi command -->

 ‹expression› |> ‹PipeCommands› |> ‹PipeCommands›

<!-- ["AB", "CD"] |> map .toLowerCase -->

# Pipe Commands

<!-- <PipeCommands> = Mapcommand | Flatmapcommand | Filtercommand | Zipcommand | Dropcommand | Sortcommand | Samplecommand | Foldcommand | Productcommand | Groupbycommand -->
<!-- the following covers the complete PipeCommands -->

Each of the commands has the syntax of `‹cmd name› ‹values›` todo
The value can be todo

<!-- Mapcommand = (<'xform'> | <'map'>) SPACES (IDENTIFIER | Keyword | FunctionExpr) -->
   <!-- :Mapcommand (fn [x] (fn [inside] `(~'visi.runtime/v-map ~inside ~x ))) -->

    xform ‹name,keyword,ƒ›
    map ‹name,keyword,ƒ›

<!-- Flatmapcommand = (<'xform-cat'> | <'mapcat'> | <'flatmap'>) SPACES (IDENTIFIER | Keyword | FunctionExpr) -->
   <!-- :Flatmapcommand (fn [x] (fn [inside] `(~'visi.runtime/v-flat-map ~inside ~x ))) -->

    xform-cat ‹name,keyword,ƒ›
    mapcat ‹name,keyword,ƒ›
    flatmap ‹name,keyword,ƒ›

<!-- Filtercommand = (<'filter'>) SPACES (IDENTIFIER | Keyword | FunctionExpr) -->
   <!-- :Filtercommand (fn [x] (fn [inside] `(~'visi.runtime/v-filter ~inside ~x ))) -->

    filter ‹name,keyword,ƒ›

<!-- Sortcommand = (<'sort'>) SPACES (IDENTIFIER | Keyword | FunctionExpr) (SPACES <','> ('ascending' | 'descending'))? -->
   <!-- :Sortcommand (fn -->
   <!--                ([x] (fn [inside] `(~'visi.runtime/v-sort-by ~inside ~x true))) -->
   <!--                ([x order] (fn [inside] `(~'visi.runtime/v-sort-by ~inside ~x (= order 'ascending'))))) -->

    sort ‹name,keyword,ƒ›
    sort ‹name,keyword,ƒ›, ascending
    sort ‹name,keyword,ƒ›, descending

<!-- Groupbycommand = (<'group by'> / <'group'> ) SPACES (IDENTIFIER | Keyword | FunctionExpr) -->
   <!-- :Groupbycommand (fn [x] (fn [inside] `(~'visi.runtime/v-group-by ~inside ~x ))) -->

    group ‹name,keyword,ƒ›
    group by ‹name,keyword,ƒ›

<!-- Zipcommand = (<'join'> | <'zip'>) SPACES (IDENTIFIER | (<'('> (SPACES? IDENTIFIER SPACES? <','> SPACES?)+ <')'> )) -->
<!-- not implemented? -->
    join ‹name›
    join (‹elem1›, ‹elem2›, …)
    zip ‹name›
    zip (‹elem1›, ‹elem2›, …)

<!-- Productcommand = (<'product'> ) SPACES (IDENTIFIER | (<'('> (SPACES? IDENTIFIER SPACES? <','> SPACES?)+ <')'> )) -->
<!-- not implemented? -->
    product ‹name›
    product (‹name›)
    product (‹name1›, ‹name2›, …)

<!-- Dropcommand = (<'drop'>) SPACES (IDENTIFIER | ConstExpr | ParenExpr) -->
    drop ‹(IDENTIFIER | ConstExpr | ParenExpr)›

<!-- Samplecommand = (<'sample'>) SPACES (IDENTIFIER | ConstExpr | ParenExpr) -->
    sample ‹(IDENTIFIER | ConstExpr | ParenExpr)›

<!-- Foldcommand = (<'fold'> | <'reduce'>) SPACES (((IDENTIFIER | ConstExpr | ParenExpr | MapExpr | VectorExpr) SPACES <'->'> SPACES)? (IDENTIFIER | FunctionExpr)) -->
    fold …
    reduce …

Example:
todo

    lower = .cache(info |> map .toLowerCase)
    map(.trim)

Example:
todo

    lower = .cache(info |> map .toLowerCase)

    sins = lower |> filter # (.contains(it, "sin") && not(.contains(it, "sing")))

    sins-plus-god-or-christ = sins |> filter # begin

    twit = v/stream-into-watching((v/create-twitter-stream({:duration -> 5000})) |> map .getText |> map calc_sent |> filter # (1 < it.pos || 1 < it.neg) |> reduce | merge-with((+)))

    lower-bible = (bible |> map .toLowerCase) >> # .cache(it)

    wc = (lower-bible |> mapcat # .split(it, "\\W+")) >> # v/v-map-to-pair(it, # [it, 1] ) >> # v/v-reduce-by-key(it, (+))

    lower = .cache(info |> map .toLowerCase)
    sins = lower |> filter # (.contains(it, "sin") && not(.contains(it, "sing")))
    sins-plus-god-or-christ = sins |> filter # begin

    build_set(str) = s/split-lines(str) >> | map(.trim) >> | filter(# 0 < count(it)) >> | remove(# .startsWith(it, "

# Expressions

<!-- EXPRESSION = EXPRESSION2 / Pipe2Expression / PipeExpression -->

EXPRESSION2 =
BlockExpression /
  BlockExpression = SPACES? <'begin'> (SPACES |  LineEnd)* (EXPRESSION LineEnd SPACES*)* EXPRESSION LineEnd* SPACES* <'end'> LineEnd?;
   :BlockExpression (fn [& x] `(do ~@x))


GetExpression /
  GetExpression = SPACES? IDENTIFIER (<'['> EXPRESSION <']'>)+ SPACES?


IfElseExpr /

  IfElseExpr = (SPACES? <'if'> SPACES EXPRESSION SPACES <'then'>
               SPACES EXPRESSION SPACES <'else'> SPACES (OprExpression / EXPRESSION)) /
               (EXPRESSION SPACES <'?'> SPACES EXPRESSION SPACES <':'> SPACES (OprExpression / EXPRESSION));
:IfElseExpr (fn [test a b] `(~'if ~test ~a ~b))

FuncCall /
  FuncCall = SPACES? (IDENTIFIER | ClojureSymbol) SPACES?
             <'('> (EXPRESSION <','>)*
                   (EXPRESSION <','>?)? SPACES? <')'> SPACES?;
   :FuncCall (fn [func & params] `(~func ~@params))


ParenExpr /
  ParenExpr = Partial1 / Partial2 / Partial3 / (SPACES? <'('> EXPRESSION <')'> SPACES?);
   :ParenExpr identity

ConstExpr /
  ConstExpr = SPACES? (Number | Keyword | StringLit | RegexLit) SPACES?;
   :ConstExpr identity

FieldExpr /
  FieldExpr = SPACES? IDENTIFIER (SPACES? <'.'> IDENTIFIER)+ SPACES?;
   :FieldExpr (fn [root & others]
                `(~'-> ~root ~@(map (fn [x] `(~'get ~(keyword x))) others)))

FunctionExpr /
  <FunctionExpr> = HashFunctionExpr / PartialFunction / FunctionExpr1 / DotFuncExpr / Partial1 / Partial2 / Partial3

  FunctionExpr1 = SPACES? (IDENTIFIER | (<'('> SPACES? (IDENTIFIER SPACES?
                 <','> SPACES?)*  IDENTIFIER SPACES? <','>? SPACES? <')'> ) )
                 SPACES? <'=>'> SPACES? EXPRESSION SPACES?;

MapExpr /
  MapExpr = SPACES? <'{'> (Pair <','>)* Pair (<','> SPACES?)? <'}'> SPACES?;

VectorExpr /
  VectorExpr = SPACES? <'['> (EXPRESSION <','>)* EXPRESSION (<','> SPACES?)? <']'> SPACES?;
   :VectorExpr (fn [& x] `[~@x])

(SPACES? (IDENTIFIER | ClojureSymbol) SPACES?) /

InlineFunc /
  InlineFunc = SPACES? (ConstDef | FuncDef)+ SPACES EXPRESSION
   :InlineFunc (fn [& x] (process-inner (drop-last x) (last x)))

MergeExpr /
  MergeExpr = EXPRESSION (SPACES <'%%'> SPACES Pair)+;
   :MergeExpr (fn [core & others] `(~'merge ~core ~@others))

OprExpression
  OprExpression = SPACES? Op10Exp SPACES?
  Op10Exp =(Op9Exp SPACES Op10 SPACES Op9Exp) / Op9Exp;
   :OprExpression identity

Visi syntax are made of lines. Each line is one of:

* *Constant Definition*. Example: `x = 4`
* *Function Definition*. Example: `f(x) = x + 1`
* *Source Definition*. Loading a URL. Example: `source cities = "https://www.census.gov/.../cities.txt"`. See URL section.
* Expression. Example: `3 + 4`.

Expression computes a value. Visi supports many types of expressions.

# Block Expression
BlockExpression start with “begin” and ends with “end”. In between, it can be one or more expressions, each on a line.

--------------------------------------------------------------------------------

# Keyword

<!-- Keyword = <':'> IDENTIFIER; -->
<!-- todo find out what this do exactly -->

# DottedThing

<!-- todo find out what's DottedThing, how it behave -->

<!-- todo: not exactly sure what NeverMatch is for. -->

# Parenthesized Expressions

These seems to be used for grouping purposes only.

ParenExpr = Partial1 / Partial2 / Partial3 / (SPACES? <'('> EXPRESSION <')'> SPACES?);
    (‹expression›)

<!-- Partial1 = (SPACES? <'('> SPACES? Operator SPACES? <')'> SPACES?) -->
<!-- :Partial1 (fn [x] (-> x second second op-lookup)) -->

    (‹operator›)

<!-- Partial2 = (SPACES? <'('> SPACES? EXPRESSION SPACES Operator SPACES? <')'> SPACES?) -->
   <!-- :Partial2 (fn [v x] -->
   <!--             (let [x2 `x#] -->
   <!--               `(~'fn [~x2] (~(-> x second second op-lookup) ~v ~x2)))) -->
    (‹expression› ‹operator›)

<!-- Partial3 = (SPACES? <'('> SPACES? Operator SPACES EXPRESSION SPACES? <')'> SPACES?) -->
   <!-- :Partial3 (fn [x v] -->
   <!--             (let [x2 `x#] -->
   <!--               `(~'fn [~x2] (~(-> x second second op-lookup) ~x2 ~v )))) -->
    (‹operator› ‹expression›)

