2014-11-30
Visi Syntax
2014-12-06 this is incomplete

────────── ────────── ────────── ────────── ──────────
# Line comment start with ;

example
    ; this is a comment

# Block comment `/* ... */`

example:

    /* this is
    a multi-line
    comment
    */

────────── ────────── ────────── ────────── ──────────
#Arithmetics

addition
example
    3 + 2
returns 5

substraction
example
    3 - 2
returns 1

multiplication
example
    3 * 2
returns 6

Power (exponential)
example
    3 ^ 2
returns 9.0

division and fracation
example
    3 / 2
returns an exact fraction 3/2

    3 / 2.0
returns 1.5

Join String
&
example
    "a" & "b"
returns
"ab"

────────── ────────── ────────── ────────── ──────────
#Comparison

You can use
    x < y
    x > y
    x <= y
    x >= y

    x == y
    x != y
    x <> y

「!=」 is the same as 「<>」

────────── ────────── ────────── ────────── ──────────
#boolean operator

「true」  is builtin value for true.
「false」  is builtin value for false.

“and” operator is 「&&」
example
    x && y

“or” operator is 「||」
example
    x || y

────────── ────────── ────────── ────────── ──────────
#number

example
    3
    +3
    -3
    3.
    +3.
    -3.
    3.1
    +3.1
    -3.1

number quantifiers

Number can follow a quantifier

    1.seconds  # same as 1000
    1.minutes # same as 60 * 1000
    1.hours
    1.days
    1% # same as 1/100
    1.% # same as 0.01

────────── ────────── ────────── ────────── ──────────
#url
example

    http://google.com
    https://google.com
    http://google.com:80/
    file://…
    ftp://…
    twtr://…

────────── ────────── ────────── ────────── ──────────
#function

function definition has this syntx:
    ‹name›( ‹param name 1›, ‹param name 1›, …) = ‹expression›;

example
    f(x, y) = x + y

────────── ────────── ────────── ────────── ──────────
visi expression

Each line is one of:

* constant definition
* function definition
* Source
* expression

A constant definition looks like this:
    ‹name› = ‹expression›;

A “sink” looks like this
    sink ‹name› = ‹EXPRESSION›
or
    sink: ‹name› = ‹EXPRESSION›

example
    sink wc2 = wc

A “source” looks like this

    source ‹name›
    source ‹name› = ‹URL›
    source ‹name› = ‹EXPRESSION›

example
    source cities = "https://www.census.gov/geo/reference/codes/files/national_county.txt"

Expression computes a value.

 EXPRESSION2 / Pipe2Expression / PipeExpression

Expression computes a value.

PipeExpression is something like

 ‹expression› |> ‹PipeCommands›
 ‹expression› |> ‹PipeCommands› |> ‹PipeCommands›

example

    lower = .cache(info |> map .toLowerCase)

    sins = lower |> filter # (.contains(it, "sin") && not(.contains(it, "sing")))

    sins-plus-god-or-christ = sins |> filter # begin

    twit = v/stream-into-watching((v/create-twitter-stream({:duration -> 5000})) |> map .getText |> map calc_sent |> filter # (1 < it.pos || 1 < it.neg) |> reduce | merge-with((+)))

    lower-bible = (bible |> map .toLowerCase) >> # .cache(it)

    wc = (lower-bible |> mapcat # .split(it, "\\W+")) >> # v/v-map-to-pair(it, # [it, 1] ) >> # v/v-reduce-by-key(it, (+))

  PipeExpression = (ParenExpr / IDENTIFIER) (SPACES '|>' SPACES PipeCommands )+

  <PipeCommands> = Mapcommand | Flatmapcommand | Filtercommand |
                   Zipcommand | Dropcommand | Sortcommand |
                   Samplecommand | Foldcommand | Productcommand |
                   Groupbycommand

  ParenExpr = Partial1 / Partial2 / Partial3 / (SPACES? ( EXPRESSION ) SPACES?);

BlockExpression start with “begin” and ends with “end”. In between, it can be one or more expressions, each on a line.

  EXPRESSION2 = BlockExpression / GetExpression /
  IfElseExpr / FuncCall / ParenExpr /  ConstExpr /
  FieldExpr / FunctionExpr / MapExpr / VectorExpr /
  (SPACES? (IDENTIFIER | ClojureSymbol) SPACES?) /
  InlineFunc / MergeExpr / OprExpression

  Pipe2Expression = EXPRESSION2  (SPACES <'>>'> SPACES (FunctionExpr / EXPRESSION2))+;

  PipeExpression = (ParenExpr / IDENTIFIER) (SPACES <'|>'> SPACES PipeCommands )+

lower = .cache(info |> map .toLowerCase)
sins = lower |> filter # (.contains(it, "sin") && not(.contains(it, "sing")))
sins-plus-god-or-christ = sins |> filter # begin

build_set(str) = s/split-lines(str) >> | map(.trim) >> | filter(# 0 < count(it)) >> | remove(# .startsWith(it, "

────────── ────────── ────────── ────────── ──────────
#commands

xform …
map …

xform-cat …
mapcat …
flatmap …

filter …
sort …

group …
group by …

join …
zip …

product …

drop …
sample …

fold …
reduce …

example
lower = .cache(info |> map .toLowerCase)
map(.trim)

Flatmapcommands
xform-cat ‹arg›
mapcat ‹arg›
flatmap ‹arg›

────────── ────────── ────────── ────────── ──────────
merge map.

 You can use 「%%」 to merge 2 maps.

example
    aa = {"a" -> 7, "b" -> 8}
    bb = {"c" -> 9, "d" -> 2}
    aa %% bb
returns
    {"c" 9, "d" 2, "a" 7, "b" 8}

todo: not exactly sure what NeverMatch is for.

────────── ────────── ────────── ────────── ──────────
map, array, set

a “map” is a set of pairs, each pair is a key and value.
Write it like this:
    oo = {"xx" -> 3, "yy" -> 4}

a array is a ordered list of items.
Write it like this:
    [7, 2, "something"]

a set is a collection of items. Items should not appear more than once.
Write it like this:
    #{7 2 "something"}

────────── ────────── ────────── ────────── ──────────
string escape

inside a string,
\n means newline
\t means tab
\\ means \
\" means "

backslash followed by anything else does not have special meaning.
example
"\9" means 「\9」

────────── ────────── ────────── ────────── ──────────

