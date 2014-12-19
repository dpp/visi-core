# REPL Overview

How is Visi different from Gorilla? What's the diff of the input from gorilla?

## Background

Turns out that Hadoop and Spark are just Lisp-ish things, but across a cluster of machines.
Hadoop and Spark are engines for distributing the map-reduce thing across address spaces (machines).
Yes, in Spark, you also get filter and mapcat (called flatMap) and a few other collections manipulation functions
But at the end of the day, all Hadoop and Spark are are fancy ways to apply functions over collections and then reduce (fold) the result.
So in your mental map of things, put a placeholder that says, "Spark & Hadoop == applying functions over collections"
or "they are List Processors across an array of machines."

This abstraction is actually meaningful in Visi.
Because Visi has some custom-built syntax to describe "apply this function across this collection" and we reify the concept into the specific API calls necessary to run that Java bytecode on Spark (and soon Hadoop).
But thinking core Lisp concepts will help you stay away from the weeds.


## So, Visi...

I did an "elegant engineering solution" (aka a hack) to get Gorilla REPL to support the Visi syntax.
If you look in the `middleware.clj` file (probably needs to be renamed something more meaningful), I created a Clojure REPL middleware module.

Much of Clojure is built on what they call "middleware" and what others call a processing pipeline.
Clojure middleware is the ability to insert a phase into a processing pipeline.

So, the REPL supports middleware and Ring (the web framework) supports middleware.
What I did with Visi is to insert a processing phase just before the actual REPL takes the String the user types, passes it into the reader and then passes that s-expression into eval.
And the hack is that I look at the string... if it doesn't start with a "Clojure character: `(`, `[`, or `{`" I call the Visi parser (in `parser.clj`) to parse the `String` in the Visi grammar (using Instaparse) and then transliterate the AST into Clojure.
So, `parser.clj` does String -> AST -> S-expression work.

That S-expression is turned back into a `String` and forwarded to the normal REPL's Read/Eval phase.

In the Visi web repl, you can type Clojure (for example, the `ns` expression at the first line of the Notebook) and it just gets passed through unchanged.
Or you can type something that isn't Clojure-like and `middleware.clj` will pass it to `parser.clj` to get an S-expression that's then turned back into a `String` and sent on down the chain.

Visi is just a shim in the REPL plus a few other hacks.
But Visi and Gorilla REPL are pretty much the same thing.
There are maybe 500 lines of code in Visi.
It's not huge, but the difference in what it presents to the end user is huge.
