# How to convert an simple piece of Clojure into an agnostic Map, Reduce, etc. command

The previous version of runtime was a pure hack that worked for the Visi demo and worked in a local JVM, but didn't work distributed.
The previous version also only worked with Spark.
This version has a couple of major changes.

First, all of the operations (e.g., map, flatMap, etc.) are done as macros. This is so that the function parameter can be captured as an S-expression rather than a compiled Clojure function.
It turns out that distributing the bytecode for Clojure functions to the Spark cluster was non-trivial.
Yes, Spark does it with Scala, but there's a significant amount of ASM magic where-in the bytecode is re-written.
Plus, I can't figure out how Clojure turns a function into bytecode, so I can't capture the bytecode before it gets turned into a JVM class.

So, instead of sending serialized Clojure functions, sending the S-expression over the wire makes much more sense.
The Java file `SerializableFunctions.java` contains the classes that are both a Spark function and something that can be cleanly serialized.

So, we serialize the S-expression and when the function gets called, if the S-expression hasn't been compiled, eval is called on the s-expression and boom... a Clojure `IFn`.

There's still some work that needs to be done because we don't necesssarily have all the "requires" context set up for the Clojure runtime on each node.
Luckily, we can analyze the S-expression and get that information (not done yet, but there's a simple place to do it in the code).

So, the `runtime/v-map` macro returns `(ti-map ~this (quote ~(clean-it-up func)))`.

The `clean-it-up` function runs the Clojure analyzer on the S-expression and yields an S-expression with fully materialized namespaces, etc. It's in `clean-it-up` that we can also add the requires analysis.

`clean-it-up` relies on https://github.com/clojure/jvm.tools.analyzer which gives us a very nice intermediate format for the S-express that we can walk and discover all sorts of nice stuff about the expression (including anything we have to explicitly require).

`ti-map` is a function in the `TransformInfo` protocol.
Protocols are like Java interfaces... a Protocol can be implemented for each class where the class is the class of the instance the first paramter to the call. It's grouped polymorphic dispatch based on the first parameter.
The nice thing is that you can add an existing Java class to a Protocol without adding the interface to the class. It's Clojure's answer to the Expression Problem http://en.wikipedia.org/wiki/Expression_problem .

In order to support `ti-map` for *any* compute backend, we just extend the back end's "thing that represents data" (in the case of Spark, it's an RDD) to support the TransformInfo protocol.
So, ti-map for a Spark RDD looks like:

```
   :ti-map
   (fn [^JavaRDDLike this func]
     (.map
      this
      (func-for this func)))
```

`func-for` is a function on the `FunctionFor` protocol.
This protocol wraps the S-expression representing the function with a wrapper that the target runtime expects (in this case, it's a http://spark.apache.org/docs/latest/api/java/org/apache/spark/api/java/function/Function.html ).
So, `func-for` takes the S-expression and turns it into a Spark Function.
This is done by:

```
:func-for (fn [this form] (SerializableFunctions/mkFunction form))
```

This calls:

```
    public static <A, B> Function<A, B> mkFunction(Object p) {
        return new VFunction<A, B>(p);
    }
```

And boom, we have a function that can be serialized and sent across the Spark cluster.
This also means that we can implement the `FunctionFor` protocol to support whatever arbitrary system we want (Hadoop MapReduce, Storm, etc.)
