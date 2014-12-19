-*- coding: utf-8 -*-

2014-11-15

# visi source code is this:

    .
    ├── gorilla_plot
    │   ├── core.clj
    │   ├── util.clj
    │   └── vega.clj
    ├── gorilla_repl
    │   ├── core.clj
    │   ├── files.clj
    │   ├── html.clj
    │   ├── image.clj
    │   ├── latex.clj
    │   ├── nrepl.clj
    │   ├── renderer.clj
    │   ├── render_values_mw.clj
    │   ├── table.clj
    │   ├── vega.clj
    │   ├── version.clj
    │   └── websocket_relay.clj
    ├── java
    │   └── visi
    │       └── SerializableFunctions.java
    └── visi
        ├── core.clj
        ├── middleware.clj
        ├── parser.clj
        ├── repl_starter.clj
        ├── runtime.clj
        └── t_stream.clj

The primary files are the ones in `visi` directory.

Also important are gorilla_repl directory.

Basically, visi relies on gorilla as the main framework. It modifies gorilla repl so that:

* some other syntax are supported
* computation is send to the cloud using various engines. e.g. Apache Spark, Hadoop.

# Visi Code Flow

in summary:

visi.repl_starter loads visi.parser,
visi.parser loads gorilla-repl.core,
gorilla-repl.core loads visi.runtime,
visi.runtime loads visi.middleware

note: t_stream.clj is not called.

## here's a log of what's called when starting visi

    ◆ ~/git/dpp/visi
    ◆ lein run

      repl_starter.clj called
      parser.clj called
      gorilla_repl/core.clj called
      runtime.clj called
      middleware.clj called
      Setting app params to  {:port 8990}
      Started nREPL server on port 42063
      Running at http://127.0.0.1:8990/

Here's sequence of load events:

`~/git/dpp/visi/project.clj`
〔~/git/dpp/visi/project.clj〕
the line
:main visi.repl-starter
runs that file.
the line
:aot [visi.repl-starter visi.runtime]
compiles that file.

here's order:

the
〔~/git/dpp/visi/src/visi/repl_starter.clj〕
calls
(:require [visi.parser :as vp])

and its 「-main」 calls
(require 'gorilla-repl.core)

the
〔~/git/dpp/visi/src/visi/parser.clj〕
calls
(:require
   [instaparse.core :as insta]
   )

instaparse is only used by parser.clj
instaparse is not in the “src” directory. It is a dependency lib declared in project.clj

it is in the directory named “target”, at
~/git/dpp/visi/target/base+system+user+dev/classes/instaparse/

the file
〔~/git/dpp/visi/src/gorilla_repl/core.clj〕
loads many things.
Of visi code, it loads
[visi.runtime :as v-run]

the file
〔~/git/dpp/visi/src/visi/runtime.clj〕
also loads many things, including:
[gorilla-renderable.core :as render]
[visi.middleware :as v-mid]

the file
〔~/git/dpp/visi/src/visi/middleware.clj〕
does load some things, but basically nothing about gorilla or visi that's not already loaded.

# File Function Contents

here's function or macro defined in each file

../src/visi/core.clj

    (defn -main

../src/visi/repl_starter.clj

    (defn -main [& args]

../src/visi/middleware.clj

    (def session-info (atom {}))
    (def ^{:dynamic true} current-message nil)
    (defn- handle-message
    (defn visi-eval
    (middleware/set-descriptor!

  find where they are called

    (rgrep "session-info" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "current-message" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "visi-eval" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "middleware/set-descriptor!" "*.clj" "~/git/dpp/visi/src/" nil)

../src/visi/parser.clj

    (def ^{:dynamic true :private true} *found-rdds* #{})
    (def ^{:dynamic true :private true} *name-lookup* {})
    (defn- deal-with-identifier
    (def parse-def
    (def the-parser
    (def line-parser
    (defn- process-inner
    (def multipliers
    (def op-lookup
    (def do-opr
    (defn- expand-vars
    (def xform-rules
    (defn post-process
    (defn parse-line
    (defn visi-parse [s]
    (defn serialize-to-file
    (defn deserialize-from-file

  find where they are called

    (rgrep "*found-rdds*" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "*name-lookup*" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "deal-with-identifier" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "parse-def" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "the-parser" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "line-parser" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "process-inner" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "multipliers" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "op-lookup" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "do-opr" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "expand-vars" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "xform-rules" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "post-process" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "parse-line" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "visi-parse [s]" "*.clj" "~/git/dpp/visi/src/" nil) 
    (rgrep "serialize-to-file" "*.clj" "~/git/dpp/visi/src/" nil)
    (rgrep "deserialize-from-file" "*.clj" "~/git/dpp/visi/src/" nil)

the only function called by others is
parse-line
called by ../src/visi/middleware.clj

../src/visi/runtime.clj

    (defprotocol FunctionFor
    (defprotocol TransformInfo
    (def spark-func-maker
    (extend JavaRDDLike
    (defn clean-it-up
    (defmacro clean-it-up-mac
    (defmacro v-aggregate [this zero-value seq-op comb-op]
    (defmacro v-aggregate-by-key [this zero-value seq-op comb-op]
    (defmacro v-combine-by-key [this create-combiner merge-value merge-combiners]
    (defmacro v-flat-map [this func]
    (defmacro v-flat-map-double [this func]
    (defmacro v-flat-map-pair [this func]
    (defmacro v-flat-map-values [this func]
    (defmacro v-fold-by-key [this zero-value func]
    (defmacro v-fold [this zero func]
    (defmacro v-foreach [this func]
    (defmacro v-group-by [this func]
    (defmacro v-key-by [this func]
    (defmacro v-map-values [this func]
    (defmacro v-map [this func]
    (defmacro v-map-to-double [this func]
    (defmacro v-map-to-pair [this func]
    (defmacro v-reduce [this func]
    (defmacro v-reduce-by-key [this func]
    (defmacro v-filter [this func]
    (defmacro v-sort-by [this func ascending]
    (def ^:private -app-params (atom {}))
    (defn app-params [] @-app-params)
    (defn set-app-params! [params]
    (defn to-iterable
    (defn- record-access
    (def ^:private session-info (ref {}))
    (defn add-msg-for-session
    (defn pull-session-info
    (defn- recompute
    (defprotocol Settable
    (defn build-watching-var
    (defn spark-context
    (defn get-data-from-url
    (defn build-rdd-from-url
    (defmacro source
    (defn scala-prod-to-vector
    (defn- function2
    (defn ensure-iterable
    (defn ensure-tuple2
    (defn s-drop
    (extend org.apache.spark.api.java.JavaRDDLike
    (defn read_url
    (defn starts-with [^String s ^String s2] (.startsWith s s2))
    (defn twitter-config-builder
    (defn twitter-factory
    (defn twitter-instance [] (.getInstance (twitter-factory)))
    (defn twitter-authorization [] (.getAuthorization (twitter-instance)))
    (defn streaming-context
    (defn create-twitter-stream
    (defn create-mqtt-stream
    (defn stream-into-watching
    (defn mark-watching-var
    (defmacro text-when
    (defn to-pair
    (defn to-iterable-pair
    (defn to-iterable-double


FunctionFor
TransformInfo
spark-func-maker
JavaRDDLike
clean-it-up
    (defmacro clean-it-up-mac
    (defmacro v-aggregate [this zero-value seq-op comb-op]
    (defmacro v-aggregate-by-key [this zero-value seq-op comb-op]
    (defmacro v-combine-by-key [this create-combiner merge-value merge-combiners]
    (defmacro v-flat-map [this func]
    (defmacro v-flat-map-double [this func]
    (defmacro v-flat-map-pair [this func]
    (defmacro v-flat-map-values [this func]
    (defmacro v-fold-by-key [this zero-value func]
    (defmacro v-fold [this zero func]
    (defmacro v-foreach [this func]
    (defmacro v-group-by [this func]
    (defmacro v-key-by [this func]
    (defmacro v-map-values [this func]
    (defmacro v-map [this func]
    (defmacro v-map-to-double [this func]
    (defmacro v-map-to-pair [this func]
    (defmacro v-reduce [this func]
    (defmacro v-reduce-by-key [this func]
    (defmacro v-filter [this func]
    (defmacro v-sort-by [this func ascending]
    (def ^:private -app-params (atom {}))
    (defn app-params [] @-app-params)
    (defn set-app-params! [params]
to-iterable
    (defn- record-access
    (def ^:private session-info (ref {}))
add-msg-for-session
pull-session-info
    (defn- recompute
    (defprotocol Settable
build-watching-var
spark-context
get-data-from-url
build-rdd-from-url
    (defmacro source
scala-prod-to-vector
    (defn- function2
ensure-iterable
    (defn ensure-tuple2
s-drop
    (extend org.apache.spark.api.java.JavaRDDLike
read_url
    (defn starts-with [^String s ^String s2] (.startsWith s s2))
twitter-config-builder
twitter-factory
    (defn twitter-instance [] (.getInstance (twitter-factory)))
    (defn twitter-authorization [] (.getAuthorization (twitter-instance)))
streaming-context
create-twitter-stream
create-mqtt-stream
stream-into-watching
mark-watching-var
    (defmacro text-when
to-pair
to-iterable-pair
to-iterable-double

../src/visi/t_stream.clj

    println "t_stream.clj called")
    ns visi.t-stream
    defn setup-system-props
    defmacro s-flat-map
    defn setup-thing
