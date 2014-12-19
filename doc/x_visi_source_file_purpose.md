2014-11-15

there are these files

    .
    ├── gorilla_plot
    │   ├── core.clj
    │   ├── util.clj
    │   └── vega.clj
    ├── gorilla_repl
    │   ├── core.clj ★
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
        ├── middleware.clj ★
        ├── parser.clj ★
        ├── repl_starter.clj ★
        ├── runtime.clj ★
        └── t_stream.clj

those marked with a star ★ are main ones

now, the purpose of the files

core.clj doesn't seem to do anything

repl_starter.clj all it does is load
visi.parser
gorilla-repl.core
and 
eval gorilla-repl.core/-main

now 
gorilla_repl/core.clj
does lots of things.

(ns gorilla-repl.core
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [org.httpkit.server :as server]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.params :as params]
            [ring.middleware.json :as json]
            [ring.util.response :as res]
            [visi.runtime :as v-run]
            [gorilla-repl.nrepl :as nrepl]
            [gorilla-repl.websocket-relay :as ws-relay]
            [gorilla-repl.renderer :as renderer] ;; this is needed to bring the render implementations into scope
            [gorilla-repl.files :as files]
            [gorilla-repl.version :as version]
            [complete.core :as complete]
            [clojure.set :as set]
            [clojure.java.io :as io])
  (:gen-class))

it starts a web serv

    ├── middleware.clj
    ├── parser.clj
    ├── 
    ├── runtime.clj
    └── t_stream.clj
