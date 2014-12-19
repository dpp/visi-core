2014-11-15

# what does gorilla-repl do technically?

basically, it starts a server.

the website is a single page app. (thus relies on JavaScript/html/css tech)

the page is a interactive notebook. User enter input, and output gets printed.

gorilla must take input, process it, produce output.

the web page is the frontend. JavaScript is used to take input events, send stuff to server, get server output, and display output events (text or graphics)

# gorilla-repl source files

there are these gorilla-repl files at visi/src/

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

## gorilla_plot is never called

the files in dir
gorilla_plot
is never called.

It is called when user loads it in the web interface
e.g.
(use 'gorilla-plot.core)

# files at visi/src/gorilla_repl

now let's see the files at visi/src/gorilla_repl

(the trailing number is line count)

    6.1k ../src/gorilla_repl/core.clj              178
    1.5k ../src/gorilla_repl/files.clj             48
     464 ../src/gorilla_repl/html.clj              17
    1.9k ../src/gorilla_repl/image.clj             42
     468 ../src/gorilla_repl/latex.clj             16
    1.3k ../src/gorilla_repl/nrepl.clj             25
    5.6k ../src/gorilla_repl/renderer.clj          206
    3.6k ../src/gorilla_repl/render_values_mw.clj  70
    1.1k ../src/gorilla_repl/table.clj             33
     461 ../src/gorilla_repl/vega.clj              15
     971 ../src/gorilla_repl/version.clj           26
    2.8k ../src/gorilla_repl/websocket_relay.clj   80

# gorilla-repl core functions

here's a list of things defined in core.clj

    (defn print-req
    (defn wrap-api-handler
    (defn load-worksheet
    (defn save
    (defn completions
    (def excludes (atom #{".git"}))
    (defn gorilla-files [req]
    (def conf (atom {}))
    (defn set-config [k v] (swap! conf assoc k v))
    (defn config [req] (res/response @conf))
    (defroutes app-routes
    (defn fix-prefix
    (defn fix-slash
    (defn rw-app-routes
    (defn- mk-number
    (defn run-gorilla-server
    (defn -main
