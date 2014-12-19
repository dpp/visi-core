2014-11-15

# notes on gorilla renderer

see
http://gorilla-repl.org/renderer.html
first

here's some main points.

• gorilla-repl is like clojure repl, it takes input, and produce output. This is done thru a network.

when user presses shift+enter, the input is sent to clojure then, it goes thru a rendering process, which produces the output.

there are 3 steps in this.

1. get user input. (from the web. when user presses shift+enter)
2. transform this clojure expression into something. (not as plain string, as plain terminal repl does)
3. send this transformed output to browser as JSON. The browser renders it (turn it into DOM fragment).

the most important step is 2. The main rendering step. This rendering process is done in repl “middleware”.

# The Render Protocol

Gorilla carries out this process rather simply by calling a single function `gorilla-renderable.core/render`, on the value.

This is the sole function in the Renderable protocol, found in the `gorilla-renderable.core` namespace.

This namespace lives in its own tiny project, with no dependencies, so supporting Gorilla rendering is a very light addition to your project.

the project is this https://github.com/JonyEpsilon/gorilla-renderable

and the entire project is 1 single file of 2 lines of content, this:

    (ns gorilla-renderable.core)
    ;;; This is the protocol that a type must implement if it wants to customise its rendering in Gorilla. It defines a
    ;;; single function, render, that should transform the value into a value that the front-end's renderer can display.
    (defprotocol Renderable (render [self]))

## the render function

The render function `gorilla-renderable.core/render` takes the value and returns its rendered representation. We'll specify the form of this rendered representation below, but it might be something like:

{:type :html :content "<span class='clj-long'>3</span>" :value "3"}

The `:value` is a readable presentation, similar to clojure's 「pr」 in clojure repl.

for aggregate values such as Clojure list [1 2 3], it's like this:

{:type :list-like,
 :open "<span class='clj-vector'>[<span>",
 :close "<span class='clj-vector'>]</span>",
 :separator " ",
 :items
 ({:type :html, :content "<span class='clj-long'>1</span>", :value "1"}
  {:type :html, :content "<span class='clj-long'>2</span>", :value "2"}
  {:type :html, :content "<span class='clj-long'>3</span>", :value "3"}),
 :value "[1 2 3]"}

# Extending the renderer

see
http://gorilla-repl.org/renderer.html
