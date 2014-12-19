2014-11-18

# what's the difference/modification visi made to gorilla-repl?

diff -r -i -E -b -w -B --brief gorilla_repl_original gorilla_repl

    -i, --ignore-case
    -E, --ignore-tab-expansion
    -b, --ignore-space-change
    -w, --ignore-all-space
    -B, --ignore-blank-lines
    --strip-trailing-cr

the files that differ are:

* gorilla_repl/core.clj differ
* gorilla_repl/nrepl.clj differ
* gorilla_repl/render_values_mw.clj differ
* gorilla_repl/websocket_relay.clj differ

the diff commands

    diff -i -E -b -w -B gorilla_repl_original/core.clj gorilla_repl/core.clj
    diff -i -E -b -w -B gorilla_repl_original/nrepl.clj gorilla_repl/nrepl.clj
    diff -i -E -b -w -B gorilla_repl_original/render_values_mw.clj gorilla_repl/render_values_mw.clj
    diff -i -E -b -w -B gorilla_repl_original/websocket_relay.clj gorilla_repl/websocket_relay.clj

here's their differences, in these files

    x_diff_gorilla/diff_gorilla_core.diff
    x_diff_gorilla/diff_gorilla_render_values_mw.diff
    x_diff_gorilla/diff_gorilla_repl.diff
    x_diff_gorilla/diff_gorilla_websocket_relay.diff
