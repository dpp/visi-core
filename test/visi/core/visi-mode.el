;;; visi-mode.el --- Major mode for editing visi code. -*- coding: utf-8 -*-

;;; REQUIREMENT
;; You need to have the following installed
;; • Leiningen http://leiningen.org/ , version 2.5.0 or later
;; • cider https://github.com/clojure-emacs/cider , version 0.8.2 or later
;; • visi engine. 2015-01-27 or later
;; • GNU Emacs 24.3 or later

;;; SETUP
;; 1. Create a dir at ~/.emacs.d/lisp/ , place visi-mode.el in that directory.
;; 2. Add the following to your emacs init.
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'auto-mode-alist '("\\.visi\\'" . visi-mode))
;; 3. Restart emacs.

;;; TO EVAL VISI CODE
;; Call `cider-jack-in' or `cider-connect' to start Visi/clojure REPL.
;; After Cider is ready (you'll see a buffer popping up with prompt), call `visi-load-visi-lib'. Wait a few seconds for visi lib to load
;; Call `visi-eval-line-or-region' to eval.

;; TODO
;; • 2015-02-07 eliminate visi-load-visi-lib step. can be done by (nrepl-sync-request:eval …)
;; • 2015-01-27 possibly need to escape backslash. try regex ⁖  「re$-matches( #/a.+/, "abc")」 with lots slash or backslash and also contain double quotes. Solution, base64 encode it. See (visi.core.runtime/as-string (visi.core.parser/parse-and-eval-multiline (visi.core.util/decode-base64 \"%s\"))) in ob-visi.el
;; • make/enhance the syntax coloring syntax based (instead of just inert keywords)
;; • improve doc on how to use/setup
;; • add/improve inline doc to all functions.
;; • consider using https://github.com/sanel/monroe as nrepl backend instead of CIDER
;; • 2015-02-10 refine regex based syntax coloring for {package, namespace, java/clojure interopr} related syntax. ⁖  name::name, $.methodName, etc.
;; • 2015-02-10 find out if {name -> some} is still supported, and in general, Visi's syntax for associative array. Also, keywords, eg 「:name」, 「name:」

(require 'cider)
(require 'newcomment)

(defvar visi-mode-hook nil "Standard hook for `visi-mode'")

(add-to-list 'auto-mode-alist '("\\.visi\\'" . visi-mode))


(defvar visi-visi-operators nil "list of Visi operators.")
(setq visi-visi-operators
      '(
"="
"+"
"-"
"*"
"/"
"<"
">"
"<="
">="
"!="
"<>"
"=="
"!="
"&&"
""
"||"
"&"
"%%"
"->"
">>"
"|>"
 )
      )

(defvar visi-clojure-operator-like-things nil "list of clojure.core symbols that are operator-like in appearance.")
(setq visi-clojure-operator-like-things
      '( "&" "*" "*'" "+" "+'" "-" "-'" "->" "->>" "." ".." "/" "<" "<=" "=" "==" ">" ">=" )
      )

(defvar visi-clojure.core-words nil "list of clojure.core words.")
(setq visi-clojure.core-words
      ;; note: this is not exactly a complete list. Some, such as + - < > are removed.
      '(
        "*1"
        "*2"
        "*3"
        "*agent*"
        "*allow-unresolved-vars*"
        "*assert*"
        "*clojure-version*"
        "*command-line-args*"
        "*compile-files*"
        "*compile-path*"
        "*compiler-options*"
        "*data-readers*"
        "*default-data-reader-fn*"
        "*e"
        "*err*"
        "*file*"
        "*flush-on-newline*"
        "*fn-loader*"
        "*in*"
        "*math-context*"
        "*ns*"
        "*out*"
        "*print-dup*"
        "*print-length*"
        "*print-level*"
        "*print-meta*"
        "*print-readably*"
        "*read-eval*"
        "*source-path*"
        "*unchecked-math*"
        "*use-context-classloader*"
        "*verbose-defrecords*"
        "*warn-on-reflection*"
        "->ArrayChunk"
        "->Vec"
        "->VecNode"
        "->VecSeq"
        "-cache-protocol-fn"
        "-reset-methods"
        "ArrayChunk"
        "EMPTY-NODE"
        "Vec"
        "VecNode"
        "VecSeq"
        "accessor"
        "aclone"
        "add-classpath"
        "add-watch"
        "agent"
        "agent-error"
        "agent-errors"
        "aget"
        "alength"
        "alias"
        "all-ns"
        "alter"
        "alter-meta!"
        "alter-var-root"
        "amap"
        "ancestors"
        "and"
        "apply"
        "areduce"
        "array-map"
        "as->"
        "aset"
        "aset-boolean"
        "aset-byte"
        "aset-char"
        "aset-double"
        "aset-float"
        "aset-int"
        "aset-long"
        "aset-short"
        "assert"
        "assoc!"
        "assoc"
        "assoc-in"
        "associative?"
        "atom"
        "await"
        "await-for"
        "await1"
        "bases"
        "bean"
        "bigdec"
        "bigint"
        "biginteger"
        "binding"
        "bit-and"
        "bit-and-not"
        "bit-clear"
        "bit-flip"
        "bit-not"
        "bit-or"
        "bit-set"
        "bit-shift-left"
        "bit-shift-right"
        "bit-test"
        "bit-xor"
        "boolean"
        "boolean-array"
        "booleans"
        "bound-fn"
        "bound-fn*"
        "bound?"
        "butlast"
        "byte"
        "byte-array"
        "bytes"
        "case"
        "cast"
        "catch"
        "char"
        "char-array"
        "char-escape-string"
        "char-name-string"
        "char?"
        "chars"
        "chunk"
        "chunk-append"
        "chunk-buffer"
        "chunk-cons"
        "chunk-first"
        "chunk-next"
        "chunk-rest"
        "chunked-seq?"
        "class"
        "class?"
        "clear-agent-errors"
        "clojure-version"
        "coll?"
        "comment"
        "commute"
        "comp"
        "comparator"
        "compare"
        "compare-and-set!"
        "compile"
        "complement"
        "concat"
        "cond"
        "cond->"
        "cond->>"
        "condp"
        "conj!"
        "conj"
        "cons"
        "constantly"
        "construct-proxy"
        "contains?"
        "count"
        "counted?"
        "create-ns"
        "create-struct"
        "cycle"
        "dec"
        "dec'"
        "decimal?"
        "declare"
        "def"
        "default-data-readers"
        "definline"
        "definterface"
        "defmacro"
        "defmethod"
        "defmulti"
        "defn"
        "defn-"
        "defonce"
        "defprotocol"
        "defrecord"
        "defstruct"
        "deftype"
        "delay"
        "delay?"
        "deliver"
        "denominator"
        "deref"
        "derive"
        "descendants"
        "destructure"
        "disj!"
        "disj"
        "dissoc!"
        "dissoc"
        "distinct"
        "distinct?"
        "do"
        "doall"
        "dorun"
        "doseq"
        "dosync"
        "dotimes"
        "doto"
        "double"
        "double-array"
        "doubles"
        "drop"
        "drop-last"
        "drop-while"
        "empty"
        "empty?"
        "ensure"
        "enumeration-seq"
        "error-handler"
        "error-mode"
        "eval"
        "even?"
        "every-pred"
        "every?"
        "ex-data"
        "ex-info"
        "extend"
        "extend-protocol"
        "extend-type"
        "extenders"
        "extends?"
        "false?"
        "ffirst"
        "file-seq"
        "filter"
        "filterv"
        "finally"
        "find"
        "find-keyword"
        "find-ns"
        "find-protocol-impl"
        "find-protocol-method"
        "find-var"
        "first"
        "flatten"
        "float"
        "float-array"
        "float?"
        "floats"
        "flush"
        "fn"
        "fn?"
        "fnext"
        "fnil"
        "for"
        "force"
        "format"
        "frequencies"
        "future"
        "future-call"
        "future-cancel"
        "future-cancelled?"
        "future-done?"
        "future?"
        "gen-class"
        "gen-interface"
        "gensym"
        "get"
        "get-in"
        "get-method"
        "get-proxy-class"
        "get-thread-bindings"
        "get-validator"
        "group-by"
        "hash"
        "hash-combine"
        "hash-map"
        "hash-ordered-coll"
        "hash-set"
        "hash-unordered-coll"
        "identical?"
        "identity"
        "if"
        "if-let"
        "if-not"
        "if-some"
        "ifn?"
        "import"
        "in-ns"
        "inc"
        "inc'"
        "init-proxy"
        "instance?"
        "int"
        "int-array"
        "integer?"
        "interleave"
        "intern"
        "interpose"
        "into"
        "into-array"
        "ints"
        "io!"
        "isa?"
        "iterate"
        "iterator-seq"
        "juxt"
        "keep"
        "keep-indexed"
        "key"
        "keys"
        "keyword"
        "keyword?"
        "last"
        "lazy-cat"
        "lazy-seq"
        "let"
        "letfn"
        "line-seq"
        "list"
        "list*"
        "list?"
        "load"
        "load-file"
        "load-reader"
        "load-string"
        "loaded-libs"
        "locking"
        "long"
        "long-array"
        "longs"
        "loop"
        "macroexpand"
        "macroexpand-1"
        "make-array"
        "make-hierarchy"
        "map"
        "map-indexed"
        "map?"
        "mapcat"
        "mapv"
        "max"
        "max-key"
        "memfn"
        "memoize"
        "merge"
        "merge-with"
        "meta"
        "method-sig"
        "methods"
        "min"
        "min-key"
        "mix-collection-hash"
        "mod"
        "monitor-enter"
        "monitor-exit"
        "munge"
        "name"
        "namespace"
        "namespace-munge"
        "neg?"
        "new"
        "newline"
        "next"
        "nfirst"
        "nil?"
        "nnext"
        "not"
        "not-any?"
        "not-empty"
        "not-every?"
        "not="
        "ns"
        "ns-aliases"
        "ns-imports"
        "ns-interns"
        "ns-map"
        "ns-name"
        "ns-publics"
        "ns-refers"
        "ns-resolve"
        "ns-unalias"
        "ns-unmap"
        "nth"
        "nthnext"
        "nthrest"
        "num"
        "number?"
        "numerator"
        "object-array"
        "odd?"
        "or"
        "parents"
        "partial"
        "partition"
        "partition-all"
        "partition-by"
        "pcalls"
        "peek"
        "persistent!"
        "pmap"
        "pop!"
        "pop"
        "pop-thread-bindings"
        "pos?"
        "pr"
        "pr-str"
        "prefer-method"
        "prefers"
        "primitives-classnames"
        "print"
        "print-ctor"
        "print-dup"
        "print-method"
        "print-simple"
        "print-str"
        "printf"
        "println"
        "println-str"
        "prn"
        "prn-str"
        "promise"
        "proxy"
        "proxy-call-with-super"
        "proxy-mappings"
        "proxy-name"
        "proxy-super"
        "push-thread-bindings"
        "pvalues"
        "quot"
        "quote"
        "rand"
        "rand-int"
        "rand-nth"
        "range"
        "ratio?"
        "rational?"
        "rationalize"
        "re-find"
        "re-groups"
        "re-matcher"
        "re-matches"
        "re-pattern"
        "re-seq"
        "read"
        "read-line"
        "read-string"
        "realized?"
        "record?"
        "recur"
        "reduce"
        "reduce-kv"
        "reduced"
        "reduced?"
        "reductions"
        "ref"
        "ref-history-count"
        "ref-max-history"
        "ref-min-history"
        "ref-set"
        "refer"
        "refer-clojure"
        "reify"
        "release-pending-sends"
        "rem"
        "remove"
        "remove-all-methods"
        "remove-method"
        "remove-ns"
        "remove-watch"
        "repeat"
        "repeatedly"
        "replace"
        "replicate"
        "require"
        "reset!"
        "reset-meta!"
        "resolve"
        "rest"
        "restart-agent"
        "resultset-seq"
        "reverse"
        "reversible?"
        "rseq"
        "rsubseq"
        "satisfies?"
        "second"
        "select-keys"
        "send"
        "send-off"
        "send-via"
        "seq"
        "seq?"
        "seque"
        "sequence"
        "sequential?"
        "set!"
        "set"
        "set-agent-send-executor!"
        "set-agent-send-off-executor!"
        "set-error-handler!"
        "set-error-mode!"
        "set-validator!"
        "set?"
        "short"
        "short-array"
        "shorts"
        "shuffle"
        "shutdown-agents"
        "slurp"
        "some"
        "some->"
        "some->>"
        "some-fn"
        "some?"
        "sort"
        "sort-by"
        "sorted-map"
        "sorted-map-by"
        "sorted-set"
        "sorted-set-by"
        "sorted?"
        "special-symbol?"
        "spit"
        "split-at"
        "split-with"
        "str"
        "string?"
        "struct"
        "struct-map"
        "subs"
        "subseq"
        "subvec"
        "supers"
        "swap!"
        "symbol"
        "symbol?"
        "sync"
        "take"
        "take-last"
        "take-nth"
        "take-while"
        "test"
        "the-ns"
        "thread-bound?"
        "throw"
        "time"
        "to-array"
        "to-array-2d"
        "trampoline"
        "transient"
        "tree-seq"
        "true?"
        "try"
        "type"
        "unchecked-add"
        "unchecked-add-int"
        "unchecked-byte"
        "unchecked-char"
        "unchecked-dec"
        "unchecked-dec-int"
        "unchecked-divide-int"
        "unchecked-double"
        "unchecked-float"
        "unchecked-inc"
        "unchecked-inc-int"
        "unchecked-int"
        "unchecked-long"
        "unchecked-multiply"
        "unchecked-multiply-int"
        "unchecked-negate"
        "unchecked-negate-int"
        "unchecked-remainder-int"
        "unchecked-short"
        "unchecked-subtract"
        "unchecked-subtract-int"
        "underive"
        "unquote"
        "unquote-splicing"
        "unsigned-bit-shift-right"
        "update-in"
        "update-proxy"
        "use"
        "val"
        "vals"
        "var"
        "var-get"
        "var-set"
        "var?"
        "vary-meta"
        "vec"
        "vector"
        "vector-of"
        "vector?"
        "when"
        "when-first"
        "when-let"
        "when-not"
        "when-some"
        "while"
        "with-bindings"
        "with-bindings*"
        "with-in-str"
        "with-loading-context"
        "with-local-vars"
        "with-meta"
        "with-open"
        "with-out-str"
        "with-precision"
        "with-redefs"
        "with-redefs-fn"
        "xml-seq"
        "zero?"
        "zipmap"
        ))

(defvar visi-visi-words nil "list of visi specific words.")
(setq visi-visi-words
      '(
        "for"
        "if"
        "then"
        "else"
        "begin"
        "end"
        "true"
        "false"
        "source"
        "sink"
        ))

(defvar visi-java-words nil "list of Java specific words.")
(setq visi-java-words
      '(
        "Math/cos" ; todo
        "Math/log10"
        "Math/PI"
        ))

(defvar visi-all-keywords nil "list of all keywords")
(setq visi-all-keywords
      (append visi-clojure.core-words visi-visi-words visi-java-words ))


;; completion


;; indent/reformat related


;; abbrev

(setq visi-abbrev-table nil)

;; TODO add more Visi abbrevs
(define-abbrev-table 'visi-abbrev-table
  '(
    ("if" "if(TEST▮, TrueBody, ElseBody)" nil :system t )
    ("begin" "begin\n ▮\nend" nil :system t ))

  "abbrev table for `visi-mode'"
  ;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  )


;; eval related

(defun visi-repl-connect ()
  "Start a connection to Visi.
This command currently uses emacs CIDER lib to connect to Clojure nREPL."
  (interactive)
  (let ()
    (cider-jack-in)
    ;; TODO problem now is that nrepl is async. Need to wait for connection to finish before loading visi lib. e.g. write callback, or probably look at process sentinel, see `nrepl-start-server-process', or (nrepl-sync-request:eval …)
    ;; (visi-load-visi-lib)
    ;; (add-hook 'nrepl-connected-hook 'visi-load-visi-lib) ;; don't
    ))

(defun visi-load-visi-lib ()
  "Send nREPL code to load Visi lib."
  (interactive)
  (let ()
    (cider-interactive-eval "(ns emacsvisi-test (:require [clojure.test] [visi.core.parser] [visi.core.runtime] [visi.core.util]))")
    (message "`visi-load-visi-lib' called, wait for 4 seconds.")
    ))

;; possibly write a nrepl handler.
;; possibly write a nrepl handler.
;; a function that takes one arg.
;; the arg is response from reple server process.
;; the response is “alist”, and contains at least “id” and “session” keys.
;; Other standard response keys are “value”, “out”, “err”, “status”.

;; The presence of a particular key determines the type of the response.

;; For example, if “value” key is present, the response is of type “value”, if “out” key is present the response is “stdout” etc.

;; Depending on the type, the handler dispatches the appropriate value to one of the supplied
;; handlers: VALUE-HANDLER, STDOUT-HANDLER, STDERR-HANDLER, DONE-HANDLER, and
;; EVAL-ERROR-HANDLER.  If the optional EVAL-ERROR-HANDLER is nil, the default
;; `nrepl-err-handler' is used.  If any of the other supplied handlers are nil
;; nothing happens for the coresponding type of response.

;; When `nrepl-log-messages' is non-nil, *nrepl-messages* buffer contains
;; server responses."

(defun visi-eval-line-or-region (pos1 pos2)
  "Evaluate the current line, or text selection.
If `universal-argument' is called first, insert result at cursor position.

You must call `visi-repl-connect' first, when ready, call `visi-load-visi-lib'.
To eval Clojure code, call `cider-eval-last-sexp', `cider-eval-region' etc."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (if (cider-connected-p)
      (let* (
             (input-text (buffer-substring-no-properties pos1 pos2))
             (quoted-input-text (replace-regexp-in-string "\"" "\\\"" input-text "FIXEDCASE" "LITERAL")))

        ;; (message "input-text is: 「%s」" input-text)
        ;; (message "quoted-input-text is: 「%s」" quoted-input-text)
        (cider-interactive-eval
         (format "(visi.core.parser/parse-and-eval-for-tests \"%s\")"
                 quoted-input-text )
         (when current-prefix-arg (cider-eval-print-handler))))
    (user-error "No active nREPL connection. Call `visi-repl-connect' first.")))


;; keybinding

(defvar visi-keymap nil "Keybinding for `visi-mode'")
(progn
  (setq visi-keymap (make-sparse-keymap))
  (define-key visi-keymap (kbd "C-c C-l") 'visi-load-visi-lib)
  (define-key visi-keymap (kbd "C-x C-e") 'visi-eval-line-or-region)
  (define-key visi-keymap (kbd "C-c M-c") 'cider-connect)
  (define-key visi-keymap (kbd "C-c M-j") 'cider-jack-in))


;; syntax coloring related

(setq visi-font-lock-keywords
      (let (
            (clojureCoreWords (regexp-opt visi-clojure.core-words 'symbols))
            (visiOperators (regexp-opt visi-visi-operators 'symbols))
            (visiWords (regexp-opt visi-visi-words 'symbols)))
        `(
          ("#\\(/\\{1,10\\}\\)\\(.+?\\)\\1" . font-lock-string-face) ; regex
          ("#\\('\\{2,10\\}\\)\\(.+?\\)\\1" . (0 font-lock-string-face t)) ; string literal
          ("#\\(\"\\{2,10\\}\\)\\(.+?\\)\\1" . (0 font-lock-string-face t)) ; string literal
          ("#days\\|#seconds\\|#minutes\\|#hours" . font-lock-builtin-face)
          ("[A-Za-z]+::[A-Za-z]+" . font-lock-function-name-face)
          (":[A-Za-z]+" . font-lock-constant-face)
          ("[A-Za-z]+:" . font-lock-constant-face)
          ("\\([_A-Za-z]+\\)(" . (1 font-lock-function-name-face))
          ("\\([_A-Za-z]+\\) *=[^=>]" . (1 font-lock-variable-name-face))
          (,visiOperators . font-lock-type-face)
          (,visiWords . font-lock-keyword-face)
          (,clojureCoreWords . font-lock-function-name-face)
          ;; ("##" . font-lock-comment-delimiter-face)
          )))

;; font-lock-builtin-face
;; font-lock-comment-delimiter-face
;; font-lock-comment-face
;; font-lock-constant-face
;; font-lock-doc-face
;; font-lock-function-name-face
;; font-lock-keyword-face
;; font-lock-negation-char-face
;; font-lock-preprocessor-face
;; font-lock-reference-face
;; font-lock-string-face
;; font-lock-type-face
;; font-lock-variable-name-face
;; font-lock-warning-face



;; syntax table
(defvar visi-syntax-table nil "Syntax table for `visi-mode'.")
(setq visi-syntax-table
      (let ((synTable (make-syntax-table)))

        (modify-syntax-entry ?\/ ". 14" synTable)
        (modify-syntax-entry ?* ". 23b" synTable)

        synTable))


;; define the mode
(defun visi-mode ()
  "A major mode for Visi.
See URL `https://github.com/visicorp/visi-core'

\\{visi-keymap}"
  ;; TODO possibly consider making it derived after prog-mode (and deal with whatever that problem)
  (interactive)

  (kill-all-local-variables)
  (setq mode-name "visi")
  (setq major-mode 'visi-mode)
  (set-syntax-table visi-syntax-table)
  (setq font-lock-defaults
        '((visi-font-lock-keywords)
          nil
          nil
          (
            ;; (modify-syntax-entry ?# "<" synTable)
            ;; (modify-syntax-entry ?\n ">" synTable)
            ("#" . ". 12")
            ("\n" . ">"))
          ))

;; syntax-begin-function

  (use-local-map visi-keymap)
  (setq local-abbrev-table visi-abbrev-table)

  (setq-local comment-start "## ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "##+ +")
  (setq-local comment-add 1)
  (setq-local comment-column 2)

  (abbrev-mode 1)

  (run-mode-hooks 'visi-mode-hook))

(provide 'visi-mode)
