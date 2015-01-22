;;; visi-mode.el --- Major mode for editing visi code. -*- coding: utf-8 -*-

;;; REQUIREMENT
;; You need to have the following installed
;; • Leiningen http://leiningen.org/
;; • cider https://github.com/clojure-emacs/cider
;; • visi engine.

;;; SETUP
;; 1. Create a dir at ~/.emacs.d/lisp/
;; Place visi-mode.el in your load path.
;; 2. Add the following to your emacs init.
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (add-to-list 'auto-mode-alist '("\\.visi\\'" . visi-mode))
;; 3. Restart emacs.

;;; TO EVAL VISI CODE
;; Call `visi-init-nrepl-connection' to start Cider.
;; After Cider is ready, call `visi-load-visi-lib'. Wait a few seconds for visi lib to load
;; Call `visi-eval-line-or-region'

;; 2015-01-20 things to do

;; • eliminate visi-load-visi-lib step
;; • make sure there's a doc on how to use/setup
;; • add inline doc
;; • make org mode able to contain visi code and also eval it.
;; • be sure visi code org mode can be exported to html, with syntax highlighting

(require 'cider)

(defvar visi-mode-hook nil "Standard hook for `visi-mode'")



;; syntax table
(defvar visi-syntax-table nil "Syntax table for `visi-mode'.")
(setq visi-syntax-table
      (let ((synTable (make-syntax-table )))

        ;; (modify-syntax-entry ?~ "'   " synTable)
        ;; (modify-syntax-entry ?\{ "(}" synTable)
        ;; (modify-syntax-entry ?\} "){" synTable)
        ;; (modify-syntax-entry ?\[ "(]" synTable)
        ;; (modify-syntax-entry ?\] ")[" synTable)
        ;; (modify-syntax-entry ?^ "'" synTable)

        (modify-syntax-entry ?\/ ". 14" synTable)
        (modify-syntax-entry ?* ". 23" synTable)

        ;; (modify-syntax-entry ?# "< b" synTable)
        ;; (modify-syntax-entry ?\n "> b" synTable)

        synTable))



(defvar visi-clojure.core-words nil "list of clojure.core words.")
(setq visi-clojure.core-words
      '(
"&" "*" "*'" "*1" "*2" "*3" "*agent*" "*allow-unresolved-vars*" "*assert*" "*clojure-version*" "*command-line-args*" "*compile-files*" "*compile-path*" "*compiler-options*" "*data-readers*" "*default-data-reader-fn*" "*e" "*err*" "*file*" "*flush-on-newline*" "*fn-loader*" "*in*" "*math-context*" "*ns*" "*out*" "*print-dup*" "*print-length*" "*print-level*" "*print-meta*" "*print-readably*" "*read-eval*" "*source-path*" "*unchecked-math*" "*use-context-classloader*" "*verbose-defrecords*" "*warn-on-reflection*" "+" "+'" "-" "-'" "->" "->>" "->ArrayChunk" "->Vec" "->VecNode" "->VecSeq" "-cache-protocol-fn" "-reset-methods" "." ".." "/" "<" "<=" "=" "==" ">" ">=" "ArrayChunk" "EMPTY-NODE" "Vec" "VecNode" "VecSeq" "accessor" "aclone" "add-classpath" "add-watch" "agent" "agent-error" "agent-errors"
"aget" "alength" "alias" "all-ns" "alter" "alter-meta!" "alter-var-root" "amap" "ancestors" "and" "apply" "areduce" "array-map" "as->" "aset" "aset-boolean" "aset-byte" "aset-char" "aset-double" "aset-float" "aset-int" "aset-long" "aset-short" "assert" "assoc!" "assoc" "assoc-in" "associative?" "atom" "await" "await-for" "await1" "bases" "bean" "bigdec" "bigint" "biginteger" "binding" "bit-and" "bit-and-not" "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set" "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean" "boolean-array" "booleans" "bound-fn" "bound-fn*" "bound?" "butlast" "byte" "byte-array" "bytes" "case" "cast" "catch" "char" "char-array" "char-escape-string" "char-name-string"
"char?" "chars" "chunk" "chunk-append" "chunk-buffer" "chunk-cons" "chunk-first" "chunk-next" "chunk-rest" "chunked-seq?" "class" "class?" "clear-agent-errors" "clojure-version" "coll?" "comment" "commute" "comp" "comparator" "compare" "compare-and-set!" "compile" "complement" "concat" "cond" "cond->" "cond->>" "condp" "conj!" "conj" "cons" "constantly" "construct-proxy" "contains?" "count" "counted?" "create-ns" "create-struct" "cycle" "dec" "dec'" "decimal?" "declare" "def" "default-data-readers" "definline" "definterface" "defmacro" "defmethod" "defmulti" "defn" "defn-" "defonce" "defprotocol" "defrecord" "defstruct" "deftype" "delay" "delay?" "deliver" "denominator" "deref" "derive" "descendants" "destructure" "disj!" "disj" "dissoc!" "dissoc" "distinct" "distinct?" "do" "doall" "dorun" "doseq"
"dosync" "dotimes" "doto" "double" "double-array" "doubles" "drop" "drop-last" "drop-while" "empty" "empty?" "ensure" "enumeration-seq" "error-handler" "error-mode" "eval" "even?" "every-pred" "every?" "ex-data" "ex-info" "extend" "extend-protocol" "extend-type" "extenders" "extends?" "false?" "ffirst" "file-seq" "filter" "filterv" "finally" "find" "find-keyword" "find-ns" "find-protocol-impl" "find-protocol-method" "find-var" "first" "flatten" "float" "float-array" "float?" "floats" "flush" "fn" "fn?" "fnext" "fnil" "for" "force" "format" "frequencies" "future" "future-call" "future-cancel" "future-cancelled?" "future-done?" "future?" "gen-class" "gen-interface" "gensym" "get" "get-in" "get-method" "get-proxy-class" "get-thread-bindings" "get-validator" "group-by" "hash" "hash-combine" "hash-map" "hash-ordered-coll" "hash-set" "hash-unordered-coll" "identical?" "identity" "if" "if-let" "if-not" "if-some" "ifn?" "import" "in-ns" "inc" "inc'" "init-proxy" "instance?" "int" "int-array" "integer?" "interleave" "intern" "interpose" "into" "into-array" "ints"
"io!" "isa?" "iterate" "iterator-seq" "juxt" "keep" "keep-indexed" "key" "keys" "keyword" "keyword?" "last" "lazy-cat" "lazy-seq" "let" "letfn" "line-seq" "list" "list*" "list?" "load" "load-file" "load-reader" "load-string" "loaded-libs" "locking" "long" "long-array" "longs" "loop" "macroexpand" "macroexpand-1" "make-array" "make-hierarchy" "map" "map-indexed" "map?" "mapcat" "mapv" "max" "max-key" "memfn" "memoize" "merge" "merge-with" "meta" "method-sig" "methods" "min" "min-key" "mix-collection-hash" "mod" "monitor-enter" "monitor-exit" "munge" "name" "namespace" "namespace-munge" "neg?" "new" "newline" "next" "nfirst" "nil?" "nnext" "not" "not-any?" "not-empty" "not-every?" "not=" "ns" "ns-aliases" "ns-imports" "ns-interns" "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-resolve" "ns-unalias"
"ns-unmap" "nth" "nthnext" "nthrest" "num" "number?" "numerator" "object-array" "odd?" "or" "parents" "partial" "partition" "partition-all" "partition-by" "pcalls" "peek" "persistent!" "pmap" "pop!" "pop" "pop-thread-bindings" "pos?" "pr" "pr-str" "prefer-method" "prefers" "primitives-classnames" "print" "print-ctor" "print-dup" "print-method" "print-simple" "print-str" "printf" "println" "println-str" "prn" "prn-str" "promise" "proxy" "proxy-call-with-super" "proxy-mappings" "proxy-name" "proxy-super" "push-thread-bindings" "pvalues" "quot" "quote" "rand" "rand-int" "rand-nth" "range" "ratio?" "rational?" "rationalize" "re-find" "re-groups" "re-matcher" "re-matches" "re-pattern" "re-seq" "read"
"read-line" "read-string" "realized?" "record?" "recur" "reduce" "reduce-kv" "reduced" "reduced?" "reductions" "ref" "ref-history-count" "ref-max-history" "ref-min-history" "ref-set" "refer" "refer-clojure" "reify" "release-pending-sends" "rem" "remove" "remove-all-methods" "remove-method" "remove-ns" "remove-watch" "repeat" "repeatedly" "replace" "replicate" "require" "reset!" "reset-meta!" "resolve" "rest" "restart-agent" "resultset-seq" "reverse" "reversible?" "rseq" "rsubseq" "satisfies?" "second" "select-keys" "send" "send-off" "send-via" "seq" "seq?" "seque" "sequence" "sequential?" "set!" "set" "set-agent-send-executor!" "set-agent-send-off-executor!" "set-error-handler!" "set-error-mode!" "set-validator!" "set?" "short" "short-array" "shorts" "shuffle" "shutdown-agents" "slurp" "some" "some->" "some->>" "some-fn" "some?" "sort" "sort-by" "sorted-map" "sorted-map-by" "sorted-set" "sorted-set-by" "sorted?" "special-symbol?" "spit" "split-at" "split-with" "str" "string?" "struct" "struct-map" "subs" "subseq" "subvec" "supers"
"swap!" "symbol" "symbol?" "sync" "take" "take-last" "take-nth" "take-while" "test" "the-ns" "thread-bound?" "throw" "time" "to-array" "to-array-2d" "trampoline" "transient" "tree-seq" "true?" "try" "type" "unchecked-add" "unchecked-add-int" "unchecked-byte" "unchecked-char" "unchecked-dec" "unchecked-dec-int" "unchecked-divide-int" "unchecked-double" "unchecked-float" "unchecked-inc" "unchecked-inc-int" "unchecked-int" "unchecked-long" "unchecked-multiply" "unchecked-multiply-int" "unchecked-negate" "unchecked-negate-int" "unchecked-remainder-int" "unchecked-short" "unchecked-subtract" "unchecked-subtract-int" "underive" "unquote" "unquote-splicing" "unsigned-bit-shift-right" "update-in" "update-proxy" "use" "val" "vals" "var" "var-get" "var-set" "var?" "vary-meta" "vec" "vector" "vector-of" "vector?" "when" "when-first" "when-let" "when-not" "when-some" "while" "with-bindings" "with-bindings*" "with-in-str" "with-loading-context" "with-local-vars" "with-meta" "with-open" "with-out-str" "with-precision" "with-redefs" "with-redefs-fn" "xml-seq" "zero?" "zipmap"
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
        "#seconds" ; todo
        "#minutes"
        "#hours"
        "#days"
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

(define-abbrev-table 'visi-abbrev-table
  '(
    ("if" "if(TEST▮, TrueBody, ElseBody)" nil :system t )
)

  "abbrev table for `visi-mode'"
;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  )


;; eval related

(defun visi-display-ready-message ()
  ""
  (interactive)
  (message "Visi ready."))

(defun visi-visi-lib-loaded-p ()
  ""
  (interactive)
  (let ()
    (if (cider-connected-p)
        (progn
          (if 3
              ;; check if visi lib is loaded
              ;; (not= (resolve 'clojure.core/list) nil)
              ;; (not= (resolve 'visi.core.parser/line-parser) nil)
              nil
            nil))
      nil
      )))


(defun visi-init-nrepl-connection ()
  ""
  (interactive)
  (let ()
    (cider-jack-in)
    ;; todo problem now is that nrepl is async. Need to wait for connection to finish before loading visi lib. e.g. write callback, or probably look at process sentinel, see `nrepl-start-server-process'
    ;; (visi-load-visi-lib)
    ;; (add-hook 'nrepl-connected-hook 'visi-load-visi-lib) ;; don't
    ))

(defun visi-load-visi-lib ()
  "Send nrepl code to load Visi lib."
  (interactive)
  (let ()
    (cider-interactive-eval "(ns emacsvisi-test (:require [clojure.test :as t] [visi.core.parser :as vp] [visi.core.runtime :as vr] [visi.core.util :as vu] [instaparse.core :as insta] ))")
    (message "`visi-load-visi-lib' called.")
    ))

;; write a nrepl handler.
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


(defun visi-eval-line-or-region (p1 p2)
  "Evaluate the current line, or text selection.
You must call `visi-init-nrepl-connection' first.
To eval clojure code, call `cider-eval-last-sexp', `cider-eval-region' etc."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (if (cider-connected-p)
      (progn
        (let ()
          (cider-interactive-eval
           (format "(vp/parse-and-eval-for-tests \"%s\")"
                   (buffer-substring-no-properties p1 p2)) p1)))
    (progn (error "No active nREPL connection. Call `visi-init-nrepl-connection' first."))))

(defun visi-gen-random-namespace ()
  ""
  (interactive)
  (format "session-%c%d"
            (+ (random 25) 97)
            (random (+ 99))))


;; syntax coloring related

(setq visi-font-lock-keywords
      (let (
            (clojureCoreWords (regexp-opt visi-clojure.core-words 'symbols))
            (visiWords (regexp-opt visi-visi-words 'symbols))
            (keywordWords ":[A-Za-z]+" )
            )
        `(
          ("##.+" . font-lock-comment-face)
          (,clojureCoreWords . font-lock-function-name-face)
          (,visiWords . font-lock-keyword-face)
          (,keywordWords . font-lock-constant-face)
          ("##" . font-lock-comment-delimiter-face)
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


;; keybinding

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(defvar visi-keymap nil "Keybinding for `visi-mode'")
(progn
  (setq visi-keymap (make-sparse-keymap))
  (define-key visi-keymap (kbd "<tab>") 'visi-complete-or-indent)

  (define-key visi-keymap (kbd "C-x C-e") 'visi-eval-line-or-region)
  (define-key visi-keymap (kbd "C-x M-c") 'cider-connect)
  (define-key visi-keymap (kbd "C-x M-j") 'cider-jack-in)

  (define-prefix-command 'visi-single-keys-keymap)
  (define-key visi-keymap (kbd "<menu> e") visi-single-keys-keymap)
  (define-key visi-single-keys-keymap (kbd "w m") 'visi-eval-line-or-region)

)



;; define the mode
(defun visi-mode ()
  "A major mode for Visi.

\\{visi-keymap}"
  (interactive)

  (kill-all-local-variables)
  (setq mode-name "visi")
  (setq major-mode 'visi-mode)
  (setq font-lock-defaults '((visi-font-lock-keywords)))

  (set-syntax-table visi-syntax-table)
  (use-local-map visi-keymap)
  (setq local-abbrev-table visi-abbrev-table)

  (setq-local comment-start "##")
  (setq-local comment-end "")
  (setq-local comment-start-skip "##+ *")
  (setq-local comment-add 1)
  (setq-local comment-column 2)

  (abbrev-mode 1)

  (run-mode-hooks 'visi-mode-hook))

(provide 'visi-mode)
