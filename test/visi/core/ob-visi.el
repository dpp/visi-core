;;; ob-visi.el --- org-babel functions for Visi evaluation

;;; REQUIREMENT

;; • visi-mode.el
;; • CIDER https://github.com/clojure-emacs/cider , version 0.8.2 or later
;; • Leiningen http://leiningen.org/ , version 2.5.0 or later
;; • Visi engine. 2015-01-27 or later
;; • GNU Emacs 24.3 or later

;;; Code:
(require 'ob)
(require 'cider)

(eval-when-compile (require 'cl))

(declare-function nrepl-dict-get "ext:nrepl-client" (dict key))
(declare-function nrepl-sync-request:eval "ext:nrepl-client" (input &optional ns session))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("visi" . "visi"))

(defvar org-babel-default-header-args:visi '())
(defvar org-babel-header-args:visi '((package . :any))) ; todo

(defcustom org-babel-visi-backend
  'cider
  "Backend used to evaluate Visi code blocks."
  :group 'org-babel
  :type '(choice
	  (const :tag "cider" cider)
	  ))

(defun org-babel-expand-body:visi (body params)
  "Expand BODY according to PARAMS, return the expanded BODY.
For example, this in org mode:

 #+BEGIN_SRC visi :var xx=3 yy=5
 xx + yy
 #+END_SRC

will make this function return this string
 \"xx = 3; yy = 5;  xx + yy\"

If no :var header is used, BODY is returned unchanged.

URL `http://orgmode.org/manual/Code-block-specific-header-arguments.html#Code-block-specific-header-arguments'"
  (let (
         (ξtheVars (mapcar #'cdr (org-babel-get-header params :var)))
         (localVarStr ""))
    ;; (print (format "ξtheVars is 「%s」" ξtheVars))
    ;; (print (format "first var is symbol 「%s」" (symbolp (car (car ξtheVars)))))
    ;; (print (format "second var is number 「%s」" (numberp (cdr (car ξtheVars)))))
    ;; (print (concat (symbol-name (car (car ξtheVars))) "=" (number-to-string (cdr (car ξtheVars))) "; "))
    (mapc
     (lambda (varValConsPair)
       (setq
        localVarStr
        (concat localVarStr
                (symbol-name (car varValConsPair))
                " = "
                (number-to-string (cdr varValConsPair))
                "; ")))
     ξtheVars)
    ;; (print (format "localVarStr is 「%s」" localVarStr))
    (concat localVarStr " " body)
    ))

(defun org-babel-execute:visi (visi-code ob-params)
  "Execute a block of Visi code with Babel."
  (let* (
         ;; (quoted-visi-code (replace-regexp-in-string "\"" "\\\"" visi-code "FIXEDCASE" "LITERAL"))
         (expanded-visi-code (org-babel-expand-body:visi visi-code ob-params))
         (clojureCode
          (format
           "(visi.core.runtime/as-string (visi.core.parser/parse-and-eval-multiline (visi.core.util/decode-base64 \"%s\")))"
           (base64-encode-string expanded-visi-code t)))
         (resultParams (cdr (assoc :result-params ob-params)))
         result)

    (progn
      (require 'cider)
      (setq result
            (nrepl-dict-get
             (nrepl-sync-request:eval clojureCode)
             (if (or (member "output" resultParams)
                     (member "pp" resultParams))
                 "out"
               "value"))))

    (org-babel-result-cond resultParams
      result
      (condition-case nil (org-babel-script-escape result)
	(error result)))))

(provide 'ob-visi)

;;; ob-visi.el ends here
