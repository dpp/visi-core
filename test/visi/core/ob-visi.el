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

;; org-man-source-highlight-langs

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

(defun org-babel-expand-body:visi (φbody φparams)
  "Expand BODY according to PARAMS, return the expanded body.
."
  (org-babel-expand-body:generic φbody φparams))

;; (org-babel-expand-body:generic
;;  "y + x + 4"
;;  '(
;;   (:var . ("x" . 7)) ; testing. no effect
;;   (:var . ('x . 7))  ; testing. no effect
;;   (:var . ('x 7))  ; testing. no effect
;;   (:var . ("x" 7))  ; testing. no effect
;;   (:comments . "")
;;   (:shebang . "")
;;   (:cache . "no")
;;   (:padline . "")
;;   (:noweb . "no")
;;   (:tangle . "no")
;;   (:exports . "both")
;;   (:results . "replace value")
;;   (:session . "none")
;;   (:hlines . "no")
;;   (:result-type . value)
;;   (:result-params "value" "replace")
;;   (:rowname-names)
;;   (:colname-names))
;;  ) ;;  "x + y + 4"

;; (org-babel-expand-body:visi
;;  "y + x + 4"
;;  '(
;;   (:var . ("x" . 7)) ; testing. no effect
;;   (:var . ('x . 8))  ; testing. no effect
;;   (:var . ('x 9))  ; testing. no effect
;;   (:var . ("x" 10))  ; testing. no effect
;;   (:comments . "")
;;   (:shebang . "")
;;   (:cache . "no")
;;   (:padline . "")
;;   (:noweb . "no")
;;   (:tangle . "no")
;;   (:exports . "both")
;;   (:results . "replace value")
;;   (:session . "none")
;;   (:hlines . "no")
;;   (:result-type . value)
;;   (:result-params "value" "replace")
;;   (:rowname-names)
;;   (:colname-names))
;;  )

;; "(let [\"x\" (quote 7)
;;       (quote x) (quote 8)
;;       (quote x) (quote (9))
;;       \"x\" (quote (10))]
;; y + x + 4)"

;; sample org babel  φob-params format
;; '(
;;   (:comments . "")
;;   (:shebang . "")
;;   (:cache . "no")
;;   (:padline . "")
;;   (:noweb . "no")
;;   (:tangle . "no")
;;   (:exports . "both")
;;   (:results . "replace value")
;;   (:session . "none")
;;   (:hlines . "no")
;;   (:result-type . value)
;;   (:result-params "value" "replace")
;;   (:rowname-names)
;;   (:colname-names))

;; ;; sample nrepl-dict-get call
;; (nrepl-dict-get (nrepl-sync-request:eval "(+ 3 4)") "out") ; nil
;; (nrepl-dict-get (nrepl-sync-request:eval "(+ 3 4)") "value") ; 7

;; (format "(visi.core.parser/parse-and-eval-for-tests \"%s\")" "3 + 4") ; "(visi.core.parser/parse-and-eval-for-tests \"3 + 4\")"

(defun org-babel-execute:visi (φvisi-code φob-params)
  "Execute a block of Visi code with Babel."

  (let* (
         (ξquoted-visi-code (replace-regexp-in-string "\"" "\\\"" φvisi-code "FIXEDCASE" "LITERAL"))
         (ξexpanded-visi-code (org-babel-expand-body:visi ξquoted-visi-code φob-params))
         (ξclojureCode (format "(visi.core.parser/parse-and-eval-for-tests \"%s\")" ξexpanded-visi-code))
         (ξresultParams (cdr (assoc :result-params φob-params)))
         ξresult)

    (message "raw input: 「%s」" φvisi-code)
    (message "quoted: 「%s」" ξquoted-visi-code)
    (message "expanded: 「%s」" ξexpanded-visi-code)
    (message "clojure code: 「%s」" ξclojureCode)
    (message "“:result-params” value: 「%s」" ξresultParams)

    (progn
      (require 'cider)
      (setq ξresult
            (nrepl-dict-get
             (nrepl-sync-request:eval ξclojureCode)
             (if (or (member "output" ξresultParams)
                     (member "pp" ξresultParams))
                 "out"
               "value"))))

    (org-babel-result-cond ξresultParams
      ξresult
      (condition-case nil (org-babel-script-escape ξresult)
	(error ξresult)))))

(message "----------------ob-visi.el loaded")

(provide 'ob-visi)

;;; ob-visi.el ends here
