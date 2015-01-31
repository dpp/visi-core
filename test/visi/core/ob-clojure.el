;;; ob-clojure.el --- org-babel functions for clojure evaluation

;; Copyright (C) 2009-2014 Free Software Foundation, Inc.
;; Copyright (C) 2015 Xah Lee
;; Author: Joel Boehland, Eric Schulte, Oleh Krehel
;; 2015-01-30 modified by Xah Lee. Removed SLIME support. Changed a few var names.

;; this software is licensed by General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

;;; Code:
(require 'ob)
(eval-when-compile
  (require 'cl))

(require 'cider) ; (featurep 'cider)

(declare-function nrepl-dict-get "ext:nrepl-client" (dict key))
(declare-function nrepl-sync-request:eval "ext:nrepl-client" (input &optional ns session))
(declare-function slime-eval "ext:slime" (sexp &optional package))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure '())
(defvar org-babel-header-args:clojure '((package . :any)))

(defcustom org-babel-clojure-backend
  'cider
  "Backend used to evaluate Clojure code blocks."
  :group 'org-babel
  :type '(choice
	  (const :tag "cider" cider)
	  ))

(defun org-babel-expand-body:clojure (φbody φparams)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((ξvars (mapcar #'cdr (org-babel-get-header φparams :var)))
	 (ξresult-params (cdr (assoc :result-params φparams)))
	 (print-level nil) (print-length nil)
	 (ξbody (org-babel-trim
		(if (> (length ξvars) 0)
		    (concat "(let ["
			    (mapconcat
			     (lambda (var)
			       (format "%S (quote %S)" (car var) (cdr var)))
			     ξvars "\n      ")
			    "]\n" φbody ")")
		  φbody))))
    (if (or (member "code" ξresult-params)
	    (member "pp" ξresult-params))
	(format "(clojure.pprint/pprint (do %s))" ξbody)
      ξbody)))

(defun org-babel-execute:clojure (φbody φparams)
  "Execute a block of Clojure code with Babel."
  (let ((ξexpanded (org-babel-expand-body:clojure φbody φparams))
	ξresult)
    (progn
      (require 'cider)
      (let ((ξresultParams (cdr (assoc :result-params φparams))))
        (setq ξresult
              (nrepl-dict-get
               (nrepl-sync-request:eval ξexpanded)
               (if (or (member "output" ξresultParams)
                       (member "pp" ξresultParams))
                   "out"
                 "value")))))

    (org-babel-result-cond (cdr (assoc :result-params φparams))
      ξresult
      (condition-case nil (org-babel-script-escape ξresult)
	(error ξresult)))))

(message "----------------modified ob-clojure.el loaded")

(provide 'ob-clojure)

;;; ob-clojure.el ends here
