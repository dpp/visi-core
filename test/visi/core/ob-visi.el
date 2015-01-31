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

(defun org-babel-execute:visi (φbody φparams)
  "Execute a block of Visi code with Babel."
  (let ((ξexpanded (org-babel-expand-body:visi φbody φparams))
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

(message "----------------ob-visi.el loaded")

(provide 'ob-visi)

;;; ob-visi.el ends here
