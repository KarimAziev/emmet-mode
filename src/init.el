;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst emmet-mode-version "1.0.10")

(require 'cl-lib)

(defmacro emmet-defparameter (symbol &optional initvalue docstring)
  "Define variable SYMBOL with DOCSTRING and assign INITVALUE."
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

(defun emmet-join-string (lis joiner)
  "Join list of strings LIS with separator JOINER."
  (mapconcat #'identity lis joiner))

(defun emmet-get-keys-of-hash (hash)
  "Return keys from hash-table HASH."
  (let ((ks nil))
    (maphash #'(lambda (k _v) (setq ks (cons k ks))) hash)
    ks))

(defun emmet-get-vals-of-hash (hash)
  "Return values from hash-table HASH."
  (let ((vs nil))
    (maphash #'(lambda (_k v)
                 (setq vs (cons v vs)))
             hash)
    vs))

(defvar emmet-leaf-function nil
  "Function to execute when expanding a leaf node in the Emmet AST.")

(defun emmet-jsx-prop-value-var? (prop-value)
  "Check whether PROP-VALUE is jsx property."
  (string-match "{.+}" prop-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(defmacro emmet-aif (test-form then-form &rest else-forms)
  "Anaphoric if with TEST-FORM, THEN-FORM and ELSE-FORMS.
Temporary variable `it' is the result of TEST-FORM."
  `(let ((it ,test-form))
     (if it ,then-form ,@(or else-forms '(it)))))

(defmacro emmet-pif (test-form then-form &rest else-forms)
  "Evaluate THEN-FORM if first element of TEST-FORM is not error symbol.
Otherwise it evaluates the ELSE-FORMS.
Store TEST-FORM in let binding `it'."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@(or else-forms '(it)))))

(defmacro emmet-parse (regex nums label &rest body)
  "Parse according to a REGEX, NUMS, LABEL, eval BODY.
Update the `input' variable."
  `(emmet-aif (emmet-regex ,regex input ',(number-sequence 0 nums))
              (let ((input (elt it ,nums)))
                ,@body)
              `,`(error ,(concat "expected " ,label))))

(defmacro emmet-run (parser then-form &rest else-forms)
  "Eval THEN-FORM if result of calling PARSER is non nil, other eval ELSE-FORMS."
  `(emmet-pif (,parser input)
              (let ((input (cdr it))
                    (expr (car it)))
                ,then-form)
              ,@(or else-forms '(it))))

(defmacro emmet-por (parser1 parser2 then-form &rest else-forms)
  "Try PARSER1 or PARSER2.
If it success, eval THEN-FORM, otherwise expand ELSE-FORMS."
  `(emmet-pif (,parser1 input)
              (let ((input (cdr it))
                    (expr (car it)))
                ,then-form)
              (emmet-pif (,parser2 input)
                         (let ((input (cdr it))
                               (expr (car it)))
                           ,then-form)
                         ,@else-forms)))

(defmacro emmet-find (direction regexp &optional limit-of-search repeat-count)
  "Search for REGEXP in given DIRECTION with LIMIT-OF-SEARCH and REPEAT-COUNT.
Return the position (or nil) and leaving the point in place."
  `(save-excursion
     (if (,(intern (concat "re-search-" direction))
          ,regexp ,limit-of-search t ,repeat-count)
         (match-beginning 0))))

(defun emmet-regex (regexp string refs)
  "Return a list of REFS that match for a REGEXP on a STRING or nil."
  (if (string-match (concat "^" regexp "\\([^\n]*\\)$") string)
      (mapcar (lambda (ref)
                (match-string ref string))
              (if (sequencep refs) refs (list refs)))
    nil))