;;; company-sage.el --- company-sage -*- lexical-binding: t -*-

;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/stakemori/auto-complete-sage
;; Keywords: Sage, math, company
;; Version: 0.0.1
;; Package-Requires: ((company "0.9.0-cvs") (sage-shell-mode "0.0.8"))

;;; License
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code
(require 'sage-shell-mode)
(require 'company)
(defvar company-sage--state nil)

(defun company-sage--prefix ()
  (case major-mode
    (sage-shell-mode (sage-shell-cpl:parse-and-set-state)
                     (or (company-grab-symbol-cons "\\." 2)
                         'stop))
    (sage-shell:sage-mode
     (setq company-sage--state
           (sage-shell-edit:parse-current-state))
     (and
      (progn
        (sage-shell-edit:set-sage-proc-buf-internal nil nil)
        sage-shell:process-buffer)
      (get-buffer sage-shell:process-buffer)
      (sage-shell:redirect-finished-p)
      (sage-shell:output-finished-p)
      (not (company-in-string-or-comment))
      (or (company-grab-symbol-cons "\\." 2) 'stop)))))

(defvar company-sage--repl-python-kwds
  '("abs" "all" "and" "any" "apply" "as" "assert" "basestring"
    "bin" "bool" "break" "buffer" "bytearray" "callable" "chr"
    "class" "classmethod" "cmp" "coerce" "compile" "complex"
    "continue" "def" "del" "delattr" "dict" "dir" "divmod" "elif"
    "else" "enumerate" "eval" "except" "exec" "execfile" "file" "filter"
    "finally" "float" "for" "format" "from" "frozenset" "getattr" "global"
    "globals" "hasattr" "hash" "help" "hex" "id" "if" "import" "in" "input"
    "int" "intern" "is" "isinstance" "issubclass" "iter" "lambda" "len" "list"
    "locals" "long" "map" "max" "memoryview" "min" "next" "not" "object" "oct"
    "open" "or" "ord" "pass" "pow" "print" "property" "raise" "range" "raw"
    "reduce" "reload" "repr" "return" "reversed" "round" "set" "setattr"
    "slice" "sorted" "staticmethod" "str" "sum" "super" "try" "tuple" "type"
    "unichr" "unicode" "vars" "while" "with" "xrange" "yield" "zip" "__import__"))

(defun company-sage--candidates-async-repl (callback arg)
  (let ((types (sage-shell-cpl:get-current 'types)))
    (sage-shell-cpl:completion-init
     nil
     :compl-state sage-shell-cpl:current-state)
    (sage-shell:after-redirect-finished
      (funcall callback
               (let* ((case-fold-search nil)
                      (cands (sage-shell-cpl:candidates :regexp arg)))
                 (if (sage-shell:in "interface" types)
                     (append (all-completions arg company-sage--repl-python-kwds)
                             cands)
                   cands))))))


(defun company-sage--candidates-async (callback arg)
  (sage-shell-cpl:completion-init
   nil
   :compl-state company-sage--state)
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (sage-shell:after-redirect-finished
      (funcall callback
               (let ((case-fold-search nil))
                 (sage-shell-cpl:candidates :state company-sage--state
                                            :regexp arg))))))

;;;###autoload
(defun company-sage (command &optional arg &rest _args)
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-sage))
    (`prefix
     (company-sage--prefix))
    (`candidates
     (case major-mode
       (sage-shell-mode
        (cons :async (lambda (callback)
                       (company-sage--candidates-async-repl callback arg))))
       (sage-shell:sage-mode
        (cons :async (lambda (callback)
                       (company-sage--candidates-async callback arg))))))))

;;; company-sage.el ends here
