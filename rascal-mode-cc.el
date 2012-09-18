;; Rascal language major mode for Emacs.
;;
;; Based on CC mode.
;;
;; Rascal adaptation by Tero Hasu <tero at ii dot uib dot no>.
;;
;; References:
;; - http://cc-mode.sourceforge.net/derived-mode-ex.el
;; - CC mode itself, particularly cc-langs.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; INSTALLATION
;;
;; (setq auto-mode-alist
;;      (cons '("\\.rsc" . rascal-mode) auto-mode-alist))
;; (autoload 'rascal-mode "rascal-mode-cc"
;;           "mode for editing Rascal source files" t)

(defvar rascal-mode-hook nil
  "Normal hook run when entering Rascal mode.")

(defvar rascal-mode-map nil
  "Keymap for Rascal mode.")

(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; for anything not defined here use java-mode setting
  ;; (c-add-language must appear before c-lang-defconst)
  (c-add-language 'rascal-mode 'java-mode))

(c-lang-defconst c-doc-comment-start-regexp
  rascal "/\\*\\*")
(c-lang-defconst c-identifier-ops
  rascal '((left-assoc "::")))
(c-lang-defconst c-after-id-concat-ops
  rascal nil)
(c-lang-defconst 
 c-operators
 rascal
 `(,@(c-lang-const c-identifier-ops)
   (right-assoc ,@(c-lang-const c-assignment-operators))
   (left-assoc ".") ;; field selection
   (left-assoc "@") ;; annotation selection
   (prefix "+" "-" "!") ;; unary
   (prefix "/") ;; recursive pattern match
   (postfix "+" "*") ;; transitive closures
   (postfix "?") ;; annotation query
   (prefix "throw")
   (left-assoc "+" "-" "*" "/" "%")
   (left-assoc "<<" ">>")
   (left-assoc "<" ">" "<=" ">=")
   (left-assoc "==" "!=")
   (left-assoc "&&" "||")
   (left-assoc "&" "|")
   (left-assoc "is" "has")
   (left-assoc "join")
   (left-assoc "in" "notin")
   (left-assoc "mod") ;; modulo
   (left-assoc "o") ;; composition
   (left-assoc "?") ;; value or alternative
   (left-assoc "==>" "<==>") ;; implication
   (left-assoc ":=" "!:=" "<-") ;; pattern match
   (left-assoc "=>") ;; rewrite
   (left-assoc ":") ;; pattern capture
   (right-assoc-sequence "?" ":")))
(c-lang-defconst c-constant-kwds
  rascal '("false" "true" "it"))
(c-lang-defconst c-type-prefix-kwds
  rascal '("bag" "list" "map" "rel" "set" "tuple"))
(c-lang-defconst c-primitive-type-kwds
  rascal 
  ;; Rascal basic types. (Also all names in a type position get
  ;; highlighted, which may or may not be a good thing.)
  '("adt" "bool" "constructor" "datetime" "fun" "int" "loc" 
    "node" "non-terminal" "num" "parameter" "real" "reified" 
    "str" "type" "value" "void"))
(c-lang-defconst c-modifier-kwds
  rascal '("default" "java" "private" "public"))
(c-lang-defconst c-cpp-matchers
  rascal nil) ;; no preprocessor in Rascal
(c-lang-defconst c-typedef-kwds
  rascal '("alias" "data"))
(c-lang-defconst c-postfix-decl-spec-kwds
  rascal '("throws"))
(c-lang-defconst c-type-list-kwds
  rascal '("extend" "import"))
(c-lang-defconst c-ref-list-kwds
  rascal '("module"))
(c-lang-defconst c-lambda-kwds
  ;; Achieves correct indentation when visit is used as an expression
  ;; (kind of is a lambda, with immediate application), but a
  ;; semicolon is still required. Might have to list the other
  ;; statements usable as expressions here as well.
  rascal '("visit"))
(c-lang-defconst c-inexpr-block-kwds
  rascal nil)
(c-lang-defconst c-inexpr-class-kwds
  rascal nil)
(c-lang-defconst c-inexpr-brace-list-kwds
  rascal nil)
(c-lang-defconst c-block-stmt-1-kwds
  rascal '("do" "else" "try"))
(c-lang-defconst c-block-stmt-2-kwds
  rascal '("for" "if" "solve" "switch" "visit" "while"))
(c-lang-defconst c-simple-stmt-kwds
  rascal '("append" "break" "continue" "fail" 
	   "filter" "insert" "return" "throw"))
(c-lang-defconst c-paren-stmt-kwds
  rascal nil)
(c-lang-defconst c-before-label-kwds
  rascal nil)
(c-lang-defconst c-case-kwds
  rascal '("case"))
(c-lang-defconst c-label-kwds
  rascal '("case" "default"))
(c-lang-defconst c-other-kwds
  rascal
  ;; catch-all for uncategorized Rascal keywords
  '("all" "anno" "any" "assert"
    "catch"
    "dynamic"
    "finally" "function" 
    "global"
    "keyword" 
    "layout" "lexical"
    "renaming" "rule" 
    "start" "syntax" 
    "tag"
    "variable" "view"
    "when"
    
    "assoc" "left" "non-assoc" "right"
    
    ;; Statement modifiers.
    "top-down-break" "top-down" "bottom-up" 
    "bottom-up-break" "outermost" "innermost"	
    ))
(c-lang-defconst c-recognize-colon-labels
  rascal nil)

(defconst rascal-font-lock-keywords-1 (c-lang-const c-matchers-1 rascal)
  "Minimal highlighting for Rascal mode.")
(defconst rascal-font-lock-keywords-2 (c-lang-const c-matchers-2 rascal)
  "Fast normal highlighting for Rascal mode.")
(defconst rascal-font-lock-keywords-3 (c-lang-const c-matchers-3 rascal)
  "Accurate normal highlighting for Rascal mode.")
(defvar rascal-font-lock-keywords rascal-font-lock-keywords-3
  "Default expressions to highlight in Rascal mode.")

(defvar rascal-mode-syntax-table nil
  "Syntax table used in rascal-mode buffers.")
(or rascal-mode-syntax-table
    (setq rascal-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table rascal))))

(setq rascal-mode-map 
  (c-make-inherited-keymap))

(easy-menu-define rascal-menu 
  rascal-mode-map "Rascal Mode Commands"
  (cons "Rascal" (c-lang-const c-mode-menu rascal)))

;; Dictionary directory relative to this source file. Dictionary files
;; named after mode, i.e. "rascal-mode" in this case, and should just
;; contain words, one per line. Our "rascal-mode" is currently empty
;; since 'ac-source-rascal-candidates' has everything. You can use M-x
;; ac-clear-dictionary-cache to refresh in-memory database if you add
;; anything.
(defvar rascal-ac-dict-directory
  (concat (file-name-directory load-file-name) "ac-dict")
  "Rascal Mode specific dictionary directory.")

(defvar rascal-mode-el load-file-name
  "Rascal Mode main Emacs Lisp source file.")

(defun ac-source-rascal-candidates ()
  (append
   '(
     ("alias" "keyword")
     ("all" "keyword")
     ("anno" "keyword")
     ("any" "keyword")
     ("append" "keyword")
     ("assert" "keyword")
     ("assoc" "keyword")
     ("bottom-up" "keyword")
     ("bottom-up-break" "keyword")
     ("break" "keyword")
     ("case" "keyword")
     ("catch" "keyword")
     ("continue" "keyword")
     ("data" "keyword")
     ("default" "keyword")
     ("do" "keyword")
     ("dynamic" "keyword")
     ("else" "keyword")
     ("extend" "keyword")
     ("fail" "keyword")
     ("finally" "keyword")
     ("for" "keyword")
     ("function" "keyword")
     ("global" "keyword")
     ("has" "keyword")
     ("if" "keyword")
     ("import" "keyword")
     ("in" "keyword")
     ("innermost" "keyword")
     ("insert" "keyword")
     ("is" "keyword")
     ("it" "keyword")
     ("java" "keyword")
     ("join" "keyword")
     ("keyword" "keyword")
     ("layout" "keyword")
     ("left" "keyword")
     ("lexical" "keyword")
     ("module" "keyword")
     ("non-assoc" "keyword")
     ("notin" "keyword")
     ("o" "keyword")
     ("outermost" "keyword")
     ("private" "keyword")
     ("public" "keyword")
     ("renaming" "keyword")
     ("return" "keyword")
     ("right" "keyword")
     ("rule" "keyword")
     ("solve" "keyword")
     ("start" "keyword")
     ("switch" "keyword")
     ("syntax" "keyword")
     ("tag" "keyword")
     ("test" "keyword")
     ("throw" "keyword")
     ("throws" "keyword")
     ("top-down" "keyword")
     ("top-down-break" "keyword")
     ("try" "keyword")
     ("variable" "keyword")
     ("view" "keyword")
     ("visit" "keyword")
     ("when" "keyword")
     ("while" "keyword")

     ("adt" "built-in primitive data type")
     ("bool" "built-in primitive data type")
     ("constructor" "built-in primitive data type")
     ("datetime" "built-in primitive data type")
     ("fun" "built-in primitive data type")
     ("int" "built-in primitive data type")
     ("loc" "built-in primitive data type")
     ("node" "built-in primitive data type")
     ("non-terminal" "built-in primitive data type")
     ("num" "built-in primitive data type")
     ("parameter" "built-in primitive data type")
     ("real" "built-in primitive data type") 
     ("reified" "built-in primitive data type") 
     ("str" "built-in primitive data type")
     ("type" "built-in primitive data type")
     ("value" "built-in primitive data type")
     ("void" "built-in primitive data type")

     ("bag" "built-in parametric data type")
     ("list" "built-in parametric data type")
     ("map" "built-in parametric data type")
     ("rel" "built-in parametric data type")
     ("set" "built-in parametric data type")
     ("tuple" "built-in parametric data type")
     ) 

   rascal-prelude-candidates))

(defun ac-source-rascal-help (x)
  (format "%s" (if (listp x) (car x) x)))

;; Might also want to define 'prefix that is tailored for Rascal. At
;; least "-" should not terminate completion.
(defvar ac-source-rascal
  '((depends rascal-mode-prelude)
    (candidates . ac-source-rascal-candidates)
    (cache)
    (symbol . "r")
    (document . ac-source-rascal-help)))

(defun reload-rascal-mode ()
  (interactive)
  (load-file rascal-mode-el)
  (rascal-mode))

;;;###autoload
(defun rascal-mode ()
  "A Rascal language major mode, based on CC Mode."
  (interactive)
  (kill-all-local-variables)

  (c-initialize-cc-mode t)

  (set-syntax-table rascal-mode-syntax-table)
  (setq major-mode 'rascal-mode mode-name "Rascal")

  ;; Note that 'c-fill-paragraph' is broken in Emacs 23.3.1 at least,
  ;; but fixed in more recent versions.
  (setq c-doc-comment-style 'javadoc)

  (use-local-map rascal-mode-map)
  (c-init-language-vars rascal-mode)
  (c-common-init 'rascal-mode)

  ;; For auto-complete-mode.
  (when (boundp 'ac-dictionary-directories)
    (add-to-list 'ac-dictionary-directories rascal-ac-dict-directory))

  ;; For auto-complete-mode.
  (when (boundp 'ac-sources)
    ;; ac-source-rascal better be first since it supports hover help.
    (setq ac-sources '(ac-source-rascal 
		       ac-source-yasnippet 
		       ac-source-dictionary
		       ac-source-words-in-same-mode-buffers)))

  ;; For auto-complete-mode.
  (when (boundp 'ac-ignore-case)
    ;; Perhaps a better default for Rascal.
    (setq ac-ignore-case nil))

  (run-hooks 'c-mode-common-hook)
  (run-hooks 'rascal-mode-hook))

(provide 'rascal-mode-cc)
