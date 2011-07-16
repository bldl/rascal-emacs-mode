;; INSTALLATION
;; (setq auto-mode-alist
;;      (cons '("\\.rsc" . rascal-mode) auto-mode-alist))
;; (autoload 'rascal-mode "rascal-mode" "mode for editing Rascal files" t)

(defun rascal-run-script ()
  (interactive)
  (let ((cmd (format "rascal-cli --main --require %s" (buffer-file-name))))
    (save-buffer)
    (message cmd)
    (shell-command cmd)))

(defvar rascal-mode-hook nil
  "Normal hook run when entering Rascal mode.")

(defvar rascal-mode-map nil
  "Keymap for Rascal mode.")

(setq rascal-mode-map (make-sparse-keymap))

(make-face 'rascal-todo-face)
(set-face-attribute 'rascal-todo-face nil :underline t)
(set-face-attribute 'rascal-todo-face nil :foreground "red")
(set-face-attribute 'rascal-todo-face nil :height '300)

(setq rascal-keywords 
      '("alias" "case" "data" "else" "for" "if" "import" 
	"module" "public" "return" "rule" "switch" "visit" "while"))

;;;###autoload
(define-generic-mode rascal-mode
  '(("//" . nil) ("/*" . "*/"))
  rascal-keywords
  '(("\\([xX][xX][xX]+\\|FIXME\\|TODO\\)"
     (1 'rascal-todo-face t)))                ; font lock
  '("\\.rsc$")
  '(rascal-mode-special-setup)
  "A Rascal language major mode.")

(defun rascal-mode-special-setup ()
  (use-local-map rascal-mode-map)

  ;; If you don't like this, undefine or override in your hook.
  (define-key rascal-mode-map [f3]
    'rascal-run-script)

  (run-hooks 'rascal-mode-hook))

(provide 'rascal-mode)
