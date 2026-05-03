;;; agent-introspect-cli.el --- Introspection API for agent CLI -*- lexical-binding: t; -*-

;; このファイルは、CLI エージェントから Emacs 環境を調査するための
;; 最小限の Introspection API を提供する。
;;
;; M-: での簡単な動作確認例:
;; (my/agent-cli-function-completions "find-file")
;; (my/agent-cli-variable-completions "user-")
;; (my/agent-cli-function-documentation "find-file")
;; (my/agent-cli-variable-documentation "user-full-name")
;; (my/agent-cli-function-source "find-file")
;; (my/agent-cli-variable-value "user-full-name")

;; CLIからユーザーのEmacs環境のIntrospectionを行うためのAPI。

;; 関数の候補リストを得る
(defun my/agent-cli-function-completions (prefix)
  (require 'orderless)
  (orderless-filter prefix obarray #'functionp))

;; 変数の候補リストを得る
(defun my/agent-cli-variable-completions (prefix)
  (require 'orderless)
  (orderless-filter prefix obarray #'boundp))

;; 関数ソースを得る
(defun my/agent-cli--introspect-source (symbol &optional type)
  "Return source snippet for SYMBOL (function by default, TYPE otherwise)."
  (when-let* ((sym (intern-soft symbol))
              (save-silently t)
              (vc-follow-symlinks t)
              (loc (find-definition-noselect sym type)))
    (with-current-buffer (car loc)
      (goto-char (cdr loc))
      (let ((beg (point)))
        (cond
         ((null type) (end-of-defun))
         ((derived-mode-p 'c-mode) (forward-sexp 2) (forward-char))
         ((derived-mode-p 'emacs-lisp-mode) (forward-sexp))
         (t (error "Unexpected file mode: %S" major-mode)))
        (buffer-substring-no-properties beg (point))))))

(defun my/agent-cli-function-source (name)
  (or (my/agent-cli--introspect-source name)
      (error "Function not found: %s" name)))

;; 関数のドキュメントを得る
(defun my/agent-cli-function-documentation (name)
  (let ((sym (intern-soft name)))
    (unless (and sym (fboundp sym))
      (error "Function not found: %s" name))
    (documentation sym)))

;; 変数のドキュメントを得る
(defun my/agent-cli-variable-documentation (name)
  (let ((sym (intern-soft name)))
    (unless (and sym (boundp sym))
      (error "Variable not found: %s" name))
    (require 'cus-edit)
    (custom-variable-documentation sym)))

;; 変数の値を得る
(defun my/agent-cli-variable-value (name)
  (let ((sym (intern-soft name)))
    (unless (and sym (boundp sym))
      (error "Variable not found: %s" name))
    (default-value sym)))

(provide 'agent-introspect-cli)
;;; agent-introspect-cli.el ends here
