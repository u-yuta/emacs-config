;; -*- lexical-binding: t -*-

(use-package mozc-im :ensure t)
(use-package mozc-popup :ensure t)

(use-package mozc
  :ensure t
  :after mozc-im
  :config
  (setq mozc-helper-program-name "mozc_emacs_helper.sh")
  (require-if-exists mozc-cursor-color)

  (setq default-input-method "japanese-mozc-im")

  ;; popup スタイル を使用する
  (setq mozc-candidate-style 'popup)

  ;; カーソルカラーを設定する
  (setq mozc-cursor-color-alist '((direct        . "red")
                                  (read-only     . "yellow")
                                  (hiragana      . "green")
                                  (full-katakana . "goldenrod")
                                  (half-ascii    . "dark orchid")
                                  (full-ascii    . "orchid")
                                  (half-katakana . "dark goldenrod")))

  ;; mozc-cursor-color を利用するための対策
  (defvar-local mozc-im-mode nil)
  (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
  (advice-add 'mozc-cursor-color-update
              :around (lambda (orig-fun &rest args)
			(let ((mozc-mode mozc-im-mode))
                          (apply orig-fun args))))

  ;; isearch を利用する前後で IME の状態を維持するための対策 (mozc)
  (add-hook 'isearch-mode-hook (lambda () (setq im-state mozc-im-mode)))
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (unless (eq im-state mozc-im-mode)
		(if im-state
                    (activate-input-method default-input-method)
                  (deactivate-input-method)))))


  (advice-add 'mozc-session-execute-command
              :after (lambda (&rest args)
                       (when (eq (nth 0 args) 'CreateSession)
			 ;; (mozc-session-sendkey '(hiragana)))))
			 (mozc-session-sendkey '(Hankaku/Zenkaku)))))
  )

(provide 'setup-mozc)
