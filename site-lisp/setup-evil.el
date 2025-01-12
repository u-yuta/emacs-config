;; -*- lexical-binding: t -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setopt evil-emacs-state-modes (append evil-emacs-state-modes  
                                         '(Info-mode
                                           eshell-mode
                                           eat-mode
                                           jupyter-repl-mode)))
  ;; Magit のコミットメッセージ編集画面でInsert modeにする
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (with-eval-after-load 'org
    (evil-define-key '(motion normal) org-mode-map (kbd "RET") #'org-open-at-point)
    (evil-define-key '(motion normal) org-mode-map (kbd "M-j") #'avy-goto-char-timer))

  (evil-define-key 'normal 'global (kbd "C-r") #'isearch-backward)  ;; isearch-backword
  (evil-define-key 'normal 'global (kbd "C-.") nil)  ;; Embarkで使う
  (evil-define-key 'normal 'global (kbd "M-.") nil)  ;; Embarkで使う


  ;; すべてのブログラミングモードでアンダースコアを単語の一部として扱う
  ;; https://github.com/syl20bnr/spacemacs/blob/develop/doc/FAQ.org#include-underscores-and-dashes-in-word-motions
  (add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  )

;; 対象の文字列をカッコなどで囲む
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  (setq evil-collection-mode-list nil)
  (add-to-list 'evil-collection-mode-list '(magit dired python info vterm)))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation textobjects calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'setup-evil)
