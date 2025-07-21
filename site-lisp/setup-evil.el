;;; setup-evil.el --- Evil configurations -*- lexical-binding: t -*

;; Copyright (C) 2025 u-yuta
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setopt evil-respect-visual-line-mode t)  ;; visual-line-modeに応じて表示行での移動を行う
  :config
  (evil-mode 1)
  (setopt evil-emacs-state-modes (append evil-emacs-state-modes  
                                         '(Info-mode
                                           shell-mode
                                           debugger-mode
                                           eshell-mode
                                           vterm-mode
                                           eat-mode
                                           jupyter-repl-mode
                                           )))
  ;; Magit のコミットメッセージ編集画面でInsert modeにする
  (add-hook 'with-editor-mode-hook 'evil-insert-state)

  (with-eval-after-load 'org
    (evil-define-key '(motion normal) org-mode-map (kbd "RET") #'org-open-at-point)
    (evil-define-key '(motion normal) org-mode-map (kbd "M-j") #'avy-goto-char-timer))

  (evil-define-key 'normal 'global (kbd "C-r") #'isearch-backward)  ;; isearch-backword
  (evil-define-key 'normal 'global (kbd "C-.") nil)  ;; Embarkで使う
  (evil-define-key 'normal 'global (kbd "M-.") nil)  ;; Embarkで使う
  (evil-define-key 'insert 'global (kbd "C-t") nil)  ;; IMEで使う
  (evil-define-key 'insert 'global (kbd "C-o") nil)  ;; IMEで使う
  (evil-define-key 'insert 'global (kbd "C-p") nil)  ;; IMEで使う

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
  (setopt evil-collection-mode-list nil)
  (add-to-list 'evil-collection-mode-list '(magit dired python info vterm)))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation textobjects calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-markdown
  :ensure t
  :vc (:url "https://github.com/Somelauw/evil-markdown" :branch "main" :rev :newest))

(provide 'setup-evil)

