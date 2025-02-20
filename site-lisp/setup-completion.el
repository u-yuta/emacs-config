;;; setup-completion.el --- In-buffer completion configurations -*- lexical-binding: t -*

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


;; ポップアップによる補完
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-excluded-modes '(text-mode))
  
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  )

;; Corfuに関係するEmacs設定
(use-package emacs
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setopt tab-always-indent 'complete)

  ;; Ispellのエラーが出る問題への対処
  ;; Emacs 30 and newer: Disable Ispell completion function.
  (setopt text-mode-ispell-word-completion nil)

  ;; モードに関係ないコマンドを M-x の候補から除外する.
  ;; Corfu commands are hidden, since they are not used via M-x.
  ;; This setting is useful beyond Corfu.
  (setopt read-extended-command-predicate #'command-completion-default-include-p)

  )

;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; completion at point extensions
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p a" . cape-abbrev)
         ("C-c p d" . cape-dabbrev)
         ("C-c p w" . cape-dict)
         ("C-c p e" . cape-elisp-block)
         ("C-c p l" . cape-elisp-symbol)
         ("C-c p j" . cape-emoji)
         ("C-c p f" . cape-file)
         ("C-c p h" . cape-history)
         ("C-c p k" . cape-keyword)
         ("C-c p l" . cape-line)
         ("C-c p r" . cape-rfc1345)
         ("C-c p &" . cape-sgml)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         )
  :init
  ;; Add to the global default `completion-at-point-functions'
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
)

(provide 'setup-completion)
