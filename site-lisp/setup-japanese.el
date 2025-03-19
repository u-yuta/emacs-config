;;; setup-japanese.el --- Japanese language environment configuration -*- lexical-binding: t -*-

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


;; 言語環境
(set-language-environment "Japanese")  ;; フォントが日本語フォントになるように
(set-default-coding-systems 'utf-8-unix)
(setq system-time-locale "C")  ;; avoid Japanese in the time stamp
(setq text-quoting-style 'straight)  ;; help や message でシングルクォートが全角になるのを抑止

;; フォントの設定
(setq use-default-font-for-symbols nil)
;; デフォルト フォント
(set-face-attribute 'default nil :family "UDEV Gothic NF" :height 114)

;; プロポーショナル フォント
;; (set-face-attribute 'variable-pitch nil :family "Yu Gothic UI" :height 114)
(set-face-attribute 'variable-pitch nil :family "Noto Sans JP" :height 114)

;; 等幅フォント
(set-face-attribute 'fixed-pitch nil :family "UDEV Gothic NF" :height 114)

;; ツールチップ表示フォント
(set-face-attribute 'tooltip nil :family "UDEV Gothic NF" :height 100)

;; 記号の文字化け対策
(cond
  ((eq system-type 'windows-nt)
   (set-fontset-font t 'symbol (font-spec :family "Segoe UI Symbol") nil nil)))

;; 日本語IME設定
(if uy/system-windows-p
    ;; Windows用の日本語IME設定
    (use-package tr-ime
      :config
      (tr-ime-advanced-install)
      (setq default-input-method "W32-IME")
      ;; IME のモードライン表示設定
      ;; 不具合が出る（他のインジケーターが表示されなくなる）のでコメントアウト
      ;; (setq-default w32-ime-mode-line-state-indicator "[--]")
      ;; (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))      
      ;; (w32-ime-initialize)
      
      ;; IME 制御（yes/no などの入力の時に IME を off にする）
      (wrap-function-to-control-ime 'universal-argument t nil)
      (wrap-function-to-control-ime 'read-string nil nil)
      (wrap-function-to-control-ime 'read-char nil nil)
      (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
      (wrap-function-to-control-ime 'y-or-n-p nil nil)
      (wrap-function-to-control-ime 'yes-or-no-p nil nil)
      (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
      (wrap-function-to-control-ime 'register-read-with-preview nil nil)
      (modify-all-frames-parameters '((ime-font . "UDEV Gothic JPDOC-11"))))
  ;; Linuxの場合はmozcを使う
  (require 'setup-mozc))

;; 日本語IME関係の設定
(use-package emacs
  :ensure nil
  :commands (toggle-input-method enable-input-method disable-input-method)
  :config
  ;; カーソルの点滅を OFF にする
  (blink-cursor-mode 0)

  (defun enable-input-method (&optional arg interactive)
    (interactive "P\np")
    (if (not current-input-method)
	    (toggle-input-method arg interactive)))

  (defun disable-input-method (&optional arg interactive)
    (interactive "P\np")
    (if current-input-method
	    (toggle-input-method arg interactive)))

  ;; wdired 終了時に IME を OFF にする
  (advice-add 'wdired-finish-edit
              :after (lambda (&rest args)
                       (deactivate-input-method)))
  )

;; visual line mode で日本語文章が適度に折り返し表示されるようにする
(word-wrap-whitespace-mode 1)
(add-to-list word-wrap-whitespace-characters ?\]) ;; `]' を折り返しの区切り文字に加える

;; pangu-spacing
;; 半角文字と全角文字の間にスペースを入れる。
(use-package pangu-spacing
  :ensure t
  :config
  (global-pangu-spacing-mode -1)
  (setq pangu-spacing-real-insert-separtor nil)
  ;; org-mode で表が崩れないようにする
  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))
  )

;; migemo設定
(use-package migemo
  :ensure t
  :if (executable-find "cmigemo")  ;; Ubuntuの場合 `sudo apt install cmigemo`
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  ;; Ubuntuで `sudo apt install cmigemo` でインストールした場合
  ;; (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  ;; AZIK対応版 (https://github.com/p-snow/cmigemo) を標準設定でビルドした場合
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

;; IME切り替えキー設定
(global-set-key (kbd "<muhenkan>") 'disable-input-method)
(global-set-key (kbd "<henkan>") 'enable-input-method)
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(global-set-key (kbd "C-<f9>") 'disable-input-method)
(global-set-key (kbd "C-<f10>") 'enable-input-method)

(provide 'setup-japanese)
