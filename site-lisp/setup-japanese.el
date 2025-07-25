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
(set-face-attribute 'variable-pitch nil :family "Noto Sans JP" :height 114)

;; 等幅フォント
(set-face-attribute 'fixed-pitch nil :family "UDEV Gothic NF" :height 114)

;; ツールチップ表示フォント
(set-face-attribute 'tooltip nil :family "UDEV Gothic NF" :height 100)

;; 絵文字の表示フォント設定
(setopt use-default-font-for-symbols nil)
(setopt emoji-font "Segoe UI Emoji")  ;; `Noto Color Emoji' だと表示されなかった。
(setopt emoji-font-fallback "Segoe UI Symbol")  ;; emoji-font に含まれない文字はこれで表示
(set-fontset-font t 'emoji emoji-font)
(set-fontset-font t 'emoji emoji-font-fallback nil 'append)
;; 絵文字コード範囲を追加設定する
;; 標準で設定されている emoji 範囲 (127744 128512) に含まれていない絵文字ブロックの定義
(setopt uy/emoji-unicode-ranges-add ;; Unicode絵文字ブロック
        '((#x1F300 . #x1F5FF) ;; "Misc Symbols and Pictographs"
          (#x1F600 . #x1F64F) ;; "Emoticons"
          (#x1F650 . #x1F67F) ;; "Ornamental Dingbats"
          (#x1F680 . #x1F6FF) ;; "Transport and Map"
          (#x1F900 . #x1F9FF) ;; "Supplemental Symbols"
          ;; その他の記号範囲
          (#x2600 . #x26FF) ;; "Miscellaneous Symbols"
          (#x2700 . #x27BF) ;; "Dingbats"
          ))
(dolist (range-info uy/emoji-unicode-ranges-add)
  (set-fontset-font t range-info emoji-font)
  (set-fontset-font t range-info emoji-font-fallback nil 'append))

;; 絵文字表示確認用のサンプル
;; Misc Symbols and Pictographs (U+1F300-1F5FF): 🌀🌁🌂🌃🌄🌅🌆🌇🌈🌉
;; Emoticons (U+1F600-1F64F): 😀😁😂😃😄😅😆😇😈😉
;; Ornamental Dingbats (U+1F650-1F67F): 🙐🙑🙒🙓🙔🙕🙖🙗🙘🙙
;; Transport and Map (U+1F680-1F6FF): 🚀🚁🚂🚃🚄🚅🚆🚇🚈🚉
;; Supplemental Symbols (U+1F900-1F9FF): 🤌🤍🤎🤏🤐🤑🤒🤓🤔🤕
;; Miscellaneous Symbols (U+2600-26FF): ☀☁☂☃☄★☆☇☈☉
;; Dingbats (U+2700-27BF): ✀✁✂✃✄✅✆✇✈✉

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
        (activate-input-method default-input-method)))

  (defun disable-input-method (&optional arg interactive)
    (interactive "P\np")
    (if current-input-method
	(disable-input-method)))

  (defun isearch-enable-input-method ()
    (interactive)
    (if (not current-input-method)
        (isearch-toggle-input-method)
      (cl-letf (((symbol-function 'toggle-input-method)
                 (symbol-function 'ignore)))
        (isearch-toggle-input-method))))

  (defun isearch-disable-input-method ()
    (interactive)
    (if current-input-method
        (isearch-toggle-input-method)
      (cl-letf (((symbol-function 'toggle-input-method)
                 (symbol-function 'ignore)))
        (isearch-toggle-input-method))))

  ;; wdired 終了時に IME を OFF にする
  (advice-add 'wdired-finish-edit
              :after (lambda (&rest args)
                       (deactivate-input-method)))
  )

;; visual line mode で日本語文章が適度に折り返し表示されるようにする
(word-wrap-whitespace-mode 1)
;; `]' や句読点を折り返しの区切り文字に加える
(setopt word-wrap-whitespace-characters (append word-wrap-whitespace-characters '(?\] ?\。 ?\、)))

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

;; IME切り替えキー設定
(global-set-key (kbd "<muhenkan>") 'disable-input-method)
(define-key isearch-mode-map (kbd "<muhenkan>") 'isearch-disable-input-method)
(global-set-key (kbd "<henkan>") 'enable-input-method)
(define-key isearch-mode-map (kbd "<henkan>") 'isearch-enable-input-method)
(global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
(define-key isearch-mode-map (kbd "<zenkaku-hankaku>") 'isearch-toggle-input-method)
(global-set-key (kbd "C-<f9>") 'disable-input-method)
(global-set-key (kbd "C-<f10>") 'enable-input-method)

(provide 'setup-japanese)
