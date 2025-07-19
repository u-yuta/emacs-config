;;; setup-mozc.el --- Japanese input configuration with Mozc -*- lexical-binding: t -*-

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


;; mozc_emacs_helper for Windows https://github.com/smzht/mozc_emacs_helper
;; を利用して、WSLのEmacsからWindowsのGoogle日本語入力を使う。

;; 事前準備:
;; emacs-mozc を動かすための設定（WSL 設定編） https://w.atwiki.jp/ntemacs/pages/61.html
;; を参考に、`mozc_emacs_helper.sh'を作成してパスを通しておく。

(use-package mozc-im :ensure t)
(use-package mozc-popup :ensure t)

(use-package mozc
  ;; 変換候補が表示されない問題
  ;; https://github.com/google/mozc/commits/master/src/unix/emacs/mozc.el
  ;; の 2025-01-07 のバージョンだと発生する。その前のコミット (9a925ee) だと問題ない。

  ;; 標準の use-package :vc だとgithubのリポジトリから特定のファイルを指定して
  ;; 持ってくる方法がわからないため、以下の設定は未更新。
  ;; 手動で古いバージョンを elpa/ 内に入れることで対処している。
  :ensure t
  :after mozc-im
  :config
  (setq mozc-helper-program-name "mozc_emacs_helper.sh")
  (require-if-exists mozc-cursor-color)

  (setq default-input-method "japanese-mozc")

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
