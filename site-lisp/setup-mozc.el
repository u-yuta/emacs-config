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
  :ensure t
  :vc (:url "https://github.com/google/mozc.git"
            :branch "master"
            :rev "d703e61"
            :lisp-dir "src/unix/emacs/")
  :after mozc-im
  :config
  ;; mozc emacs helper for Windows (https://github.com/smzht/mozc_emacs_helper)
  ;; のコミット a670237 (May 17, 2025)で動作確認。
  ;; （mozc emacs helper for Windowsの古いバージョンを、
  ;;   mozc.elの 2025-01-07 以降のバージョンと組み合わせて使うと
  ;;   変換候補が表示されない問題が出るので注意。)
  (setq mozc-helper-program-name "mozc_emacs_helper.sh")

  (setq default-input-method "japanese-mozc")

  ;; popup スタイル を使用する
  (setq mozc-candidate-style 'popup)

  ;; mozc.el 2.31.5851.102 のアップデート後、下記adviceがあると
  ;; input-method有効化後にひらがな入力にならず直接入力状態になる問題が出た。
  ;; 別の環境でも様子を見るためコメントアウトしておく。問題なければ削除する。
  ;; (advice-add 'mozc-session-execute-command
  ;;             :after (lambda (&rest args)
  ;;                      (when (eq (nth 0 args) 'CreateSession)
  ;;                        ;; (mozc-session-sendkey '(hiragana)))))
  ;;                        (mozc-session-sendkey '(Hankaku/Zenkaku)))))
  )

(provide 'setup-mozc)
