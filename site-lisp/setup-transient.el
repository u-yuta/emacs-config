;;; setup-transient.el --- Transient menu UI configurations -*- lexical-binding: t -*

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

;; Transientを使ったUIの設定

;; transient-define-prefixの設定メモ
;; - :variable-pitch t を設定すると日本語を含む場合に表示幅が揃う

(require 'transient)

;; よく使うファイルを開く
(transient-define-prefix uy/transient-open-file-menu ()
  ["Find file"
   ["File"
    ("j" "Journal"
     (lambda () (interactive) (find-file (file-name-concat org-directory "journal/journal.org"))))
    ("a" "Agenda"
     (lambda () (interactive) (find-file (file-name-concat org-directory "journal/agenda.org"))))
    ("i" "Shared index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "share/02_index_shared.org"))))
    ("h" "Home index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "00_index_home.org"))))
    ("o" "Office index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "01_index_office.org"))))
    ("." "Emacs init"
     (lambda () (interactive) (find-file user-init-file)))
   ]
   ["Command"
    ("p" "Project" project-find-file)
    ("r" "Recent" recentf)
    ]]
  )

(global-set-key (kbd "C-c f") 'uy/transient-open-file-menu)

;; TransientベースのUIを追加する ----------------------------------------------
(use-package casual
  :ensure t
  :bind
  ((:map Info-mode-map
         ("C-c t" . casual-info-tmenu)
         :map calc-mode-map
         ("C-c t" . casual-calc-tmenu)
         :map calc-mode-map
         ("C-c t" . casual-calc-tmenu)))
  :commands
  ;; casual-info非起動時にも使えるようにする
  (casual-info-browse-backward-paragraph casual-info-browse-forward-paragraph) 

  :config
  ;; Info
  (require 'casual-info) ; optional if using autoloaded menu

  ;; Calc
  (require 'casual-calc) ; optional if using autoloaded menu
  )

;; RegisterとBookmark関連のコマンドをまとめたTransientメニュー -----------------
(transient-define-prefix uy/transient-register-and-bookmark-menu ()
  :variable-pitch t
  ["Registers and Bookmarks\n"
   ["Resister"
    ("s" "copy-to-register リージョンをコピー" copy-to-register :transient nil)
    ("i" "insert-register 貼り付け" insert-register :transient nil)
    ("<SPC>" "point-to-register カーソル位置を登録" point-to-register :transient nil)
    ("j" "jump-to-register 登録位置へジャンプ" point-to-register :transient nil)]
   ["Bookmark"
    ("m" "bookmark-set 登録" bookmark-set :transient nil)
    ("b" "bookmark-jump ジャンプ" bookmark-jump :transient nil)
    ("l" "bookmark-bmenu-list ブックマークの一覧表示" list-bookmarks :transient nil)  ;; なぜか表示されない
    ] 
   ]
  )

(global-set-key (kbd "C-c r") 'uy/transient-register-and-bookmark-menu)

(provide 'setup-transient)

