;; -*- lexical-binding: t -*-

;; Transientを使ったUIの設定

;; transient-define-prefixの設定メモ
;; - :variable-pitch t を設定すると日本語を含む場合に表示幅が揃う

(require 'transient)

;; よく使うファイルを開く
(transient-define-prefix uy/transient-open-file-menu ()
  [
   ["Find file"
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
    ]]
  )

(global-set-key (kbd "C-c f") 'uy/transient-open-file-menu)

;; RegisterとBookmark関連のコマンドをまとめたTransientメニュー -----------------

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
(transient-define-prefix uy/register-and-bookmark-tmenu ()
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

(global-set-key (kbd "C-c r") 'uy/register-and-bookmark-tmenu)

(provide 'setup-transient)

