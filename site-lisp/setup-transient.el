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

(use-package transient
  :ensure nil
  :config
  ;; Input methodをtransient起動時に無効化する
  (defvar my/transient-original-input-method nil
    "transient 起動前に有効だった input-method を保存する変数。")

  (defun my/transient-maybe-disable-input-method ()
    (when current-input-method
      (setopt my/transient-original-input-method current-input-method)
      (deactivate-input-method)))

  (defun my/transient-restore-input-method ()
    (when my/transient-original-input-method
      (activate-input-method my/transient-original-input-method)
      (setopt my/transient-original-input-method nil)))

  (add-hook 'transient-setup-buffer-hook #'my/transient-maybe-disable-input-method)
  (add-hook 'transient-exit-hook #'my/transient-restore-input-method)
  )

;; よく使うファイルを開く
(transient-define-prefix my/transient-open-file-menu ()
  ["Find file"
   ["File"
    ("j" "Journal" org-roam-dailies-goto-today)
    ("a" "Agenda"
     (lambda () (interactive) (find-file my/org-agenda-file)))
    ("A" "Shared Agenda"
     (lambda () (interactive) (find-file (file-name-concat org-directory "org/s0-agenda/shared-agenda.org"))))
    ("h" "Home index"
     (lambda () (interactive) (find-file (file-name-concat org-directory "org/00_index_home.org"))))
    ("m" "Documents/YYYY/mm"
     (lambda () (interactive) (find-file (format-time-string "~/Documents/%Y/%m/"))))
    ("." "Emacs init"
     (lambda () (interactive) (find-file user-init-file)))
   ]
   ["Command"
    ("f" "find-file" find-file-at-point)
    ("c" "locate" consult-locate)
    ("p" "project" project-find-file)
    ("r" "recent" recentf)
    ("z" "zoxide" dired-jump-with-zoxide)
    ]
   ]
  )

(global-set-key (kbd "C-c f") 'my/transient-open-file-menu)

;; TransientベースのUIを追加する ----------------------------------------------
(use-package casual
  :ensure t
  :bind
  ((:map Info-mode-map
         ("C-c C-t" . casual-info-tmenu)
         :map calc-mode-map
         ("C-c C-t" . casual-calc-tmenu)))
  :commands
  ;; casual-info非起動時にも使えるようにする
  (casual-info-browse-backward-paragraph casual-info-browse-forward-paragraph) 

  :config
  ;; Info
  (require 'casual-info) ; optional if using autoloaded menu

  ;; Calc
  (require 'casual-calc) ; optional if using autoloaded menu
  )

;; Leader key menu -----------------------
(global-set-key (kbd "C-c m") 'my/transient-leader-menu)

(transient-define-prefix my/transient-leader-menu ()
  ["Leader key menu"
   ("a" "App" my/transient-app-map)
   ("b" "Buffers" consult-buffer)
   ("c" "Capture" my/transient-capture-map)
   ;; ("d" "Dired" (lambda () (interactive) (dired default-directory)))  ;; open current dir
   ("d" "Dired" dired-at-point)  ;; open current dir
   ("h" "Help" my/transient-help-map)  ;; defined in Helpful configuration
   ("i" "Insert" my/transient-insert-map)
   ("f" "File" my/transient-open-file-menu)
   ("g" "Goto" my/transient-goto-map)
   ("s" "Search" my/transient-search-map)
   ("w" "Window" my/transient-window-map)]
  )

;; Help
(transient-define-prefix my/transient-help-map ()
   ["Help"
    ("f" "function and command" helpful-callable)
    ("v" "variable" helpful-variable)
    ("k" "key" helpful-key)
    ("x" "command" helpful-command)
    ("." "at-point" helpful-at-point)])

;; Applications
(transient-define-prefix my/transient-app-map ()
   ["AI"
    ("g" "gptel" gptel-menu)]
   ["Others"
    ]
  )

;; Goto key map
(transient-define-prefix my/transient-goto-map ()
   ["Goto"
    ("e" "compile-error" consult-compile-error)
    ("f" "flymake" consult-flymake)
    ("g" "goto-line" consult-goto-line)
    ("j" "goto-char-timer" avy-goto-char-timer)
    ("o" "outline" consult-outline)
    ("m" "mark" consult-mark)
    ("k" "global mark" consult-global-mark)
    ("i" "imenu" consult-imenu)
    ("I" "imenu-multi" consult-imenu-multi)]
  )

;; Search key map
(transient-define-prefix my/transient-search-map ()
  ["Search"
   ["In files"
    ("D" "rg dir file-pattern" my/consult-ripgrep-in-directory)
    ("g" "grep" consult-grep)
    ("G" "Git grep" consult-git-grep)
    ("r" "Ripgrep" consult-ripgrep)
    ("l" "line" consult-line)
    ("L" "line-multi" consult-line-multi)
    ("R" "org-roam-ripgrep" bms/org-roam-rg-search)]
   ["Others"
    ("h" "isearch-history" consult-isearch-history)
    ]]
  )

;; Insert key map
(transient-define-prefix my/transient-insert-map ()
   [["Insert"
    ("t" "timestamp" my/insert-timestamp)
    ("y" "yas-insert-snippet" yas-insert-snippet)]
    ["Completion"
    ("i" "completion-at-point" completion-at-point)
    ("x" "yas-expand" yas-expand)]]
  )
(global-set-key (kbd "C-c i") 'my/transient-insert-map)
(global-set-key (kbd "M-i") 'my/transient-insert-map)

;; org-mode capture key map
;; menu to access org[-roam]-capture and custom note-creation function
(transient-define-prefix my/transient-capture-map ()
  ["Org-mode capture"
   ("j" "Journal" (lambda () (interactive) (org-capture nil "j")))
   ("l" "Lab note" my/create-new-lab-note)
   ("w" "Work" (lambda () (interactive) (org-roam-capture nil "w")))
   ("s" "Share" (lambda () (interactive) (org-roam-capture nil "s")))
   ]
  )

(provide 'setup-transient)
