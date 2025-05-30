;;; setup-denote.el --- Denote configurations -*- lexical-binding: t -*

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

;; Denoteをテスト使用中
(use-package denote
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote" :branch "main" :rev :newest)
  :hook (dired-mode . denote-dired-mode)
  :after org
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-subdirectory)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-sort-dired))
  :config
  (setopt denote-directory org-directory)
  ;; ノート作成時にサブディレクトリとタイトルを指定する
  (setopt denote-prompts '(subdirectory title))
  
  (setopt denote-known-keywords
          '("journal" "labnote" "moc" ;; 種類・用途
            "screenshot" "scan" ;; 種類
            "emacs" "math" "linux" "llm" "python"  ;; 話題
            "noexport"  ;; org-mode機能
            ))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1)

  ;; orgファイルのidentifierをorg-modeのIDとしても利用可能な書式に変更する
  (setopt denote-org-front-matter
          ":PROPERTIES:
:ID: %4$s
:END:
#+title:      %1$s
#+date:       %2$s
#+filetags:   %3$s
#+signature:  %5$s
\n")

  ;; Documents フォルダにDenoteのノートを作るコマンド
  (defun uy/denote-in-document-directory ()
    """Create a Denote-format note in `~/Documents'."""
    (interactive)
    (let ((denote-directory "~/Documents"))
      (denote-subdirectory)))

  (defun uy/denote-in-document-directory-current-month ()
    """Create a Denote-format note in `~/Documents/YYYY/mm'."""
    (interactive)
    (let ((denote-directory (format-time-string "~/Documents/%Y/%m")))
      (denote-subdirectory)))
  )

(use-package denote-sequence
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote-sequence" :branch "main" :rev :newest)
  :after denote
  :config
  (setopt denote-sequence-scheme 'alphanumeric)
  )

(use-package denote-org
  :ensure t
  :vc (:url "https://github.com/protesilaos/denote-org" :branch "main" :rev :newest)
  :after denote
  )

(with-eval-after-load 'denote
  (require 'org-to-denote))

(provide 'setup-denote)
