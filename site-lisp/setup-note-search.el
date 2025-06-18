;;; setup-note-search.el --- note search configurations -*- lexical-binding: t -*

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

;; Denote, org-roam のノートを横断的に検索する
(use-package consult-notes
  :ensure t
  :vc (:url "https://github.com/mclear-tools/consult-notes" :branch "main" :rev :newest)
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; if using org-roam
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (setopt consult-notes-file-dir-sources
          '(("Documents"  ?d  "~/Documents")
            ("org"  ?o  "~/Documents/org")))
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (setopt consult-notes-org-headings-files `(,(uy/journal-file-name-year-month)
                                             "~/Documents/org/s0-agenda"))
  (consult-notes-org-headings-mode)
  (when (locate-library "denote")
    (consult-notes-denote-mode))
  ;; search only for text files in denote dir
  (setopt consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))
