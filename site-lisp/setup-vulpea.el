;;; setup-vulpea.el --- Vulpea configurations -*- lexical-binding: t -*

;; Copyright (C) 2026 u-yuta
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


;; Vulpea: A database layer for your org-mode notes.
;; `fd' and `fswatch'
(use-package vulpea
  :ensure t
  :bind (("C-c n f" . vulpea-find))
  :config
  (defun uy/org-symlink-directories (&optional base-dir)
    "Return symlinked directories directly under BASE-DIR.
BASE-DIR defaults to ~/Documents/org."
    (let* ((base (file-name-as-directory
                  (expand-file-name (or base-dir "~/Documents/org")))))
      (seq-filter
       (lambda (path)
         (and (file-symlink-p path)
              (file-directory-p path)))
       (directory-files base t directory-files-no-dot-files-regexp))))

  (defun uy/vulpea-sync-directories ()
    "Return directories for vulpea-db-sync-directories including symlink dirs."
    (delete-dups
     (append
      (list (expand-file-name "~/Documents"))
      (mapcar #'file-truename (uy/org-symlink-directories)))))

  ;; 1. Configure (defaults to org-directory, so often not needed)
  (setq vulpea-db-sync-directories (uy/vulpea-sync-directories))

  ;; 2. Build database (first time only)
  ;; (vulpea-db-sync-full-scan)

  ;; 3. Enable auto-sync
  (vulpea-db-autosync-mode +1)
  )

(use-package consult-vulpea
  :ensure t
  :after vulpea
  :config
  (consult-vulpea-mode 1))

;; Sidebar infrastructure and widget framework for vulpea notes.
(use-package vulpea-ui
  :bind (("C-c n b" . vulpea-ui-sidebar-toggle))
  :ensure t)

(provide 'setup-vulpea)
