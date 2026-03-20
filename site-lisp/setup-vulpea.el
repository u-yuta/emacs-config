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
  ;; 1. Configure (defaults to org-directory, so often not needed)
  (setq vulpea-db-sync-directories '("~/Documents"))

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

;; A journaling interface for vulpea that integrates seamlessly with vulpea-ui sidebar.
(use-package vulpea-journal
  :ensure t
  :after (vulpea vulpea-ui)
  :bind (("C-c j" . vulpea-journal))
  :config
  (vulpea-journal-setup)
  (setopt vulpea-journal-default-template
          (vulpea-journal-template-daily
           :file-name (file-name-concat my/journal-directory "%Y/journal-%Y%m%d.org")
           :title "%Y-%m-%d %A"
           :tags '("journal")
           :body (concat
                  "* 予定\n\n* やること\n\n* タスクキュー\n\n* メモ\n\n* 日報\n\n"))))

(provide 'setup-vulpea)
