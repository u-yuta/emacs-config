;;; setup-org-roam.el --- Org-roam configurations -*- lexical-binding: t -*

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


(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :after (org)
  :hook
  (after-init . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory org-directory)
  :bind (
         (("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n b" . org-roam-buffer-toggle)
          ("C-c n c" . org-roam-capture)
          ("C-c n d" . org-roam-dailies-capture-today)
          )
         :map org-mode-map
         (("C-c n i" . org-roam-node-insert))
         (("C-c n I" . org-roam-insert-immediate)))
  :config
  ;; Configures display formatting for Org-roam node.
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:12}" 'face 'org-tag)))

  ;; org-roam capture template
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
           ("w" "work" plain "%?" :target
           (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("p" "personal" plain "%?" :target
           (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("s" "share" plain "%?" :target
           (file+head "share/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ))
  
  ;; org-roam dailies の設定
  (setq org-roam-dailies-directory "journal/")
  )

(use-package org-roam-ui
  :ensure t
  :defer t
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'setup-org-roam)
