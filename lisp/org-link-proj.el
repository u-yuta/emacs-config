;;; org-link-proj.el - Support for links to project pages in Org mode
(require 'ol)
(require 'cl-lib)
(require 'org-roam)

(org-link-set-parameters "proj"
                         :follow #'my/org-roam-open-project-node)


(defun my/org-roam-open-project-node (name)
  "Open the Org-roam project node whose title matches NAME."
  (interactive "sProject name: ")
  (if-let* ((node
             (cl-find-if
              (lambda (node)
                (string= (org-roam-node-title node)
                         (format "proj:%s" name)))
              (org-roam-node-list))))
      (org-roam-node-visit node)
    (user-error "No org-roam node found for: %s" name)))

(provide 'org-link-proj)
  
