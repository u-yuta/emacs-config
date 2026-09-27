;;; org-day-plan.el --- Daily task planning for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2026 u-yuta

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Utilities for planning a day with ordered Org tasks, effort estimates,
;; and clock records.

;;; Code:

(require 'org)
(require 'org-duration)

(defgroup org-day-plan nil
  "Daily task planning for Org."
  :group 'org)

(defcustom org-day-plan-file-function nil
  "Function returning the Org file that contains today's plan."
  :type '(choice (const :tag "Not configured" nil) function)
  :group 'org-day-plan)

(defcustom org-day-plan-heading "メモ"
  "Top-level heading whose subtree contains today's ordered tasks."
  :type 'string
  :group 'org-day-plan)

(defun org-day-plan-show-finish-time ()
  "Show the estimated finish time for today's unfinished tasks.

Sum the Effort properties of unfinished TODO entries below
`org-day-plan-heading' in the file returned by
`org-day-plan-file-function', then add that duration to the current time."
  (interactive)
  (unless (functionp org-day-plan-file-function)
    (user-error "Customize org-day-plan-file-function first"))
  (let ((file (funcall org-day-plan-file-function))
        (minutes 0)
        (tasks 0)
        (missing 0))
    (unless (and file (file-exists-p file))
      (user-error "Today's plan file does not exist: %s" file))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (unless (re-search-forward
                (format "^\\* %s[ \t]*$" (regexp-quote org-day-plan-heading))
                nil t)
         (user-error "No top-level %s heading in %s"
                     org-day-plan-heading file))
       (org-map-entries
        (lambda ()
          (when (and (org-get-todo-state)
                     (not (org-entry-is-done-p)))
            (setq tasks (1+ tasks))
            (if-let* ((effort (org-entry-get nil "Effort"))
                      (value (org-duration-to-minutes effort)))
                (setq minutes (+ minutes value))
              (setq missing (1+ missing)))))
        nil 'tree)))
    (let ((finish (time-add (current-time)
                            (seconds-to-time (* minutes 60)))))
      (message "Remaining: %s (%d tasks%s) / estimated finish: %s"
               (org-duration-from-minutes minutes)
               tasks
               (if (> missing 0)
                   (format ", %d without Effort" missing)
                 "")
               (format-time-string "%H:%M" finish)))))

(provide 'org-day-plan)
;;; org-day-plan.el ends here
