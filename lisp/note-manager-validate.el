(defconst note-manager-valid-kinds '("task" "mission" "area")
  "Allowed KIND values for rev4 notes.")
(defconst note-manager-valid-filetags '("work" "note" "index" "ref" "journal" "data")
  "Allowed filetags for rev4 minimal validation.")
(defconst note-manager-valid-task-status '("inbox" "active" "hold" "done" "archived")
  "Allowed STATUS values for task notes.")
(defconst note-manager-valid-mission-status '("active" "hold" "done" "archived")
  "Allowed STATUS values for mission notes.")
(defun note-manager-validate-current-note ()
  "Validate current note against rev4 minimal constraints.
Return diagnostics list: (:code <symbol> :level <symbol> :message <string>)."
  (interactive)
  (let* ((meta (note-manager-current-note-metadata))
         (id (plist-get meta :id))
         (kind (plist-get meta :kind))
         (status (plist-get meta :status))
         (parent-id (plist-get meta :parent-id))
         (mission-id (plist-get meta :mission-id))
         (filetags (plist-get meta :filetags))
         diags)
    (unless id
      (push '(:code missing-id :level error :message "ID is missing") diags))
    (unless filetags
      (push '(:code missing-filetags :level error :message "filetags is missing") diags))
    (when (and kind (not (member kind note-manager-valid-kinds)))
      (push `(:code invalid-kind :level error :message ,(format "Invalid KIND: %s" kind)) diags))
    (dolist (tag filetags)
      (unless (member tag note-manager-valid-filetags)
        (push `(:code invalid-filetag :level error :message ,(format "Invalid filetag: %s" tag)) diags)))
    (when (and (equal kind "task") (not status))
      (push '(:code missing-task-status :level error :message "STATUS is missing for task") diags))
    (when status
      (let ((ok (cond
                 ((equal kind "task") (member status note-manager-valid-task-status))
                 ((equal kind "mission") (member status note-manager-valid-mission-status))
                 (t t))))
        (unless ok
          (push `(:code invalid-status :level error :message ,(format "Invalid STATUS: %s" status)) diags))))
    (when parent-id
      (unless (org-roam-node-from-id parent-id)
        (push `(:code unresolved-parent :level error :message ,(format "PARENT_ID not found: %s" parent-id)) diags)))
    (when mission-id
      (unless (org-roam-node-from-id mission-id)
        (push `(:code unresolved-mission :level error :message ,(format "MISSION_ID not found: %s" mission-id)) diags)))
    (when (member kind note-manager-valid-kinds)
      (condition-case err
          (note-manager-get-ancestors id)
        (error
         (when (string-match-p "parent-cycle-detected" (error-message-string err))
           (push `(:code parent-cycle-detected :level error :message ,(error-message-string err)) diags)))))
    (setq diags (nreverse diags))
    (when (called-interactively-p 'interactive)
      (if diags
          (message "%S" diags)
        (message "Validation OK")))
    diags))

(defun note-manager-set-status (status)
  "Set file-level STATUS property.
STATUS accepts symbol/string and is validated by KIND."
  (interactive
   (let* ((meta (note-manager-current-note-metadata))
          (kind (plist-get meta :kind))
          (choices (if (equal kind "mission")
                       note-manager-valid-mission-status
                     note-manager-valid-task-status)))
     (list (completing-read "STATUS: " choices nil t))))
  (let* ((meta (note-manager-current-note-metadata))
         (kind (plist-get meta :kind))
         (status-str (note-manager--normalize-symbol-or-string status))
         (valid (if (equal kind "mission")
                    note-manager-valid-mission-status
                  note-manager-valid-task-status)))
    (unless (member status-str valid)
      (user-error "Invalid STATUS for kind=%s: %s" kind status-str))
    (save-excursion
      (goto-char (point-min))
      (org-entry-put (point) "STATUS" status-str))
    status-str))

(provide 'note-manager-validate)
