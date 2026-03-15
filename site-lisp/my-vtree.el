;;; my-vtree.el --- Minimal tree navigation for Vulpea notes -*- lexical-binding: t; -*-

;; This file provides experimental navigation for notes connected by a
;; `PARENT` property stored in Vulpea notes.
;;
;; Each note may define:
;;
;;   PARENT: <note-id>
;;
;; where <note-id> is the ID of the parent note.  Notes without PARENT
;; are treated as roots.
;;
;; The current implementation is a minimal prototype and only supports
;; navigation around the current note:
;;
;;   - visit parent note
;;   - visit child notes
;;   - show parent/children summary
;;
;; The design may change in the future.

;;; Code:

(require 'seq)
(require 'vulpea)
(require 'vulpea-db)

(defgroup my-vtree nil
  "Tree navigation based on Vulpea note property PARENT."
  :group 'vulpea)

(defcustom my-vtree-parent-key "PARENT"
  "Property key that stores parent note ID."
  :type 'string)

(defun my-vtree-current-note ()
  "Return current buffer's file-level Vulpea note, or nil."
  (when-let* ((path (buffer-file-name))
              (notes (vulpea-db-query-by-file-paths (list path) 0)))
    (car notes)))

(defun my-vtree-note-property (note key)
  "Return property value of KEY from NOTE.
KEY is compared leniently against string/symbol keys."
  (when note
    (let ((props (vulpea-note-properties note)))
      (or (cdr (assoc key props))
          (cdr (assoc (upcase key) props))
          (cdr (assoc (downcase key) props))
          (cdr (assoc (intern-soft key) props))
          (cdr (assoc (intern-soft (upcase key)) props))
          (cdr (assoc (intern-soft (downcase key)) props))))))

(defun my-vtree-note-parent-id (note)
  "Return NOTE's parent ID from PARENT property, or nil."
  (when-let ((val (my-vtree-note-property note my-vtree-parent-key)))
    (let ((s (string-trim (format "%s" val))))
      (unless (string-empty-p s)
        s))))

(defun my-vtree-note-parent (note)
  "Return parent note of NOTE, or nil."
  (when-let ((parent-id (my-vtree-note-parent-id note)))
    (vulpea-db-get-by-id parent-id)))

(defun my-vtree-note-children (note)
  "Return file-level child notes of NOTE."
  (let ((id (vulpea-note-id note)))
    (seq-filter
     (lambda (candidate)
       (and (not (equal (vulpea-note-id candidate) id))
            (equal (my-vtree-note-parent-id candidate) id)))
     (vulpea-db-query-by-level 0))))

(defun my-vtree-note-label (note)
  "Return display label for NOTE."
  (let ((title (vulpea-note-title note))
        (path  (vulpea-note-path note)))
    (if (and title (not (string-empty-p title)))
        title
      (file-name-base path))))

(defun my-vtree-visit-note (note)
  "Visit NOTE."
  (find-file (vulpea-note-path note)))

(defun my-vtree-visit-parent ()
  "Visit current note's parent."
  (interactive)
  (if-let* ((note (my-vtree-current-note)))
      (if-let* ((parent (my-vtree-note-parent note)))
          (my-vtree-visit-note parent)
        (if (my-vtree-note-parent-id note)
            (message "Parent note not found")
          (message "No parent note")))
    (message "Current buffer is not a file-level Vulpea note")))

(defun my-vtree-visit-child ()
  "Visit one child of current note.
If multiple children exist, ask user to choose."
  (interactive)
  (if-let* ((note (my-vtree-current-note)))
      (let ((children (my-vtree-note-children note)))
        (pcase children
          ('nil
           (message "No child notes"))
          (`(,only)
           (my-vtree-visit-note only))
          (_
           (let* ((alist (mapcar (lambda (n)
                                   (cons (my-vtree-note-label n) n))
                                 children))
                  (choice (completing-read "Child: " alist nil t)))
             (when-let ((selected (cdr (assoc choice alist))))
               (my-vtree-visit-note selected))))))
    (message "Current buffer is not a file-level Vulpea note")))

(defun my-vtree-show-neighborhood ()
  "Show current note's parent and children."
  (interactive)
  (if-let* ((note (my-vtree-current-note)))
      (let* ((parent (my-vtree-note-parent note))
             (children (my-vtree-note-children note))
             (buf (get-buffer-create "*my-vtree-neighborhood*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (format "Current: %s\n\n" (my-vtree-note-label note)))
          (insert "Parent:\n")
          (insert (if parent
                      (format "- %s\n" (my-vtree-note-label parent))
                    "- (none)\n"))
          (insert "\nChildren:\n")
          (if children
              (dolist (child children)
                (insert (format "- %s\n" (my-vtree-note-label child))))
            (insert "- (none)\n"))
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer buf))
    (message "Current buffer is not a file-level Vulpea note")))

(provide 'my-vtree)
;;; my-vtree.el ends here
