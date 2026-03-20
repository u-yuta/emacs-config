;;; org-note-tree.el --- Tree navigation for sequential org notes -*- lexical-binding: t; -*-

;; This file provides experimental navigation for Org notes connected by a
;; `PARENT` property.
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
;; Current implementation uses Vulpea as the note backend.
;; The design may change in the future.

;;; Code:

(require 'seq)
(require 'vulpea)
(require 'vulpea-db)

(defgroup org-note-tree nil
  "Tree navigation based on note property PARENT."
  :group 'org)

(defcustom org-note-tree-parent-key "PARENT"
  "Property key that stores parent note ID."
  :type 'string)

(defcustom org-note-tree-order-key "ORDER"
  "Property key that stores note order among siblings."
  :type 'string)

(defun org-note-tree-current-note ()
  "Return current buffer's file-level note, or nil."
  (when-let* ((path (buffer-file-name))
              (notes (vulpea-db-query-by-file-paths (list path) 0)))
    (car notes)))

(defun org-note-tree-note-prop-value (note key)
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

(defun org-note-tree-note-parent-id (note)
  "Return NOTE's parent ID from PARENT property, or nil."
  (when-let ((val (org-note-tree-note-prop-value note org-note-tree-parent-key)))
    (let ((s (string-trim (format "%s" val))))
      (unless (string-empty-p s)
        s))))

(defun org-note-tree-note-order (note)
  "Return NOTE's sibling order, or nil."
  (when-let ((val (org-note-tree-note-prop-value note org-note-tree-order-key)))
    (let ((s (string-trim (format "%s" val))))
      (unless (string-empty-p s)
        (string-to-number s)))))

(defun org-note-tree-note-parent (note)
  "Return parent note of NOTE, or nil."
  (when-let ((parent-id (org-note-tree-note-parent-id note)))
    (vulpea-db-get-by-id parent-id)))

(defun org-note-tree-note-child-p (note candidate)
  "Return non-nil when CANDIDATE is a child of NOTE."
  (let ((id (vulpea-note-id note)))
    (and (not (equal (vulpea-note-id candidate) id))
         (equal (org-note-tree-note-parent-id candidate) id))))

(defun org-note-tree-sort-notes (notes)
  "Return NOTES sorted for display."
  (seq-sort-by
   (lambda (note)
     (list (or (org-note-tree-note-order note) most-positive-fixnum)
           (org-note-tree-note-label note)))
   (lambda (a b)
     (or (< (car a) (car b))
         (and (= (car a) (car b))
              (string-lessp (cadr a) (cadr b)))))
   notes))

(defun org-note-tree-note-children (note)
  "Return file-level child notes of NOTE."
  (org-note-tree-sort-notes
   (seq-filter
    (lambda (candidate)
      (org-note-tree-note-child-p note candidate))
    (vulpea-db-query-by-level 0))))

(defun org-note-tree-note-label (note)
  "Return display label for NOTE."
  (let ((title (vulpea-note-title note))
        (path  (vulpea-note-path note)))
    (if (and title (not (string-empty-p title)))
        title
      (file-name-base path))))

(defun org-note-tree-visit-note (note)
  "Visit NOTE."
  (find-file (vulpea-note-path note)))

(defun org-note-tree-child-candidates (children)
  "Return completion candidates for CHILDREN."
  (mapcar
   (lambda (note)
     (cons (format "%s [%s]"
                   (org-note-tree-note-label note)
                   (vulpea-note-id note))
           note))
   children))

(defun org-note-tree-neighborhood-data (note)
  "Return display data for NOTE neighborhood."
  (list :current note
        :parent (org-note-tree-note-parent note)
        :children (org-note-tree-note-children note)))

(defun org-note-tree-insert-neighborhood (data)
  "Insert neighborhood DATA into current buffer."
  (let ((current (plist-get data :current))
        (parent (plist-get data :parent))
        (children (plist-get data :children)))
    (insert (format "Current: %s\n\n" (org-note-tree-note-label current)))
    (insert "Parent:\n")
    (insert (if parent
                (format "- %s\n" (org-note-tree-note-label parent))
              "- (none)\n"))
    (insert "\nChildren:\n")
    (if children
        (dolist (child children)
          (insert (format "- %s\n" (org-note-tree-note-label child))))
      (insert "- (none)\n"))))

(defun org-note-tree-visit-parent ()
  "Visit current note's parent."
  (interactive)
  (if-let* ((note (org-note-tree-current-note)))
      (if-let* ((parent (org-note-tree-note-parent note)))
          (org-note-tree-visit-note parent)
        (if (org-note-tree-note-parent-id note)
            (message "Parent note not found")
          (message "No parent note")))
    (message "Current buffer is not a file-level note")))

(defun org-note-tree-visit-child ()
  "Visit one child of current note.
If multiple children exist, ask user to choose."
  (interactive)
  (if-let* ((note (org-note-tree-current-note)))
      (let ((children (org-note-tree-note-children note)))
        (pcase children
          ('nil
           (message "No child notes"))
          (`(,only)
           (org-note-tree-visit-note only))
          (_
           (let* ((alist (org-note-tree-child-candidates children))
                  (choice (completing-read "Child: " alist nil t)))
             (when-let ((selected (cdr (assoc choice alist))))
               (org-note-tree-visit-note selected))))))
    (message "Current buffer is not a file-level note")))

(defun org-note-tree-show-neighborhood ()
  "Show current note's parent and children."
  (interactive)
  (if-let* ((note (org-note-tree-current-note)))
      (let* ((data (org-note-tree-neighborhood-data note))
             (buf (get-buffer-create "*org-note-tree-neighborhood*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (org-note-tree-insert-neighborhood data)
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer buf))
    (message "Current buffer is not a file-level note")))

(provide 'org-note-tree)
;;; org-note-tree.el ends here
