;;; org-to-denote.el --- Export org-mode heading to denote file -*- lexical-binding: t -*-

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

;;; Commentary:

;; This package provides a function to export the current org-mode heading
;; and its contents to a denote file.

;;; Code:

(require 'org)
(require 'denote)

(defcustom org-to-denote-file-type 'org
  "File type for the exported denote file.
Fixed to 'org format to preserve org-mode properties."
  :type '(const :tag "Org" org)
  :group 'org-to-denote)


(defun org-to-denote-extract-heading-content ()
  "Extract the current heading's content without the heading itself.
Return a list (heading content timestamp property-id properties) where timestamp,
property-id, and properties may be nil. PROPERTIES blocks are removed from content."
  (org-back-to-heading t)
  (let* ((heading-text (org-to-denote--get-heading-text))
         (content (org-to-denote--get-content))
         (timestamp (org-to-denote--extract-timestamp heading-text))
         (clean-heading (org-to-denote--clean-heading heading-text timestamp))
         (properties (org-entry-properties nil))
         (property-id (org-to-denote--get-valid-property-id properties))
         (clean-content (org-to-denote--remove-properties-drawer content)))
    
    (list clean-heading clean-content timestamp property-id properties)))

(defun org-to-denote--get-heading-text ()
  "Get the heading text with links cleaned up."
  (let ((heading (nth 4 (org-heading-components))))
    (replace-regexp-in-string
     org-link-bracket-re "\\2"
     (or heading ""))))

(defun org-to-denote--get-content ()
  "Get the content of the current heading."
  (let* ((element (org-element-at-point))
         (begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element)))
    (if (and begin end)
        (buffer-substring-no-properties begin end)
      "")))

(defun org-to-denote--extract-timestamp (text)
  "Extract timestamp from TEXT in format [YYYY-MM-DD Day HH:MM]."
  (let ((timestamp-regex "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"))
    (when (string-match timestamp-regex text)
      (match-string 1 text))))

(defun org-to-denote--clean-heading (heading timestamp)
  "Remove TIMESTAMP from HEADING if present."
  (if timestamp
      (let ((timestamp-regex "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]"))
        (string-trim (replace-regexp-in-string timestamp-regex "" heading)))
    heading))

(defun org-to-denote--get-valid-property-id (properties)
  "Get valid Denote identifier from PROPERTIES if it exists."
  (let ((property-id (cdr (assoc "ID" properties))))
    (when (and property-id 
               (string-match "^[0-9]\\{8\\}T[0-9]\\{6\\}$" property-id))
      property-id)))

(defun org-to-denote--remove-properties-drawer (content)
  "Remove PROPERTIES drawer from CONTENT."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
        (let ((prop-start (match-beginning 0)))
          (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
              (delete-region prop-start (line-beginning-position 2))
            (goto-char (point-max))))))
    (string-trim (buffer-string))))


(defun org-to-denote ()
  "Export the current org-mode heading to a denote file.
This function extracts the current heading and its contents,
then creates a new denote file with org format content.

If the heading contains a timestamp like [2025-03-18 Tue 21:53],
that timestamp will be used as the note's identifier, and will
be removed from the title.

If the heading has an ID property in Denote identifier format
(YYYYMMDDThhmmss), that will be used as the identifier unless a
timestamp is found in the heading text (which takes precedence).

Assumes that `denote-org-front-matter` has been configured to include
the ID property in the desired format."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Not in org-mode"))
  
  (let* ((heading-data (org-to-denote-extract-heading-content))
         (denote-params (org-to-denote--build-params heading-data))
         (note-path (org-to-denote--create-file denote-params))
         (content (nth 1 heading-data)))
    
    (org-to-denote--insert-content note-path content)
    (message "Created denote file: %s" note-path)
    note-path))

(defun org-to-denote--build-params (heading-data)
  "Build denote parameters from HEADING-DATA."
  (let ((heading (nth 0 heading-data))
        (timestamp (nth 2 heading-data))
        (property-id (nth 3 heading-data))
        (tags (org-get-tags nil t)))
    (list :title heading
          :keywords tags
          :file-type 'org
          :id (or (when timestamp (format-time-string "%Y%m%dT%H%M%S"))
                  property-id)
          :date (when (or timestamp property-id)
                  (date-to-time (or timestamp property-id))))))

(defun org-to-denote--create-file (params)
  "Create denote file with PARAMS."
  (let ((denote-use-title (plist-get params :title))
        (denote-use-keywords (plist-get params :keywords))
        (denote-use-file-type (plist-get params :file-type))
        (denote-use-date (plist-get params :date)))
    (denote)))

(defun org-to-denote--insert-content (note-path content)
  "Insert CONTENT into denote file at NOTE-PATH."
  (with-current-buffer (find-file-noselect note-path)
    (goto-char (point-max))
    (insert content)
    (save-buffer)))

(provide 'org-to-denote)
;;; org-to-denote.el ends here
