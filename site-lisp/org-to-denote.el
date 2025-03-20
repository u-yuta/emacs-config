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
  (let* ((heading (nth 4 (org-heading-components)))
         (heading-text (replace-regexp-in-string
                        org-link-bracket-re
                        "\\2"
                        (or heading "")))
         (element (org-element-at-point))
         (begin (org-element-property :contents-begin element))
         (end (org-element-property :contents-end element))
         (content (if (and begin end)
                      (buffer-substring-no-properties begin end)
                    ""))
         ;; Extract timestamp in format [YYYY-MM-DD Day HH:MM]
         (timestamp-regex "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]")
         (timestamp nil)
         (properties (org-entry-properties nil))
         (property-id (cdr (assoc "ID" properties)))
         (clean-heading heading-text))
    
    ;; Check if heading contains a timestamp
    (when (string-match timestamp-regex heading-text)
      (setq timestamp (match-string 1 heading-text))
      ;; Remove timestamp from heading
      (setq clean-heading (string-trim (replace-regexp-in-string timestamp-regex "" heading-text))))
    
    ;; Check if ID property has Denote identifier format (YYYYMMDDThhmmss)
    (when (and property-id 
               (string-match "^[0-9]\\{8\\}T[0-9]\\{6\\}$" property-id))
      (setq property-id property-id))
    
    ;; Remove PROPERTIES drawer from content
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
          (let ((prop-start (match-beginning 0)))
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (delete-region prop-start (line-beginning-position 2))
              (goto-char (point-max))))))
      (setq content (string-trim (buffer-string))))
    
    ;; Return the extracted data
    (list clean-heading content timestamp property-id properties)))

(defun org-to-denote-parse-timestamp (timestamp)
  "Convert org timestamp to denote identifier format.
TIMESTAMP should be in format YYYY-MM-DD Day HH:MM."
  (when timestamp
    (let ((date-time (parse-time-string timestamp)))
      ;; Format as YYYYMMDDThhmmss
      (format-time-string "%Y%m%dT%H%M%S" 
                          (encode-time (nth 0 date-time) ; seconds
                                      (nth 1 date-time) ; minutes
                                      (nth 2 date-time) ; hour
                                      (nth 3 date-time) ; day
                                      (nth 4 date-time) ; month
                                      (nth 5 date-time)))))) ; year

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
         (heading (nth 0 heading-data))
         (content (nth 1 heading-data))
         (timestamp (nth 2 heading-data))
         (property-id (nth 3 heading-data))
         (tags (org-get-tags nil t))
         ;; Always use org file type
         (file-type 'org)
         ;; Prefer heading timestamp over property ID
         (denote-id (or (when timestamp (org-to-denote-parse-timestamp timestamp))
                         property-id))
         (denote-use-title heading)
         (denote-use-keywords tags)
         (denote-use-file-type file-type)
         (denote-use-date (when denote-id (date-to-time denote-id)))
         (note-path))
    
    ;; Create the denote file using the built-in denote function
    (setq note-path (denote))
    
    ;; Insert the content into the denote file 
    (with-current-buffer (find-file-noselect note-path)
      ;; Move to the end of the file
      (goto-char (point-max))
      
      ;; Insert content
      (insert content)
      (save-buffer))
    
    (message "Created denote file: %s" note-path)
    
    ;; Return the path to the created file
    note-path))

(provide 'org-to-denote)
;;; org-to-denote.el ends here
