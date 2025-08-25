;;; docdb.el --- make filelists for docdb -*- lexical-binding: t -*

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

(require 'cl-lib)

(defun docdb-export-resolved-links-to-jsonl (input-org-file output-jsonl-file)
  "Resolve links from an Org file and write them into a JSONL file."
  (interactive "fChoose Org file to process: \nFSave .jsonl to file: ")
  (let* ((resolved-paths (docdb-resolve-and-filter-links-in-file input-org-file))
         (all-paths (cons input-org-file resolved-paths)) )
    (docdb-write-paths-to-jsonl all-paths output-jsonl-file)
    (message "Successfully wrote %d paths to %s"
             (length resolved-paths)
             output-jsonl-file)))

(defun docdb-write-paths-to-jsonl (path-list output-file)
  "Write the list of file paths PATH-LIST to the specified OUTPUT-FILE in .jsonl (JSON Lines) format."
  (with-temp-file output-file
    (let ((buffer-file-coding-system 'utf-8-unix))  ;; Prevent a prompt asking for the coding system
      (dolist (path path-list)
        (let ((json-object `((path . ,path))))
          (insert (json-serialize json-object))
          (newline))))))

(defun docdb-resolve-and-filter-links-in-file (file)
  "Extract links from an Org file and return a list of file paths for the links
 that resolve to an existing file."
  (cl-remove-duplicates 
   (let ((raw-links (docdb-org-extract-links-from-file file)))
     (cl-loop for link in raw-links
              for path = (docdb-resolve-link-to-filepath link)
              when path collect path))
   :test #'equal))


(defun docdb-resolve-link-to-filepath (link)
  "Convert a link string to a file path."
  (cond
   ((string-prefix-p "denote:" link)
    (when-let* ((id-part (string-remove-prefix "denote:" link))
                (id      (car (split-string id-part "::"))))
      (denote-get-path-by-id id)))
   ((string-prefix-p "id:" link)
    (when-let* ((id     (string-remove-prefix "id:" link))
                (marker (org-id-find id t))
                (buffer (marker-buffer marker)))
      (buffer-file-name buffer)))
   ((string-prefix-p "file:" link)
    (let ((path-part (string-remove-prefix "file:" link)))
      (car (split-string path-part "::"))))
   (t nil)))

(defun docdb-org-extract-links-from-file (file)
  "Return a list of all links extracted from an Org file."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((links '()))
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (push (org-element-property :raw-link link) links)
          nil))
      (nreverse links))))

(provide 'docdb)
