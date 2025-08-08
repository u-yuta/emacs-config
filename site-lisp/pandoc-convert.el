;;; pandoc-convert.el -*- lexical-binding: t -*

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

(defun uy/pandoc-convert-region (input-format output-format)
  "Convert selected region (or whole buffer) with pandoc.
Prompt for input and output formats, then replace text with converted result."
  (interactive "sInput format: \nsOutput format: ")
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end)))
    (shell-command-on-region
     beg end
     (format "pandoc -f %s -t %s" input-format output-format)
     nil t)))

(defun uy/pandoc-convert-region-to-org (input-format)
  "Convert selected region (or whole buffer) to Org format with pandoc.
Prompt for input format, then replace text with converted result."
  (interactive "sInput format: ")
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end)))
    (shell-command-on-region
     beg end
     (format "pandoc -f %s -t org" input-format)
     nil t)))

(defun uy/pandoc-convert-region-to-markdown (input-format)
  "Convert selected region (or whole buffer) to Markdown format with pandoc.
Prompt for input format, then replace text with converted result."
  (interactive "sInput format: ")
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end)))
    (shell-command-on-region
     beg end
     (format "pandoc -f %s -t markdown" input-format)
     nil t)))
