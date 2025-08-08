;;; pandoc-transient.el -*- lexical-binding: t -*

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

(require 'transient)
(require 's)
(transient-define-prefix pandoc-transient-menu ()
  "Pandoc menu interface"
  :value '("--from=markdown" "--to=org")

  [:description "Convert selected region (or whole buffer) with pandoc."
   ["Format"
    ("-f" "Input" "--from=" :choices pandoc-transient--list-input-formats)
    ("-t" "Output" "--to=" :choices pandoc-transient--list-output-formats)]
   ["Other options" ("-c" "table of contents" "--toc")]
   ["Command" ("RET" "Convert" pandoc-transient-pandoc-on-region)]
  ]
  )

(defun pandoc-transient-pandoc-on-region ()
  "Convert selected region (or whole buffer) with pandoc using arguments
set in the transient menu.."
  (interactive)
  ;; (prin1 (s-join " " `("pandoc" ,@(transient-args 'pandoc-transient-menu)))))  ;; debug print
  (let* ((command (s-join " " `("pandoc" ,@(transient-args 'pandoc-transient-menu))))
         (beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (buffer-substring-no-properties beg end)))
    (shell-command-on-region beg end command nil t)))

(defun pandoc-transient--list-input-formats ()
  (split-string (shell-command-to-string "pandoc --list-input-formats") "\n" t))

(defun pandoc-transient--list-output-formats ()
  (split-string (shell-command-to-string "pandoc --list-output-formats") "\n" t))
