;;; setup-dape.el --- Dape: Debug Adapter Protocol for Emacs  -*- lexical-binding: t -*

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


;; Dape - Debug Adapter Protocol for Emacs
(use-package dape
  :ensure t
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)
  :config
  ;; Info buffers to the right
  (setq dape-buffer-window-arrangement 'right)

  (add-to-list 'dape-configs
  	           `(debugpy-uv-python
  	             modes (python-mode python-ts-mode)
  	             command "uv"
  	             command-args ["run" "python" "-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport]
  	             port :autoport
  	             :type "python"
  	             :request "launch"
  	             :args ["--app" "src" "run" "--no-debugger" "--no-reload"]
  	             :console "integratedTerminal"
  	             :showReturnValue t
  	             :justMyCode nil
  	             :cwd dape-cwd-function
                 :program dape-buffer-default))
  )
