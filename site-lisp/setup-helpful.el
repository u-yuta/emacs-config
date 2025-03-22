;;; setup-helpful.el --- Enhance Help buffers using Helpful -*- lexical-binding: t -*

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

(use-package helpful
  :ensure t
  :vc (:url "https://github.com/Wilfred/helpful" :branch "main" :rev :newest)
  :config
  (global-set-key (kbd "C-c h f") #'helpful-callable)
  (global-set-key (kbd "C-c h k") #'helpful-key)
  (global-set-key (kbd "C-c h v") #'helpful-variable)
  (global-set-key (kbd "C-c h x") #'helpful-command)

  ;; Notes:
  ;; - go-back/forward does not work in Helpful mode, see Github issue #250
  )

(provide 'setup-helpful)
