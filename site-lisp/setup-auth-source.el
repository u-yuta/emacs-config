;;; setup-auth-source.el --- Authentication and secret management configurations -*- lexical-binding: t -*

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


;; GPG設定
(setopt epg-pinentry-mode 'loopback)

;; password-storeの情報をauth-souceで使う
(use-package auth-source-pass
  :ensure nil
  :init (auth-source-pass-enable))

(use-package password-store :ensure t)

;; GPTelで使うAPI-keyの格納用
;; password store のデータの更新が反映されない場合は auth-source-forget-all-cached を実行する
(defun uy/get-auth-secret (host)
  "Get secret from auth-source for the specified HOST."
  (if-let ((auth-info (car (auth-source-search :host host))))
      (if-let ((secret-fn (plist-get auth-info :secret)))
          (funcall secret-fn)
        (error "No secret found for host: %s" host))
    (error "No auth-source entry found for host: %s" host)))

(provide 'setup-auth-source)
