;;; auth-sourceによる認証情報取得の設定 -*- lexical-binding: t -*-

;; GPG設定
(setq epg-pinentry-mode 'loopback)

;; password-storeの情報をauth-souceで使う
(use-package auth-source-pass
  :ensure nil
  :init (auth-source-pass-enable))

(use-package password-store :ensure t)

;; GPTelで使うAPI-keyの格納用
(defun uy/get-auth-secret (host)
  "Get secret from auth-source for the specified HOST."
  (if-let ((auth-info (car (auth-source-search :host host))))
      (if-let ((secret-fn (plist-get auth-info :secret)))
          (funcall secret-fn)
        (error "No secret found for host: %s" host))
    (error "No auth-source entry found for host: %s" host)))

(defun uy/api-key-deepseek () (uy/get-auth-secret "api.deepseek.com"))

(provide 'setup-auth-source)
