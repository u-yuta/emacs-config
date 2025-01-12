;;; early-init.el --- Emacs early init -*- lexical-binding: t -*-

;; ライブラリがあったら require するマクロ
(defmacro require-if-exists (library &rest body)
  `(when (locate-library ,(symbol-name library))
     (require ',library) ,@body))


;;; Garbage collection
;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

