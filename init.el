;; Init.el starts here
;; Those things are a few tricks to make everything load faster

;; Starting Optimization Tweak
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)
;; Finished optimization tweak

;; We want to overrid the user emacs directory, so it is more
;; portable this way
(setq user-emacs-directory (file-name-directory load-file-name))

;; We don't want to use the load-path directly to avoid load paths
;; so we created a function to load everything absolutely
(defun emacs-path (path)
  "Return a path relative to current folder"
  (concat user-emacs-directory path))

;; Provide a couple of utilities necessary to work with our own
;; packages
(require 'tron-packages (emacs-path "packages"))

(tron! :layers
       test        ;; Feature that is a test
       bam
       boo
       nil

       :features
       p1          ;; Package P1
       p2
       )

(progn
(message "Packages: %s" tron-packages)
(message "Features: %s" tron-features)
(message "Has P1 %s" (tron/has-feature-p 'p1))
(message "Has P3 %s" (tron/has-feature-p 'p3))
)

(defun compile ()
  "Compile layers to make them ready for installation"
  )

(defun install ()
  "Install and update packages to latest version"
  (message "Bootstrapping Straight.el...")
  (tron/bootstrap-straight))
