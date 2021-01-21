;;; -*- lexical-binding: t -*-
;; Init.el starts here
;; Those things are a few tricks to make everything load faster
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

(setq user-emacs-directory (file-name-directory load-file-name))
(setq initial-frame-alist '((name . "editor") (minibuffer . nil)))
(add-to-list 'default-frame-alist '(minibuffer . nil))


;; We want to overrid the user emacs directory, so it is more
;; portable this way

;; We don't want to use the load-path directly to avoid load paths
;; so we created a function to load everything absolutely
(defun emacs-path (path)
  "Return a path relative to current folder"
  (concat user-emacs-directory path))

;; Provide a couple of utilities necessary to work with our own
;; packages
(require 'tron-packages (emacs-path "packages"))

(tron! :layers
       core       ;; Core layer
       keybindings ;; Group all keybindings in one place
       windows ;; Windows and frame management
       scratch ;; Custom scratch buffer management
       ;; selectrum  ;; Completion framework
       ivy ;; Other completion framework
       project

       ;; Programming languages
       elisp

       :features
       dvorak      ;; Enable dvorak remapping of some keys
       )

;; Scripting commands, can be run straight from command line
(defun tron/install (&optional layer)
  "Install and update packages to latest version"
  (interactive "SLayer Name:")
  (defvar straight-disable-native-compilation t)
  (tron/bootstrap-straight)
  (require 'package)
  (message "Installing layers...")
  (setq comp-async-report-warnings-errors (getenv "DEBUG"))
  (setq straight-disable-byte-compilation t)
  (mapcar 'tron/tangle-layer (or (when layer `(,layer)) tron-layers))
  (mapcar 'tron/install-layer (or (when layer `(,layer)) (reverse tron-layers)))
  (mapcar 'tron/compile-layer (or (when layer `(,layer)) (reverse tron-layers)))
  ;; Compile straight libraries

  ;; block until native compilation has finished
  (tron/compile-libraries)
  (unless (getenv "DEBUG")
    (setq warning-minimum-level :error))

  (while (or comp-files-queue
             (> (comp-async-runnings) 0))
    (sleep-for 1))
  (when (and (getenv "DEBUG") (get-buffer "*Async-native-compile-log*"))
    (with-current-buffer "*Async-native-compile-log*"
      (message (buffer-string))))
  (when (and (getenv "DEBUG") (get-buffer "*Warnings*"))
    (with-current-buffer "*Warnings*"
      (message "Warnings that occured: ")
      (message (buffer-string))))
  (tron/message! "\u2714" :green "Libraries compiled"))

(defun tron/init ()
  "Init code run only when not in batch"
  (setq tron--start-time (current-time))
  (message "Starting Tron Emacs...")
  ;; If we run in DEBUG, it means the use-package macro was not directly compiled
  ;; when we ran the compile command. Hence, we need to load use-package
  ;; (tron/bootstrap-straight)
  (setq comp-async-report-warnings-errors nil)
  (when (getenv "DEBUG")
    (add-to-list 'load-path "/home/ddugue/new-emacs/straight/build/use-package")
    (add-to-list 'load-path "/home/ddugue/new-emacs/straight/build/bind-key")
    (require 'use-package)
    (require 'bind-key))
  (mapcar 'tron/load-layer (or (when (getenv "COREONLY") '(core)) (reverse tron-layers)))
  (unless (getenv "NOTHEME")
    (setq custom-theme-directory (emacs-path "themes"))
    (setq custom-safe-themes t)
    (load-theme 'yesterday-glow t))
  (pop-to-buffer "*Messages*")
  (select-frame-by-name "editor")
  (message "Tron Emacs Loaded in %s (Init %s)."
           (emacs-init-time)
           (format-time-string "%S.%3N seconds" (time-subtract nil tron--start-time) t))
  )

(unless noninteractive (tron/init))
