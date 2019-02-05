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


;; We want to override the user emacs directory, so it is more
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
       core         ;; Core layer
       ivy          ;; Ivy completion framework
       keybindings  ;; Global keybindings

       :features
       dvorak      ;; Enable dvorak remapping of some keys
       )

;; Scripting commands, can be run straight from command line
(defun tron/compile (&optional layer)
  "Compile el into elc to make them more efficient"
  (interactive "SLayer Name:")
  (message "Compiling layers... (%s)" layer)
  (mapcar 'tron/compile-layer (or (when layer `(,layer)) tron-layers))
  (byte-recompile-file (concat user-emacs-directory "packages.el") t 0))

(defun tron/tangle (&optional layer)
  "Compile layers to make them ready for installation"
  (interactive "SLayer Name:")
  (tron/bootstrap-straight)
  (message "Tangling layers...")
  (mapcar 'tron/tangle-layer (or (when layer `(,layer)) tron-layers)))

(defun tron/install (&optional layer)
  "Install and update packages to latest version"
  (interactive "SLayer Name:")
  (tron/bootstrap-straight)
  (message "Installing layers...")
  (mapcar 'tron/install-layer (or (when layer `(,layer)) tron-layers)))

(defun tron/init ()
  "Init code run only when not in batch"
  (interactive)
  (message "Starting Tron Emacs...")
  ;; If we run in DEBUG, it means the use-package macro was not directly compiled
  ;; when we ran the compile command. Hence, we need to load use-package
  (when (getenv "DEBUG")
    (message "Debug mode")
    (tron/load-package '(use-package bind-key) 'use-package)
    (setq debug-on-error t))

  ;; When loading in debug, we want to register the timing of the different layers
  (if (getenv "DEBUG")
      (mapcar (lambda (layer)
                (with-timer (format "Loading %s" layer) (tron/load-layer layer))) tron-layers)
    (mapcar 'tron/load-layer (or (when (getenv "COREONLY") '(core)) tron-layers)))
  )

;; We run initialization only when non batch
(unless noninteractive (tron/init))

;; POST Initialization

(setq file-name-handler-alist file-name-handler-alist-old
      gc-cons-threshold 800000
      gc-cons-percentage 0.1)
(garbage-collect)
