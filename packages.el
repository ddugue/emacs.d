;; File to work with packages

;; Script to bootstrap straight.el
;; You can find more info here:
;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(defun tron/bootstrap-straight ()
  "Function to install straight.el"
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package))


;; Feature setter
;; Set the different features and global package list
;; Doesn't do anything by itself
(defmacro tron/features (&rest args)
  "Features and packages that are enabled"
  (let ((result))
    (while args
      (let ((mode (car args))
            (temp)
            (current))
        (while (and args (not (memq current '(:features :layers))))
          (when current (add-to-list 'temp current))
          (setq args (cdr args))
          (setq current (car args)))

        (when temp
          (add-to-list 'result
                       `(set ',(intern (concat "tron-" (substring (symbol-name mode) 1)))
                              (quote ,(reverse temp)))))
        (setq mode current)
        (setq current nil)))
    `(progn ,@result)))

(defalias 'tron! (quote tron/features))

(defmacro tron/message! (unicode color msg &rest args)
  "Small utility to create a message with a unicode colored at start"
  (let ((color-code (cond ((eql :red color) "\e[31m")
                          ((eql :green color) "\e[32m"))))
    `(message ,(concat "["
                       (when noninteractive color-code)
                       unicode
                       (when noninteractive "\e[0m")
                       "] "
                       msg) ,@args)))

;; Utility to determine wether a certain feature is enabled or not
(defun tron/has-feature-p (feature)
  "Return non-nil if feature is installed"
  (memq feature tron-features))

(defun tron/has-layer-p (layer)
  "Return non-nil if layer is installed"
  (memq layer tron-layers))

;; Utilities to work with layers
(defun tron/layer-file (layer &optional file)
  "Return a layer file based on the layer and the file"
  (let ((layer-name (or (and (symbolp layer) (symbol-name layer)) layer)))
    (concat user-emacs-directory "layers/" layer-name "/" file)))

(defun tron/package-folder (package)
  "Return a package folder"
  (concat user-emacs-directory "straight/build/" (symbol-name package)))

;; Compile a layer .org files into its .el files
(defun tron/tangle-layer (layer)
  "Function to tangle a layer org file into the different el files"
  (require 'org)

  ;; Verbose flag enabled
  (when (getenv "VERBOSE") (message "Tangling %s..." layer))
  (let ((layer-file (tron/layer-file layer "layer.org")))
    (if (not (file-exists-p layer-file))
        (tron/message! "\u2717" :red "Could not tangle %s (%s not found)" layer layer-file)

      (condition-case err
        (let ((inhibit-message (not (getenv "VERBOSE"))))
          (org-babel-tangle-file layer-file))
        ('error
         (tron/message! "\u2717" :red "Could not tangle %s (%s not found)" layer (error-message-string err))
        (signal (car err) (cdr err)))
        )

        (tron/message! "\u2714" :green "Tangled %s layer" layer)))
  )

(defun tron/compile-layer (layer)
  "Function to compile a layer org file into the different el files"
  (tron/load-package '(use-package bind-key) 'use-package)
  (byte-recompile-directory (tron/layer-file layer) 0))


;; Install a layer based on its .el or .elc file
(defun tron/install-layer (layer)
  "Function to install a layer"
  (let* ((install-file (tron/layer-file layer "install"))
         (el-file (concat install-file ".el"))
         (elc-file (concat install-file ".elc")))
    (if (not (or (file-exists-p el-file) (file-exists-p elc-file)))
        (tron/message! "\u2717" :red "Could not install %s (%s not found)" layer el-file)
      (let ((inhibit-message (not (getenv "DEBUG"))))
        (load install-file))
        (tron/message! "\u2714" :green "Installed %s layer" layer))))

;; Install a layer based on its .el or .elc file
(defun tron/load-layer (layer)
  "Function to load a layer"
  (let* ((install-file (tron/layer-file layer "config"))
         (el-file (concat install-file ".el"))
         (elc-file (concat install-file ".elc")))
    (if (not (or (file-exists-p el-file) (file-exists-p elc-file)))
        (tron/message! "\u2717" :red "Could not load %s (%s not found)" layer el-file)
      (load (if (getenv "DEBUG") el-file install-file)))))

;; Utilities to work with packages
(defun tron/load-package (dependencies package)
  (let ((load-path (append load-path (mapcar 'tron/package-folder dependencies))))
    (message "Load path is %s" load-path)
    (require package)))

(provide 'tron-packages)
