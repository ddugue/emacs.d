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
    (load bootstrap-file nil 'nomessage)))


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
                              (quote ,temp))))
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

;; Utilities to work with layers
(defun tron/layer-file (layer &optional file)
  "Return a layer file based on the layer and the file"
  (let ((layer-name (or (and (symbolp layer) (symbol-name layer)) layer)))
    (concat user-emacs-directory "layers/" layer-name "/" file)))

;; Compile a layer .org files into its .el files
(defun tron/compile-layer (layer)
  "Function to compile a layer org file into the different el files"
  (require 'org)
  (let ((layer-file (tron/layer-file layer "layer.org")))
    (if (not (file-exists-p layer-file))
        (tron/message! "\u2717" :red "Could not compile %s (%s not found)" layer layer-file)
        (let ((inhibit-message t))
          (org-babel-tangle-file layer-file))
          ;; (unless (getenv "DEBUG") (byte-recompile-directory layer nil)))
        (tron/message! "\u2714" :green "Compiled %s" layer))))

;; Install a layer based on its .el or .elc file
(defun tron/install-layer (layer)
  "Function to install a layer"
  (let* ((install-file (tron/layer-file layer "install"))
         (el-file (concat install-file ".el"))
         (elc-file (concat install-file ".elc")))
    (if (not (or (file-exists-p el-file) (file-exists-p elc-file)))
        (tron/message! "\u2717" :red "Could not install %s (%s not found)" layer el-file)
      (let ((inhibit-message t))
        (load install-file))
        (tron/message! "\u2714" :green "Installed %s" layer))))

;; Install a layer based on its .el or .elc file
(defun tron/load-layer (layer)
  "Function to load a layer"
  (let* ((install-file (tron/layer-file layer "config"))
         (el-file (concat install-file ".el"))
         (elc-file (concat install-file ".elc")))
    (if (not (or (file-exists-p el-file) (file-exists-p elc-file)))
        (tron/message! "\u2717" :red "Could not load %s (%s not found)" layer el-file)
      (load install-file))))

(provide 'tron-packages)
