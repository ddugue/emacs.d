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

;; Utility to determine wether a certain feature is enabled or not
(defun tron/has-feature-p (feature)
  "Return non-nil if feature is installed"
  (memq feature tron-features))

;; Utilities to work with layers
(defun tron/layer-file (layer file)
  "Return a layer file based on the layer and the file"
  (let ((layer-name (or (and (symbolp layer) (symbol-name layer)) layer)))
    (concat user-emacs-directory "layers/" layer-name "/" file)))

;; Compile a layer .org files into its .el files
(defun tron/compile-layer (layer)
  "Function to compile a layer org file into the different el files"
  (require 'org)
  (let ((inhibit-message t))
    (org-babel-tangle-file (tron/layer-file layer "layer.org")))
  (message "[\e[32m\u2714\e[0m] Compiled %s" layer))

(defun tron/install-layer (layer)
  "Function to install a layer"
  (let ((inhibit-message t))
    (load (tron/layer-file layer "install.el")))
  (message "[\e[32m\u2714\e[0m] Installed %s" layer))

(provide 'tron-packages)
