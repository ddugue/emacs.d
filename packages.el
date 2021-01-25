;;; -*- lexical-binding: t -*-
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
  (straight-use-package 'use-package)
  ;; (advice-add 'use-package-handler/:commands :override 'my/use-package-handler/:commands)
  ;; We override the use-package command handler in order to support
  ;; Straight package AND have deferred load too.
  )
  ;; (setq straight-use-package-by-default t))

(defun my/use-package-handler/:commands (name _keyword arg rest state)
  (use-package-concat
   ;; Since we deferring load, establish any necessary autoloads, and also
   ;; keep the byte-compiler happy.
   (let* ((name-string (use-package-as-string name))
          (full-name (expand-file-name (concat "straight/build/" name-string "/" name-string) user-emacs-directory)))
     (cl-mapcan
      #'(lambda (command)
          (when (symbolp command)
            (append
             (unless (plist-get state :demand)
               `((unless (fboundp ',command)
                   (autoload #',command ,(if (file-exists-p (concat full-name ".el")) full-name name-string) nil t))))
             (when (bound-and-true-p byte-compile-current-file)
               `((eval-when-compile
                   (declare-function ,command ,name-string)))))))
      (delete-dups arg)))
   (use-package-process-keywords name rest state)))
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

(defun tron/has-layer-p (layer)
  "Return non-nil if layer is installed"
  (memq layer tron-layers))

;; Utilities to work with layers
(defun tron/layer-folder (layer)
  (let ((layer-name (or (and (symbolp layer) (symbol-name layer)) layer)))
    (concat user-emacs-directory "layers/" layer-name)))

(defun tron/layer-file (layer &optional file)
  "Return a layer file based on the layer and the file"
  (let ((layer-name (or (and (symbolp layer) (symbol-name layer)) layer)))
    (concat (tron/layer-folder layer) "/" file)))

(defun tron/package-folder (&optional package)
  "Return a package folder"
  (concat user-emacs-directory "straight/build/" (when package (symbol-name package))))

(defun add-tangle-headers ()
  (cond
   ((string= (file-name-extension (buffer-file-name)) "el")
    (goto-char (point-min))
    (insert ";;; -*- lexical-binding: t -*-\n"))
   (t
    nil))
  (save-buffer))

(defvar current-package nil)
(defun add-tangle-footer ()
  (cond
   ((string= (tron/get-filename (buffer-file-name)) "config.el")
    (goto-char (point-max))
    (insert (concat "\n(provide 'tron/" current-package ")\n")))
   (t
    nil))
  (save-buffer))

;; Compile a layer .org files into its .el files
(defun tron/tangle-layer (layer)
  "Function to tangle a layer org file into the different el files"
  (require 'org)
  (add-hook 'org-babel-post-tangle-hook 'add-tangle-headers)
  (add-hook 'org-babel-post-tangle-hook 'add-tangle-footer)
  (let ((layer-file (tron/layer-file layer "layer.org")))
    (setq current-package (symbol-name layer))
    (if (not (file-exists-p layer-file))
        (tron/message! "\u2717" :red "Could not tangle %s (%s not found)" layer layer-file)
      (let ((inhibit-message (not (getenv "DEBUG"))))

          (org-babel-tangle-file layer-file))
      (tron/message! "\u2714" :green "Tangled %s layer" layer))))

(defun tron/compile-layer (layer)
  "Function to natively compile el files"
  (defun warn-highjack (oldfun type message &optional level buffer-name)
    (if (equal level :error)
        (progn
          (let ((inhibit-message nil))
            (tron/message! "\u2717" :red "Could not compile %s (%s)" layer message))
          (kill-emacs 1))
      (funcall oldfun type message level buffer-name)))
  (advice-add #'display-warning :around #'warn-highjack)
  (let ((layer-file (tron/layer-file layer "layer.org")))
    (if (not (file-exists-p layer-file))
        (tron/message! "\u2717" :red "Could not compile %s (%s not found)" layer layer-file)
      (let ((inhibit-message (not (getenv "DEBUG")))
            (file (tron/layer-file layer "config")))
        (condition-case err
            (progn
              (native-compile (concat file ".el") (concat file ".eln"))
              (require `,(intern (concat "tron/" (symbol-name layer))) (concat file ".eln"))
              (tron/message! "\u2714" :green "Compiled %s layer" layer))
          (t (tron/message! "\u2717" :red "Could not compile %s (%s)" layer err) (kill-emacs 1))
          )
      ))))


;; Install a layer based on its .el or .elc file
(defun tron/install-layer (layer)
  "Function to install a layer"
  (let* ((install-file (tron/layer-file layer "install"))
         (el-file (concat install-file ".el"))
         (elc-file (concat install-file ".elc"))
         )

    (if (not (or (file-exists-p el-file) (file-exists-p elc-file)))
        (tron/message! "\u2717" :red "Could not install %s (%s not found)" layer el-file)
      (let ((inhibit-message (not (getenv "DEBUG"))))
        (load install-file))
        (tron/message! "\u2714" :green "Installed %s layer" layer))))

;; Install a layer based on its .el or .elc file
(defun tron/load-layer (layer)
  "Load a layer"
  (let ((file (concat (tron/layer-file layer "config") ".el" (unless (getenv "DEBUG") "n"))))
    (when (getenv "DEBUG") (message "Loading %s..." file))
    (if (not (file-exists-p file))
        (tron/message! "\u2717" :red "Could not load %s (%s not found)" layer file)
      (require `,(intern (concat "tron/" (symbol-name layer))) file)
  )))

;; Utilities to work with packages
(defun tron/get-filename (str)
  "Return the filename of a filepath"
  (let ((split-path (split-string str "/")))
    (car (last split-path))))

(defun tron/compile-libraries (do-not-compile)
  (native-compile (concat user-emacs-directory "themes/yesterday-glow-theme.el") (concat user-emacs-directory "themes/yesterday-glow-theme.eln"))
  ;; (native-compile (concat user-emacs-directory "config.el") (concat user-emacs-directory "config.eln"))
  (native-compile-async (tron/package-folder) 'recursively nil
                        (lambda (f)
                          (let* ((folder (string-remove-prefix (tron/package-folder) f))
                                 (filename (tron/get-filename folder)))
                            (if
                                (or
                                 (cl-some `(lambda (ex) (string-match-p ex ,filename)) do-not-compile)
                                 (string= ".dirs-locals.el" filename)
                                 (string-suffix-p "autoloads.el" filename))
                                (progn
                                  nil)
                              t)
                          ))))


(provide 'tron-packages)
