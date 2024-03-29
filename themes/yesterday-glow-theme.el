(deftheme yesterday-glow
  "Theme that emulates the good old looking of old orange terminals")

(let* ((class '((class color) (min-colors 89)))

       ;; Colors reference
       (white "#FFFFFF")
       (orange "#FF8100")
       (light-orange "#E78C45")
       (dark-orange "#994D00")
       (lime "#B9CA4A")
       (teal "#66CCCC")
       (red "#D54E53")
       (yellow "#E7C547")
       (purple "#CC99CC")
       (dark "#0F0900")
       (even-less-dark "#1A0F00")
       (less-dark "#0D0800")
       (lesser-dark "#0A0300")
       (light "#38280B")
       (brown "#A16946")
       (blue "#7AA6DA")
       ;;
       ;; Associate color with their "symbol", for instance a "string" is the "blue" color
       (foreground orange)
       (contrast teal) ;; High contrast foreground
       (alt blue) ;; Low contrast foreground

       (background dark)
       (comment brown)
       (highlight light)
       (bg1 dark)
       (bg2 even-less-dark)
       (bg3 less-dark)
       (bg4 lesser-dark)
       (highlight light)
       (base foreground)
       (cursor base)
       (lnum base)
       (func alt)
       (err red)
       (number red)
       (str contrast)
       (suc lime)
       (const red)
       (border orange)
       (type alt)
       (keyword light-orange)
       (var base)
       (mat yellow)
       (head1 alt)
       (head2 foreground)
       (head3 red)
       (head4 yellow)
       (comp purple)
       (war yellow))

  (custom-theme-set-variables
   'yesterday-glow
   `(ansi-color-names-vector [,background ,red ,lime ,yellow ,blue ,purple ,teal ,foreground]))

  (custom-theme-set-faces
   'yesterday-glow

   ;; Defaults
   `(default ((,class (:foreground ,foreground :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(shadow ((,class (:foreground ,comment))))
   `(success ((,class (:foreground ,suc))))
   `(error ((,class (:foreground ,err))))
   `(warning ((,class (:foreground ,war))))
   `(outline-4 ((,class (:slant normal :foreground ,comment))))
   `(header-line ((,class :background ,bg4)))
   `(line-number ((,class :background ,even-less-dark :foreground ,orange)))
   `(line-number-current-line ((,class :background ,even-less-dark :foreground ,alt)))

   ;; TODO
   `(match ((,class (:background ,light :foreground ,yellow))))
   `(page-break-lines ((,class (:foreground ,orange))))

   ;; Emacs interface
   `(cursor ((,class (:background ,cursor))))
   `(fringe ((,class (:background ,even-less-dark :foreground ,foreground))))
   `(custom-button ((,class :background ,less-dark :foreground ,foreground :box (:line-width 2 :style released-button))))
   `(fill-column-indicator ((,class :foreground ,comment :font "Inconsolata")))

   `(linum ((,class (:foreground ,orange :background ,even-less-dark))))
   `(linum-relative-current-face ((,class (:foreground ,blue))))

   `(border ((,class (:background ,orange))))
   `(vertical-border ((,class (:foreground ,lesser-dark))))

   `(hl-line ((,class (:background ,even-less-dark))))
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))
   `(highlight ((,class (:foreground ,foreground :background ,light))))
   `(minibuffer-prompt ((,class (:inherit bold :foreground ,blue))))
   `(region ((,class (:background ,light))))
   `(secondary-selection ((,class (:background ,less-dark))))

   ;; Mode line
   `(mode-line ((,class (:foreground ,background :background ,orange :box (:color ,orange :line-width 1)))))
   `(mode-line-inactive ((,class (:foreground ,foreground :background ,background  :box (:color ,dark-orange :line-width 1)))))
   `(mode-line-buffer-id ((,class (:inherit bold :foreground ,white))))

   ;; Web mode
   `(web-mode-block-control-face ((t (:foreground ,blue :weight bold))))
   `(web-mode-block-delimiter-face ((t (:foreground ,blue))))
   `(web-mode-block-string-face ((t (:foreground ,str))))
   `(web-mode-html-attr-name-face ((t (:foreground ,orange :weight normal))))
   `(web-mode-html-attr-value-face ((t (:foreground ,lime :weight normal :slant italic))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,foreground))))
   `(web-mode-html-tag-face ((t (:foreground ,foreground :weight bold))))
   `(web-mode-variable-name-face ((t (:foreground ,red))))

   ;; Font-lock
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func))))
   `(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword))))
   `(font-lock-builtin-face ((,class (:foreground ,mat))))
   `(font-lock-negation-char-face ((,class (:foreground ,number))))
   `(font-lock-preprocessor-face ((,class (:foreground ,alt))))
   `(font-lock-reference-face ((,class (:foreground ,red))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,war :background ,background))))

   ;; Eglot
   `(eglot-highlight-symbol-face ((,class (:foreground ,yellow :underline t))))
   ;; Selectrum
   `(selectrum-primary-highlight ((,class (:foreground ,yellow :underline t))))
   ;; Ivy
   `(ivy-current-match ((,class (:background ,highlight :inherit bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow :underline t))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,yellow :underline t))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,yellow :underline t))))
   `(ivy-remote ((,class (:foreground ,teal))))
   `(ivy-modified-buffer ((,class (:foreground ,teal))))
   `(ivy-virtual ((,class (:inherit italic))))

   ;; Show Parenthesis
   `(show-paren-match ((,class (:foreground ,purple :inherit bold))))
   `(show-paren-mismatch ((,class (:foreground ,err :inherit bold))))

   ;; Flycheck
   `(flycheck-warning ((,class (:underline (:style wave :color ,war)))))
   `(flycheck-error ((,class (:underline (:style wave :color ,err)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,blue)))))
   `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
   `(flycheck-fringe-info ((,class (:foreground ,blue :inherit bold))))
   `(flycheck-error-list-error ((,class (:foreground ,err :inherit bold))))
   `(flycheck-error-list-warning ((,class (:foreground ,light-orange :inherit bold))))
   `(flycheck-error-list-info ((,class (:foreground ,blue :inherit bold))))
   `(flycheck-error-list-id-with-explainer ((,class (:foreground ,blue :box nil :inherit bold))))

   ;; Highlight indentation mode
   `(highlight-indentation-face ((,class (:background ,even-less-dark))))
   `(highlight-indentation-current-column-face ((,class (:background ,light))))

   ;; EWW
   `(eww-form-text ((,class (:background ,background :foreground ,light-orange :box 1))))

   ;; Terminal
   `(term-color-yellow ((,class (:foreground ,yellow))))
   `(term-color-green ((,class (:foreground ,lime))))
   `(term-color-red ((,class (:foreground ,red))))
   `(term-color-blue ((,class (:foreground ,blue))))
   `(term-color-cyan ((,class (:foreground ,teal))))
   `(term-color-magenta ((,class (:foreground ,purple))))
   ;; Org
   `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
   `(org-agenda-date ((,class (:foreground ,var))))
   `(org-agenda-date-today ((,class (:foreground ,keyword :inherit bold ))))
   `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
   `(org-agenda-done ((,class (:foreground ,suc ))))
   `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
   `(org-block ((,class (:background ,lesser-dark))))
   `(org-block-background ((,class (:background ,lesser-dark))))
   `(org-block-begin-line ((,class (:background ,lesser-dark :foreground ,dark-orange))))
   `(org-block-end-line ((,class (:background ,lesser-dark :foreground ,dark-orange))))
   `(org-clock-overlay ((,class (:foreground ,comp))))
   `(org-code ((,class (:foreground ,teal))))
   `(org-column ((,class (:background ,highlight))))
   `(org-column-title ((,class (:background ,highlight))))
   `(org-date ((,class (:underline t :foreground ,var))))
   `(org-date-selected ((,class (:background ,func :foreground ,bg1))))
   `(org-document-info-keyword ((,class (:foreground ,comment))))
   `(org-document-title ((,class (:foreground ,func :inherit bold  :underline t))))
   `(org-done ((,class (:foreground ,suc :inherit bold :background ,even-less-dark))))
   `(org-ellipsis ((,class (:foreground ,keyword))))
   `(org-footnote  ((,class (:underline t :foreground ,base))))
   `(org-hide ((,class (:foreground ,base))))
   `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
   `(org-level-1 ((,class (:inherit bold :foreground ,head1))))
   `(org-level-2 ((,class (:inherit bold :foreground ,head2))))
   `(org-level-3 ((,class (:bold nil :foreground ,head3))))
   `(org-level-4 ((,class (:bold nil :foreground ,head4))))
   `(org-level-5 ((,class (:bold nil :foreground ,head1))))
   `(org-level-6 ((,class (:bold nil :foreground ,head2))))
   `(org-level-7 ((,class (:bold nil :foreground ,head3))))
   `(org-level-8 ((,class (:bold nil :foreground ,head4))))
   `(org-link ((,class (:underline t :foreground ,blue))))
   `(org-meta-line ((,class (:foreground ,comment))))
   `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
   `(org-priority ((,class (:foreground ,war :inherit bold))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-scheduled ((,class (:foreground ,comp))))
   `(org-scheduled-today ((,class (:foreground ,func ))))
   `(org-sexp-date ((,class (:foreground ,base))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-table ((,class (:foreground ,base :background ,even-less-dark))))
   `(org-time-grid ((,class (:foreground ,str))))
   `(org-todo ((,class (:foreground ,war :inherit bold))))
   `(org-verbatim ((,class (:foreground ,keyword))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-warning ((,class (:foreground ,err))))
   ))

(provide-theme 'yesterday-glow)
