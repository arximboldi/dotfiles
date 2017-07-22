;;; emacs theme -- Juan Pedro Bolívar Puente
;;; ported from tango-2-theme

(deftheme arximboldi-tangoish
  "arximboldi-tangoish color theme")

(defvar numix-colors-alist
  '(("numix-dark-bg-color" . "#2d2d2d")
    ("numix-dark-fg-color" . "#777")
    ("numix-select-color" . "#F0544C")
    ("numix-bg-color" . "#444444")
    ("numix-fg-color" . "#999")
    ("numix-light-fg-color" . "#eeeeee")))

;; https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el
(defmacro numix-with-color-variables (&rest body)
  "`let' bind all colors defined in `numix-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   numix-colors-alist))
     ,@body))

(numix-with-color-variables
 (custom-theme-set-faces
  'arximboldi-tangoish
  `(cursor ((t (:foreground "black" :background "yellow"))))
  `(fringe ((t (:background ,numix-dark-bg-color :foreground ,numix-dark-fg-color))))
  `(highlight ((t (:background ,numix-bg-color))))
  `(region ((t (:background "black"))))

  `(font-lock-builtin-face ((t (:foreground "#729fcf"))))
  `(font-lock-comment-face ((t (:foreground "#888a85"))))
  `(font-lock-constant-face ((t (:foreground "#ad7fa8"))))
  `(font-lock-function-name-face ((t (:foreground "#729fcf"))))
  `(font-lock-keyword-face ((t (:foreground "#fcaf3e"))))
  `(font-lock-string-face ((t (:foreground "#73d216"))))
  `(font-lock-type-face ((t (:foreground "#c17d11"))))
  `(font-lock-variable-name-face ((t (:foreground "#fce94f"))))
  `(font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))
  `(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
  `(lazy-highlight ((t (:background "#e9b96e" :foreground "#2e3436"))))
  `(link ((t (:foreground "#729fcf"))))
  `(link-visited ((t (:foreground "#ad7fa8"))))

  `(flyspell-duplicate ((t (:foreground "#fcaf3e"))))
  `(flyspell-incorrect ((t (:foreground "#cc0000"))))

  `(org-date ((t (:foreground "LightSteelBlue" :underline t))))
  `(org-hide ((t (:foreground "#2e3436"))))
  `(org-todo ((t (:inherit font-lock-keyword-face :bold t))))
  `(org-level-1 ((t (:inherit font-lock-function-name-face))))
  `(org-level-2 ((t (:inherit font-lock-variable-name-face))))
  `(org-level-3 ((t (:inherit font-lock-keyword-face))))
  `(org-level-4 ((t (:inherit font-lock-string-face))))
  `(org-level-5 ((t (:inherit font-lock-constant-face))))

  `(comint-highlight-input ((t (:italic t :bold t))))
  `(comint-highlight-prompt ((t (:foreground "#8ae234"))))
  `(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
  `(isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
  `(paren-face-match ((t (:inherit show-paren-match-face))))
  `(paren-face-match-light ((t (:inherit show-paren-match-face))))
  `(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
  `(persp-selected-face ((t (:foreground "#729fcf"))))
  `(show-paren-match-face ((t (:background "#729fcf" :foreground "#eeeeec"))))
  `(show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))

  ;; Customized by me
  `(custom-button ((t (:background "#333" :foreground "#fff" :box (:line-width 2 :style released-button)))))
  `(custom-button-mouse ((t (:background "#555" :foreground "#fff" :box (:line-width 2 :style released-button)))))
  `(diff-added ((t (:inherit diff-changed :foreground "green yellow"))))
  `(diff-file-header ((t (:inverse-video t :weight bold))))
  `(diff-hunk-header ((t (:inherit (highlight diff-header)))))
  `(diff-removed ((t (:inherit diff-changed :foreground "tomato"))))
  `(magit-blame-header ((t (:inherit magit-section-title :background ,numix-bg-color))))
  `(magit-item-highlight ((t (:inherit secondary-selection :background ,numix-bg-color))))
  `(magit-tag ((t (:background "#5d3703" :foreground "LemonChiffon1"))))
  `(minibuffer-prompt ((t (:foreground ,numix-select-color))))
  `(mode-line ((t (:background ,numix-select-color :foreground "white"))))
  `(mode-line-emphasis ((t (:foreground "black" :weight bold))))
  `(mode-line-inactive ((t (:inherit mode-line :background ,numix-bg-color :foreground ,numix-fg-color :weight light))))
  `(scroll-bar ((t nil)))
  `(vertical-border ((((type x tty)) (:inherit mode-line-inactive :foreground ,numix-bg-color))))
  `(comint-highlight-prompt ((t nil)))
  `(jabber-activity-face ((t (:foreground "lime green" :weight bold))))
  `(jabber-chat-prompt-foreign ((t (:foreground "light coral" :weight bold))))
  `(jabber-chat-prompt-local ((t (:foreground "cornflower blue" :weight bold))))
  `(jabber-rare-time-face ((t (:foreground "yellow green" :underline t))))
  `(jabber-roster-user-online ((t (:foreground "deep sky blue" :slant normal :weight bold))))
  `(jabber-activity-personal-face ((t (:foreground "royal blue" :weight bold))))
  `(jabber-roster-user-away ((t (:foreground "chartreuse" :slant italic :weight normal))))))

(provide-theme 'arximboldi-tangoish)
