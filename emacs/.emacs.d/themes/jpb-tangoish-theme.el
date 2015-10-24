;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;
;; Ported from tango-2-theme
;;

(deftheme jpb-tangoish
  "jpb-tangoish color theme")

(custom-theme-set-faces
 'jpb-tangoish
 '(default ((t (:background "#121212" :foreground "#eeeeec" :height 150 :family "Inconsolata"))))
 '(cursor ((t (:foreground "black" :background "yellow"))))
 '(fringe ((t (:background "#121212" :foreground "#888"))))
 '(highlight ((t (:background "#202020"))))
 '(region ((t (:background "black"))))

 ;;'(cursor ((t (:foreground "#888888"))))
 ;;'(region ((t (:background "#555753"))))
 ;;'(highlight ((t (:background "#444444"))))
 ;;'(modeline ((t (:background "#2e3436" :foreground "#eeeeec"))))
 ;;'(modeline-inactive ((t (:background "#111111" :foreground "#cccddd"))))
 '(fringe ((t (:background "#111111"))))
 ;;'(minibuffer-prompt ((t (:foreground "#729fcf"))))
 '(font-lock-builtin-face ((t (:foreground "#729fcf"))))
 '(font-lock-comment-face ((t (:foreground "#888a85"))))
 ;; '(font-lock-label-face ((t (:foreground "#888a85"))))
 ;; '(font-lock-label-face ((t (:foreground "#888a85"))))
 '(font-lock-constant-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-function-name-face ((t (:foreground "#729fcf"))))
 '(font-lock-keyword-face ((t (:foreground "#fcaf3e"))))
 '(font-lock-string-face ((t (:foreground "#73d216"))))
 '(font-lock-type-face ((t (:foreground "#c17d11"))))
 '(font-lock-variable-name-face ((t (:foreground "#fce94f"))))
 '(font-lock-warning-face ((t (:bold t :foreground "#cc0000"))))
 '(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
 '(lazy-highlight ((t (:background "#e9b96e" :foreground "#2e3436"))))
 '(link ((t (:foreground "#729fcf"))))
 '(link-visited ((t (:foreground "#ad7fa8"))))

 '(flyspell-duplicate ((t (:foreground "#fcaf3e"))))
 '(flyspell-incorrect ((t (:foreground "#cc0000"))))

 '(org-date ((t (:foreground "LightSteelBlue" :underline t))))
 '(org-hide ((t (:foreground "#2e3436"))))
 '(org-todo ((t (:inherit font-lock-keyword-face :bold t))))
 '(org-level-1 ((t (:inherit font-lock-function-name-face))))
 '(org-level-2 ((t (:inherit font-lock-variable-name-face))))
 '(org-level-3 ((t (:inherit font-lock-keyword-face))))
 '(org-level-4 ((t (:inherit font-lock-string-face))))
 '(org-level-5 ((t (:inherit font-lock-constant-face))))

 '(comint-highlight-input ((t (:italic t :bold t))))
 '(comint-highlight-prompt ((t (:foreground "#8ae234"))))
 '(isearch ((t (:background "#f57900" :foreground "#2e3436"))))
 '(isearch-lazy-highlight-face ((t (:foreground "#2e3436" :background "#e9b96e"))))
 '(paren-face-match ((t (:inherit show-paren-match-face))))
 '(paren-face-match-light ((t (:inherit show-paren-match-face))))
 '(paren-face-mismatch ((t (:inherit show-paren-mismatch-face))))
 '(persp-selected-face ((t (:foreground "#729fcf"))))
 '(show-paren-match-face ((t (:background "#729fcf" :foreground "#eeeeec"))))
 '(show-paren-mismatch-face ((t (:background "#ad7fa8" :foreground "#2e3436"))))

 ;; CUSTOMIZED BY JPB

 '(custom-button ((t (:background "#333" :foreground "#fff" :box (:line-width 2 :style released-button)))))
 '(custom-button-mouse ((t (:background "#555" :foreground "#fff" :box (:line-width 2 :style released-button)))))
 '(diff-added ((t (:inherit diff-changed :foreground "green yellow"))))
 '(diff-file-header ((t (:inverse-video t :weight bold))))
 '(diff-hunk-header ((t (:inherit (highlight diff-header)))))
 '(diff-removed ((t (:inherit diff-changed :foreground "tomato"))))
 ;; '(ido-first-match ((t (:foreground "#D64937" :weight bold))))
 ;; '(ido-only-match ((t (:foreground "lawn green"))))
 ;; '(ido-subdir ((t (:foreground "#729FCF"))))
 '(magit-blame-header ((t (:inherit magit-section-title :background "#202020"))))
 '(magit-item-highlight ((t (:inherit secondary-selection :background "#202020"))))
 '(magit-tag ((t (:background "#5d3703" :foreground "LemonChiffon1"))))
 '(minibuffer-prompt ((t (:foreground "#d64937"))))
 '(mode-line ((t (:background "#d64937" :foreground "black"))))
 '(mode-line-emphasis ((t (:foreground "black" :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#2d2d2d" :foreground "#999" :weight light))))
 '(scroll-bar ((t nil)))
 '(vertical-border ((((type x tty)) (:inherit mode-line-inactive :foreground "#222"))))
 '(comint-highlight-prompt ((t nil)))

 '(jabber-activity-face ((t (:foreground "lime green" :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:foreground "light coral" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "cornflower blue" :weight bold))))
 '(jabber-rare-time-face ((t (:foreground "yellow green" :underline t))))
 '(jabber-roster-user-online ((t (:foreground "deep sky blue" :slant normal :weight bold))))
 '(jabber-activity-personal-face ((t (:foreground "royal blue" :weight bold)))))

(provide-theme 'jpb-tangoish)
