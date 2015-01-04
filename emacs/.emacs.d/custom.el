(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "lualatex")
 '(LaTeX-command-style (quote (("" "%(latex) %S%(PDFout)"))))
 '(TeX-output-view-style
   (quote
    (("^dvi$"
      ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
      "%(o?)dvips -t landscape %d -o && gv %f")
     ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f")
     ("^dvi$"
      ("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a4r -s 0 %d")
     ("^dvi$" "^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)xdvi %dS -paper a4 %d")
     ("^dvi$"
      ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "^landscape$")
      "%(o?)xdvi %dS -paper a5r -s 0 %d")
     ("^dvi$" "^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)xdvi %dS -paper a5 %d")
     ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d")
     ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d")
     ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d")
     ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d")
     ("^dvi$" "." "%(o?)xdvi %dS %d")
     ("^pdf$" "." "evince %o")
     ("^html?$" "." "netscape %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "Evince")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(TeX-view-style
   (quote
    (("^\\(?:a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4\\)$" "%(o?)evince %dS -paper a4 %d")
     ("^\\(?:a5\\(?:comb\\|paper\\)\\)$" "%(o?)evince %dS -paper a5 %d")
     ("^b5paper$" "%(o?)evince %dS -paper b5 %d")
     ("^letterpaper$" "%(o?)evince %dS -paper us %d")
     ("^legalpaper$" "%(o?)evince %dS -paper legal %d")
     ("^executivepaper$" "%(o?)evince %dS -paper 7.25x10.5in %d")
     ("^landscape$" "%(o?)evince %dS -paper a4r -s 0 %d")
     ("." "%(o?)evince %d"))))
 '(coffee-extend-comments nil)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-skip-threshold 2)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(debug-on-error (quote (nil)))
 '(ecb-layout-window-sizes
   (quote
    (("leftright2"
      (0.1935483870967742 . 0.6190476190476191)
      (0.1935483870967742 . 0.35714285714285715)
      (0.1870967741935484 . 0.6190476190476191)
      (0.1870967741935484 . 0.35714285714285715)))))
 '(frame-background-mode (quote dark))
 '(global-hl-line-mode t)
 '(global-rainbow-delimiters-mode t)
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-doc turn-on-haskell-indentation
                         (lambda nil
                           (ghc-init)
                           (flymake-mode)))) t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-directory-size 30000)
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ido-separator nil)
 '(ido-ubiquitous-command-exceptions
   (quote
    (vc-version-diff gtags-find-tag gtags-find-rtag gtags-find-tag-other-window)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil t)
 '(jmaker-make-compiler-options "-g")
 '(line-number-mode t)
 '(line-spacing 4)
 '(menu-bar-mode nil)
 '(mumamo-chunk-coloring 2)
 '(projectile-global-mode t)
 '(python-shell-prompt-alist (quote (("ipython" . "^In \\[[0-9]+\\]: *") (t . "^>>> "))))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(send-mail-function nil)
 '(show-paren-mode t)
 '(sort-fold-case t t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "raskolnikov@es.gnu.org")
 '(vc-follow-symlinks t)
 '(yas/root-directory "/home/raskolnikov/.emacs.d/snippets" nil (yasnippet))
 '(zoom-frame/buffer (quote buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000000" :foreground "white" :height 150 :family "Inconsolata"))))
 '(custom-button ((t (:background "#333" :foreground "#fff" :box (:line-width 2 :style released-button)))))
 '(custom-button-mouse ((t (:background "#555" :foreground "#fff" :box (:line-width 2 :style released-button)))))
 '(diff-added ((t (:inherit diff-changed :foreground "green yellow"))))
 '(diff-file-header ((t (:inverse-video t :weight bold))))
 '(diff-hunk-header ((t (:inherit (highlight diff-header)))))
 '(diff-removed ((t (:inherit diff-changed :foreground "tomato"))))
 '(highlight ((t (:background "#202020"))))
 '(ido-first-match ((t (:foreground "#D64937" :weight bold))))
 '(ido-only-match ((t (:foreground "lawn green"))))
 '(ido-subdir ((t (:foreground "#729FCF"))))
 '(mode-line ((t (:background "#d64937" :foreground "black"))))
 '(mode-line-emphasis ((t (:foreground "black" :weight bold))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#222" :foreground "#999" :weight light))))
 '(region ((t (:background "white" :foreground "black")))))
