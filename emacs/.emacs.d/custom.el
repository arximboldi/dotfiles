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
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["black" "red" "green yellow" "yellow" "deep sky blue" "magenta" "cyan" "white"])
 '(clojure-defun-style-default-indent t)
 '(coffee-extend-comments nil)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save t)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 2)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (jpb-tangoish)))
 '(custom-safe-themes
   (quote
    ("4c1a8d0143d6b934e0850ccad0834a0ff5ce560429d7943951b5eb63926ddc42" "e0d172b92241af14b48eeec945acbfa630f375d977894b8c53fde080b612bace" "46c1851dcac429f36a456bf15faf9e182fdd0d7e8b0ce2187994b99980c93c49" "0e02eb0f034e42b40cd57186bb3829daad7ad5e9c3c7f92290ab6ec2b2d8b9fe" "3160bcafe2e03bcf965eee0d65cbdc633edea5b537b6362bcc5a843ac3fe1f92" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(debug-on-error (quote (nil)))
 '(ecb-layout-window-sizes
   (quote
    (("leftright2"
      (0.1935483870967742 . 0.6190476190476191)
      (0.1935483870967742 . 0.35714285714285715)
      (0.1870967741935484 . 0.6190476190476191)
      (0.1870967741935484 . 0.35714285714285715)))))
 '(focus-follows-mouse t)
 '(ggtags-update-on-save nil)
 '(global-hl-line-mode t)
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
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(multi-term-dedicated-select-after-open-p t)
 '(mumamo-chunk-coloring 2)
 '(projectile-global-mode t)
 '(python-shell-prompt-alist (quote (("ipython" . "^In \\[[0-9]+\\]: *") (t . "^>>> "))))
 '(safe-local-variable-values
   (quote
    ((web-mode-markup-indent-offset . 4)
     (web-mode-markup-indent-offset . 80)
     (web-markup-indent-offset . 80)
     (coffee-tab-width . 4)
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(send-mail-function nil)
 '(show-paren-mode t)
 '(sort-fold-case t t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "raskolnikov@es.gnu.org")
 '(vc-follow-symlinks t)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(web-mode-markup-indent-offset 2)
 '(yas/root-directory "/home/raskolnikov/.emacs.d/snippets" nil (yasnippet))
 '(zoom-frame/buffer (quote buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
