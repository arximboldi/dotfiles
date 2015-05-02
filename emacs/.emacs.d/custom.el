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
 '(custom-enabled-themes (quote (jpb-colors)))
 '(custom-safe-themes
   (quote
    ("ebaaf0e487abf3499cde46618d6b3152f141133318228a38ffd3442cc5930172" "12dc22fa6c8189a358386593019ba51cda0add90049bfd292f1a8bba9dbc6724" "cf416230c28ca8da9b1c91f9f3feeb407d10eb77fe77786c683a6a4db0e1c5f8" "300a340dc3c59ac2e456c09de226aa77b6e227a363146cdf2d4a28046ea740ba" "c515b07e84dfddb1d24fc8e45bd2092a1c7b21f2d3f2c389307b69532dd1658e" "141d2c493b27d04ae6ae728fa66217094fb305a7a947b573517b02e2061a0db6" "e7ec0cc3ce134cc0bd420b98573bbd339a908ac24162b8034c98e1ba5ee1f9f6" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "0251780e8e79d2a5e75eec7ee3b6c646b882495cb884d9dd32f30c60f9d65db6" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "2bc75d7be15ccc7e1bc7a97653bc91dbf893ec0fa9d889ee8a6aeb3abfdf85f9" "cf63a0bbcf204eaace5d918575fa078e8bdd52758e994e5a38e5df628f9ad015" "0ba649556dc51762e6794b92017f6f7406754ae3136eafef686d81c6da176cc5" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "f7b2af67ec4434c94a3869dc1f0e4d2a4f85e5a1bec2b1c19776717d7ad001da" "18a33cdb764e4baf99b23dcd5abdbf1249670d412c6d3a8092ae1a7b211613d5" default)))
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
 '(menu-bar-mode nil)
 '(multi-term-dedicated-select-after-open-p t)
 '(mumamo-chunk-coloring 2)
 '(projectile-global-mode t)
 '(python-shell-prompt-alist (quote (("ipython" . "^In \\[[0-9]+\\]: *") (t . "^>>> "))))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
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
 '(yas/root-directory "/home/raskolnikov/.emacs.d/snippets" nil (yasnippet))
 '(zoom-frame/buffer (quote buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
