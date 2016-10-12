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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["black" "red" "green yellow" "yellow" "deep sky blue" "magenta" "cyan" "white"])
 '(auto-revert-use-notify t)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "xdg-open")
 '(calendar-week-start-day 1)
 '(clojure-defun-style-default-indent t)
 '(clojure-indent-style :align-arguments)
 '(coffee-extend-comments nil)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save t)
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 1)
 '(confirm-kill-emacs nil)
 '(custom-enabled-themes (quote (jpb-wombatish)))
 '(custom-safe-themes
   (quote
    ("6720cad62cc53720c4d16a9b529383483e647c4e65b15c78c509dbbdb49805af" "e355167d78e5217f8125de13df566cf0f7fb402f4dbbd1623ce7085ca92ddc77" "8f5ba51d5c3ba470af56b44b6d06211bcc5261c0350947a04cf0899b70c34501" "e57185d35bc6d7c0d1ba4b858621283a63bf76b259f3564911316ce6249b4398" "b84b971271829b001b5c216b668afdcd2603af52425f159755b4c133169c3001" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "d6fc9e361d210ecbfe54b439758ba8d65a228f7b17facbeefa036b44d47c0e0f" "93e4e6fff01a22f0e2446a91c1d3e80283bf482d6e8b0cf40884d21335b57a9b" "f6b5a86b0ce223e5291255299957bb1d2201690fdcdd65aff7f64fea1b4b0f6b" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "5d7e1a089a0392827c1a1a626c93f2be5cf1a108a5f86663e9f1eed67fd094ea" "a272871ca537510c61bd3ddb4daaeb95ba73845eccbf2fe7937c884a18c924b5" "1b4658616494ac5d7da8bc6314c9d158b66b74e127e2a0541850a7347280d70b" "9d47f3395c25ccd8253c2fc34f26cb8cf12ef60663f2309f0c8dcd26b82beda7" "78775f7c0a85c79428d3de173d3aa6f1c9882e1f6f4ec117de07d19139115cda" "fb212a2004c6280201f189ebd10dc24de9bccae9d1280aff678c4a9a1054fb02" "134d8e86968d524c7b5c06820c122ddab65a4f5292394d071afa68ebd6c46e00" "e3fd68932f934e84a4eb9030d01835c8e82e3fe12b4ccc83047c1f356a1563a2" "897c1818c6418f5a3de91e8d62a37602aa646105135ad69f52cd0967c8219d7e" "67a2629193b0e4f5a9ac2eae2fd3055b62140898f43947ca45f5fc40fe3a46b8" "595535e6f9aec16532f70f1468cb02320ecc771b3ecb52e40938a844b6ca3bfc" "f028ce0fa615bc4afc178e39a3b8c335b5f8278b76ea2fbba4134931fbacdac9" "596677a7bb114238a95e3fe2fbfec3c639da2dfdd4ac64063cfa7a92c68c2a8b" "ba7d3ed0071a22b41ad29d29dcbc1bce9240b2b4fc07fe92964fcc505bdac2c7" "47bd4d88ce84346599420b4e6acb749c879b394cb29f39340488bb38121c6009" "70e75e4d9d072a8c60c60c2b5f27a585710ae89bc87424a102435d79d5af67a2" "cda6696b4b65c87c0b7e5bf45f68b636f6e2bf256c9153a5bd4b9a547db7bd9c" "ebaaf0e487abf3499cde46618d6b3152f141133318228a38ffd3442cc5930172" "c7d165cd1f4367204d12371237c37e7043ce546b1b9b61bedb6fb227dfa292b8" "c2ccbbb11ba66ad80a74556bf9c2a90e4add0394bc88fb399856f2c6a0891443" "629789b9c4d26628afb186bfd9e02998f64bbd37af53675ac16c371790fd71a8" "1b9b8658cb7a8176e9dc12b047dff532bffbcaf31434ac98b8f2511f4370d92e" "c1775eea5ecc640e09e69a5c07a746745e7d7348d2202521ca4e18c76ebd50c3" "4c1a8d0143d6b934e0850ccad0834a0ff5ce560429d7943951b5eb63926ddc42" "e0d172b92241af14b48eeec945acbfa630f375d977894b8c53fde080b612bace" "46c1851dcac429f36a456bf15faf9e182fdd0d7e8b0ce2187994b99980c93c49" "0e02eb0f034e42b40cd57186bb3829daad7ad5e9c3c7f92290ab6ec2b2d8b9fe" "3160bcafe2e03bcf965eee0d65cbdc633edea5b537b6362bcc5a843ac3fe1f92" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(debug-on-error (quote (nil)))
 '(desktop-auto-save-timeout 1)
 '(desktop-load-locked-desktop t)
 '(dired-listing-switches "-al -X --group-directories-first")
 '(ecb-layout-window-sizes
   (quote
    (("leftright2"
      (0.1935483870967742 . 0.6190476190476191)
      (0.1935483870967742 . 0.35714285714285715)
      (0.1870967741935484 . 0.6190476190476191)
      (0.1870967741935484 . 0.35714285714285715)))))
 '(fci-always-use-textual-rule t)
 '(fci-rule-color "#444")
 '(focus-follows-mouse t)
 '(ggtags-update-on-save nil)
 '(global-hl-line-mode t)
 '(gnus-use-dribble-file nil)
 '(helm-M-x-fuzzy-match t)
 '(helm-candidate-separator "▶")
 '(helm-display-header-line t)
 '(helm-ff-fuzzy-matching t)
 '(helm-file-cache-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(helm-projectile-fuzzy-match t)
 '(helm-split-window-default-side (quote other))
 '(horizontal-scroll-bar-mode nil)
 '(ido-enable-flex-matching t)
 '(ido-max-directory-size 30000)
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ido-separator nil)
 '(ido-ubiquitous-command-exceptions
   (quote
    (vc-version-diff gtags-find-tag gtags-find-rtag gtags-find-tag-other-window)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary nil)
 '(jabber-alert-presence-message-function (quote jabber-presence-only-chat-open-message))
 '(jabber-history-enabled t)
 '(jabber-roster-line-format " %a %c %-25n %u %-8s  %S (%j)")
 '(jabber-roster-show-bindings nil)
 '(jabber-roster-show-title nil)
 '(jabber-vcard-avatars-retrieve nil)
 '(jmaker-make-compiler-options "-g")
 '(js2-strict-missing-semi-warning nil)
 '(line-number-mode t)
 '(line-spacing 4)
 '(magit-auto-revert-immediately nil)
 '(magit-auto-revert-mode nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mu4e-completing-read-function (quote completing-read) t)
 '(mu4e-maildir "~/mail")
 '(multi-term-dedicated-select-after-open-p t)
 '(mumamo-chunk-coloring 2)
 '(openwith-associations
   (quote
    (("\\.pdf\\'" "xdg-open"
      (file))
     ("\\.mp3\\'" "xdg-open"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "xdg-open"
      ("-idx" file))
     ("\\.svg\\'" "inkscape" nil)
     ("\\.\\(?:jp?g\\|png\\)\\'" "xdg-open"
      (file)))))
 '(openwith-mode nil)
 '(package-selected-packages
   (quote
    (helm-ag company-ycmd flycheck-ycmd gnus-alias cmake-mode nsis-mode emoji-cheat-sheet-plus google-translate babel hackernews helm-smex helm-mu multiple-cursors helm-make helm-notmuch helm-package helm-unicode swiper-helm helm-projectile helm nm notmuch notmuch-labeler fill-column-indicator zoom-window zoom-frm zeal-at-point yaml-mode xterm-color web-mode w3 vline travis string-utils smex skewer-mode skeletor shell-toggle scss-mode sass-mode rust-mode request-deferred realgud rainbow-delimiters projectile nginx-mode neotree multi-term monokai-theme modern-cpp-font-lock markdown-mode+ magit-filenotify jack-connect jabber-otr ido-ubiquitous haskell-mode git-messenger gist geiser flymake-yaml flymake-sass expand-region ensime emms dumb-jump dirtree dirtrack-buffer-name-track-mode diminish dedicated cyberpunk-theme cmake-ide clojurescript-mode clojure-cheatsheet cljsbuild-mode circe bitlbee auctex apt-utils adaptive-wrap)))
 '(projectile-global-mode t)
 '(projectile-use-git-grep t)
 '(python-shell-prompt-alist (quote (("ipython" . "^In \\[[0-9]+\\]: *") (t . "^>>> "))))
 '(rainbow-delimiters-max-face-count 10)
 '(safe-local-variable-values
   (quote
    ((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")
     (eval c-set-offset
           (quote innamespace)
           0)
     (web-mode-markup-indent-offset . 2)
     (web-mode-markup-indent-offset . 4)
     (web-mode-markup-indent-offset . 80)
     (web-markup-indent-offset . 80)
     (coffee-tab-width . 4)
     (eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1)))))
 '(sass-indent-offset 4)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(scroll-step 1)
 '(send-mail-function nil)
 '(shell-toggle-launch-shell (quote shell))
 '(show-paren-mode t)
 '(sort-fold-case t t)
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "raskolnikov@es.gnu.org")
 '(vc-follow-symlinks t)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(web-mode-markup-indent-offset 2)
 '(yas/root-directory "~/.emacs.d/snippets" nil (yasnippet))
 '(zoom-frame/buffer (quote buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
