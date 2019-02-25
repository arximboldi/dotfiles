(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(auto-revert-use-notify t)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "xdg-open")
 '(calendar-week-start-day 1)
 '(cider-lein-command "~/usr/bin/lein")
 '(clang-format-executable "/run/current-system/sw/bin/clang-format")
 '(clojure-defun-style-default-indent t)
 '(clojure-indent-style :align-arguments)
 '(coffee-extend-comments nil)
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(comint-prompt-read-only t)
 '(compilation-always-kill t)
 '(compilation-ask-about-save t)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output (quote first-error))
 '(compilation-skip-threshold 1)
 '(confirm-kill-emacs nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (arximboldi-wombatish)))
 '(custom-safe-themes
   (quote
    ("09640daf3480eca557e808dea6367c0e0f37ff45d3b26cbf179b5859b455ff86" "51d2af49e41d624c7c87d327697d9d908189aa3382e99ca1b3c10b88e23eaff0" "0ebb00c0263c0f7d58c467f11bc16a9cd67b9cd5852beb88062b741e86cb246e" "3b5ce826b9c9f455b7c4c8bff22c020779383a12f2f57bf2eb25139244bb7290" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "3629b62a41f2e5f84006ff14a2247e679745896b5eaa1d5bcfbc904a3441b0cd" "d8004f25fd54dd59b4531dabc01ea52b53c06e2ed134d7f8ec141dfde97931bb" "d2f901cc5f645f4902e0b7ec34db9051a3fd0d606c3dff701274fa6121497113" "f12ea40ac12a52796c3964885d2097b1e6d863e8140e2a8771852229a6aaa910" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4137e19e0111dba058c0353124748bda825f0e4d3a598437e9ebef09336394ff" "327ba2fb02afd733e0a1ca3223461b01614b7e05ae25c6f283c328d70253c664" "2d3aeda84fd1d4f9a3eb86f5ac35032f49ffa5a7a87db7b9369e0a02f9da4ab3" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0becccb038ce4f208bbd4857a5fcef01db55d50cf76ea3eafee69fef11d66266" "057036168b48a3f01947c59e53ff3e783314023fd47119b76feaa24d7a5f1fbe" "b12485642c320bc0c0484c1c82966ef1029833901a58eef974787c518cf177d7" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "51e228ffd6c4fff9b5168b31d5927c27734e82ec61f414970fc6bcce23bc140d" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "5e402ccb94e32d7d09e300fb07a62dc0094bb2f16cd2ab8847b94b01b9d5e866" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "d8a7a7d2cffbc55ec5efbeb5d14a5477f588ee18c5cddd7560918f9674032727" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "ff8c6c2eb94e776c9eed9299a49e07e70e1b6a6f926dec429b99cf5d1ddca62a" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "78559045fb299f3542c232166ad635c59cf0c6578d80a58b885deafe98a36c66" "0f97285f9e0c7d9cad04f2130859d20d6c9b3142877b2bca52d958f4f1cf346f" "c7a9a68bd07e38620a5508fef62ec079d274475c8f92d75ed0c33c45fbe306bc" "2956312abe58f2fafdff7b123ffadb8a4f899f65e536b0a626a00301a3c8cd4c" "8f5ba51d5c3ba470af56b44b6d06211bcc5261c0350947a04cf0899b70c34501" "e57185d35bc6d7c0d1ba4b858621283a63bf76b259f3564911316ce6249b4398" "b84b971271829b001b5c216b668afdcd2603af52425f159755b4c133169c3001" "557c283f4f9d461f897b8cac5329f1f39fac785aa684b78949ff329c33f947ec" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "d6fc9e361d210ecbfe54b439758ba8d65a228f7b17facbeefa036b44d47c0e0f" "93e4e6fff01a22f0e2446a91c1d3e80283bf482d6e8b0cf40884d21335b57a9b" "f6b5a86b0ce223e5291255299957bb1d2201690fdcdd65aff7f64fea1b4b0f6b" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "f3d6a49e3f4491373028eda655231ec371d79d6d2a628f08d5aa38739340540b" "5d7e1a089a0392827c1a1a626c93f2be5cf1a108a5f86663e9f1eed67fd094ea" "a272871ca537510c61bd3ddb4daaeb95ba73845eccbf2fe7937c884a18c924b5" "1b4658616494ac5d7da8bc6314c9d158b66b74e127e2a0541850a7347280d70b" "9d47f3395c25ccd8253c2fc34f26cb8cf12ef60663f2309f0c8dcd26b82beda7" "78775f7c0a85c79428d3de173d3aa6f1c9882e1f6f4ec117de07d19139115cda" "fb212a2004c6280201f189ebd10dc24de9bccae9d1280aff678c4a9a1054fb02" "134d8e86968d524c7b5c06820c122ddab65a4f5292394d071afa68ebd6c46e00" "e3fd68932f934e84a4eb9030d01835c8e82e3fe12b4ccc83047c1f356a1563a2" "897c1818c6418f5a3de91e8d62a37602aa646105135ad69f52cd0967c8219d7e" "67a2629193b0e4f5a9ac2eae2fd3055b62140898f43947ca45f5fc40fe3a46b8" "595535e6f9aec16532f70f1468cb02320ecc771b3ecb52e40938a844b6ca3bfc" "f028ce0fa615bc4afc178e39a3b8c335b5f8278b76ea2fbba4134931fbacdac9" "596677a7bb114238a95e3fe2fbfec3c639da2dfdd4ac64063cfa7a92c68c2a8b" "ba7d3ed0071a22b41ad29d29dcbc1bce9240b2b4fc07fe92964fcc505bdac2c7" "47bd4d88ce84346599420b4e6acb749c879b394cb29f39340488bb38121c6009" "70e75e4d9d072a8c60c60c2b5f27a585710ae89bc87424a102435d79d5af67a2" "cda6696b4b65c87c0b7e5bf45f68b636f6e2bf256c9153a5bd4b9a547db7bd9c" "ebaaf0e487abf3499cde46618d6b3152f141133318228a38ffd3442cc5930172" "c7d165cd1f4367204d12371237c37e7043ce546b1b9b61bedb6fb227dfa292b8" "c2ccbbb11ba66ad80a74556bf9c2a90e4add0394bc88fb399856f2c6a0891443" "629789b9c4d26628afb186bfd9e02998f64bbd37af53675ac16c371790fd71a8" "1b9b8658cb7a8176e9dc12b047dff532bffbcaf31434ac98b8f2511f4370d92e" "c1775eea5ecc640e09e69a5c07a746745e7d7348d2202521ca4e18c76ebd50c3" "4c1a8d0143d6b934e0850ccad0834a0ff5ce560429d7943951b5eb63926ddc42" "e0d172b92241af14b48eeec945acbfa630f375d977894b8c53fde080b612bace" "46c1851dcac429f36a456bf15faf9e182fdd0d7e8b0ce2187994b99980c93c49" "0e02eb0f034e42b40cd57186bb3829daad7ad5e9c3c7f92290ab6ec2b2d8b9fe" "3160bcafe2e03bcf965eee0d65cbdc633edea5b537b6362bcc5a843ac3fe1f92" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(debug-on-error (quote (nil)))
 '(desktop-auto-save-timeout 1)
 '(desktop-load-locked-desktop t)
 '(desktop-restore-frames nil)
 '(dired-listing-switches "-al -X --group-directories-first")
 '(dumb-jump-max-find-time 10)
 '(dumb-jump-prefer-searcher (quote ag))
 '(ecb-layout-window-sizes
   (quote
    (("leftright2"
      (0.1935483870967742 . 0.6190476190476191)
      (0.1935483870967742 . 0.35714285714285715)
      (0.1870967741935484 . 0.6190476190476191)
      (0.1870967741935484 . 0.35714285714285715)))))
 '(fci-always-use-textual-rule t)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#444")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(focus-follows-mouse t)
 '(geiser-default-implementation (quote guile))
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
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
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
 '(line-spacing 0.2)
 '(magit-auto-revert-immediately nil)
 '(magit-auto-revert-mode nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mu4e-completing-read-function (quote completing-read) t)
 '(mu4e-maildir "~/mail")
 '(multi-term-dedicated-select-after-open-p t)
 '(mumamo-chunk-coloring 2)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
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
    (bazel-mode wanderlust elm-mode char-menu charmap visual-fill-column nix-mode solarized-theme use-package parinfer color-theme-sanityinc-tomorrow twilight-bright-theme twilight-theme github-theme emojify dockerfile-mode clang-format sublimity ag biblio bibretrieve bibslurp company-auctex company-bibtex ebib empos gscholar-bibtex helm-bibtex jade-mode helm-go-package go-playground company-go go-eldoc go-mode helm-ag company-ycmd flycheck-ycmd gnus-alias cmake-mode nsis-mode emoji-cheat-sheet-plus google-translate babel hackernews helm-smex helm-mu multiple-cursors helm-make helm-notmuch helm-package helm-unicode swiper-helm helm-projectile helm nm notmuch notmuch-labeler fill-column-indicator zoom-window zoom-frm zeal-at-point yaml-mode xterm-color web-mode w3 vline travis string-utils smex skewer-mode skeletor shell-toggle scss-mode sass-mode rust-mode request-deferred realgud rainbow-delimiters projectile nginx-mode neotree multi-term monokai-theme modern-cpp-font-lock markdown-mode+ magit-filenotify jack-connect jabber-otr ido-ubiquitous haskell-mode git-messenger gist geiser expand-region ensime emms dumb-jump dirtree dirtrack-buffer-name-track-mode diminish dedicated cyberpunk-theme cmake-ide clojurescript-mode clojure-cheatsheet cljsbuild-mode circe bitlbee auctex apt-utils adaptive-wrap)))
 '(pdf-view-midnight-colors (quote ("#969896" . "#f8eec7")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-enable-caching t)
 '(projectile-global-mode t)
 '(projectile-use-git-grep t)
 '(python-shell-prompt-alist (quote (("ipython" . "^In \\[[0-9]+\\]: *") (t . "^>>> "))))
 '(rainbow-delimiters-max-face-count 10)
 '(recentf-max-saved-items 1000)
 '(ring-bell-function (quote ignore))
 '(rust-format-on-save t)
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote access-label)
           (quote -))
     (eval c-set-offset
           (quote substatement-open)
           0)
     (eval c-set-offset
           (quote arglist-cont-nonempty)
           (quote +))
     (eval c-set-offset
           (quote arglist-cont)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote +))
     (eval c-set-offset
           (quote inline-open)
           0)
     (eval c-set-offset
           (quote defun-open)
           0)
     (indicate-empty-lines . t)
     (bug-reference-bug-regexp . "\\(\\(?:[Ii]ssue \\|[Ff]ixe[ds] \\|[Rr]esolve[ds]? \\|[Cc]lose[ds]? \\|[Pp]\\(?:ull [Rr]equest\\|[Rr]\\) \\|(\\)#\\([0-9]+\\))?\\)")
     (indent-tabs-mode . f)
     (eval add-hook
           (quote before-save-hook)
           (function clang-format-buffer)
           nil t)
     (eval modify-syntax-entry 43 "'")
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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sort-fold-case nil t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(truncate-lines nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(user-mail-address "raskolnikov@es.gnu.org")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(visible-bell nil)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas/root-directory "~/.emacs.d/snippets")
 '(ycmd-global-config "~/.ycm_extra_conf.py")
 '(ycmd-server-command (quote ("ycmd")))
 '(zoom-frame/buffer (quote buffer)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
