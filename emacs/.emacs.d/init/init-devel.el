;;; emacs config file -- Juan Pedro Bolívar Puente

(require 'compile)

(defun @fill-mode ()
  (interactive)
  (setq fill-column 80)
  (fci-mode))

;; Autocompletion
(require 'company)
;;(require 'company-box)
;;(require 'company-tabnine)

;;(add-to-list 'company-backends #'company-tabnine)
;;(add-hook 'company-mode-hook 'company-box-mode)
(add-hook 'after-init-hook 'global-company-mode)

(require 'lsp-mode)
(require 'dap-mode)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools))

;;(add-hook 'c-mode-hook 'lsp)
;;(add-hook 'c++-mode-hook 'lsp)

;; Trigger completion immediately.
(setq company-idle-delay 0)
(setq company-show-numbers t)

;;(setq gc-cons-threshold (* 100 1024 1024)
;;      read-process-output-max (* 1024 1024)
;;      treemacs-space-between-root-nodes nil
;;      company-idle-delay 0.0
;;      company-minimum-prefix-length 1
;;      lsp-idle-delay 0.1)

;; Documentatio
(require 'eldoc)
(add-hook 'after-init-hook 'global-eldoc-mode)

;; Projects
(require 'projectile)
(add-to-list 'projectile-project-root-files ".travis.yml")
(add-to-list 'projectile-project-root-files ".editorconfig")
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-to-list 'projectile-globally-ignored-directories "third-party")
(add-to-list 'projectile-globally-ignored-directories "third_party")
(add-to-list 'projectile-globally-ignored-directories "Third_Party")
(setq projectile-indexing-method 'alien)

;;
;; GIT
;;

;; (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

;;
;; Yasnippet
;;

;; (require 'yasnippet)
;; (yas-global-mode 1)

;;
;; Python
;;

(add-hook 'python-mode-hook 'flycheck-mode)

;;
;; LaTeX
;;

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(add-hook 'rst-mode-hook 'auto-fill-mode)
(add-hook 'rst-mode-hook 'flyspell-mode)

(setq reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)

;; cmake-format
(require 'reformatter)

(reformatter-define cmake-format
  :program "cmake-format"
  :args '("-"))

;;
;; C++
;;

;; ycmd
;; (company-ycmd-setup)
;; (flycheck-ycmd-setup)
;; (ycmd-eldoc-setup)
;; (set-variable 'ycmd-extra-conf-whitelist '("~/dev/*"))
;; (add-hook 'c-mode-hook 'ycmd-mode)
;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook 'ycmd-eldoc-setup)
;; (add-hook 'c++-mode-hook 'ycmd-mode)
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c++-mode-hook 'ycmd-eldoc-setup)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . c++-mode))

(c-add-style "arximboldi"
             '("stroustrup"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-offsets-alist . ((inline-open . 0)
                                   (brace-list-open . 0)
                                   (innamespace . 0)
                                   (inlambda . 0)
                                   (statement-case-open . +)))))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq fill-column 80)
            (c-set-style "arximboldi")))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-hook 'c-mode-common-hook #'auto-fill-mode)

(defun @enable-cpp-headers ()
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(defun @disable-cpp-headers ()
  (interactive)
  (setq auto-mode-alist (remove '("\\.h\\'" . c++-mode) auto-mode-alist)))

;;
;; Compilation
;;

(setq compilation-window-height 12)

(defun @compile-here ()
  "Set the compilation directory to the current one"
  (interactive)
  (setq compile-command
        (concat "cd " (file-name-directory (buffer-file-name)) "; make")))

(defun @compile-this ()
  "Set the compile command to make this file if there is no makefile around"
  (interactive)
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "make -k "
                 (file-name-sans-extension (buffer-file-name))))))

(defun @compile-at (str)
  "Set the compile command to build a selected directory"
  (interactive "DCompilation directory: ")
  (setq gud-gdb-command-name
        (concat "cd " str "; gdb -i=mi"))
  (setq compile-command
        (concat "cd " str "; make")))

(defun @compile-leave ()
  (interactive)
  (setq compilation-finish-function nil))

(defun @compile-close ()
  (interactive)
  (setq compilation-finish-function
        (lambda (buf str)
          (if (string-match "exited abnormally" str)
              (message "Compilation errors, press C-x ` to visit")
            (run-at-time 0.5 nil 'delete-windows-on buf)
            (message "NO COMPILATION ERRORS :-)")))))

;;
;; Debug
;;

(setq gdb-many-windows nil)

(defun @gdb-file (fname)
  "Set the compile command to build a selected directory"
  (interactive "fExecutable: ")
  (gdb (concat "gdb -i=mi " fname)))

(defun @gdb-at (loc)
  "Set the compile command to build a selected directory"
  (interactive "DLocation: \n")
  (gdb (concat "gdb --cd=\"" loc "\" -i=mi")))

;; Force gdb-mi to not dedicate any windows
(defadvice gdb-display-buffer
    (after undedicate-gdb-display-buffer)
  (set-window-dedicated-p ad-return-value nil))
(ad-activate 'gdb-display-buffer)

(defadvice gdb-set-window-buffer
    (after undedicate-gdb-set-window-buffer
           (name &optional ignore-dedi window))
  (set-window-dedicated-p window nil))
(ad-activate 'gdb-set-window-buffer)

;;
;; Bazel
;;
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . bazel-build-mode))
(add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-build-mode))

;;
;; QML
;;

(add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

(font-lock-add-keywords
 'js-mode
 '(("\\<\\(readonly\\|signal\\|property\\)\\>[^:]" . font-lock-keyword-face)
   ("id:\\s-*?\\<\\([[:alpha:]_$]\\(?:\\s_\\|\\sw\\)*\\)\\>" . font-lock-variable-name-face)))

;; JS
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))


;;
;; Gtags
;;

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (interactive)
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))

;; Not needed, ggtags takes care of this now...
;;   (add-hook 'after-save-hook #'gtags-update-hook)

(defun compile-gtags (str)
  "compile etags for the current project"
  (interactive "DSources directory: ")
  (compile (concat "cd " (expand-file-name str) "; gtags")))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

;;
;; Coffee
;;
(require 'coffee-mode)

;;
;; Update copyright
;;

;;(add-hook 'before-save-hook 'copyright-update)
;;(remove-hook 'before-save-hook 'copyright-update)

;; Python errors
(add-to-list 'compilation-error-regexp-alist '("^In file \\(.*?\\):\\([0-9]+\\)$" 1 2))

;; Ableton assertions
(add-to-list 'compilation-error-regexp-alist '("File \"\\(.*?\\)\", Line \\([0-9]+\\):$" 1 2))

;; Clang sanitizer errors
(add-to-list 'compilation-error-regexp-alist '("^    #[0-9]+ 0x[[:xdigit:]]+ in .* \\(.*?\\):\\([0-9]+\\):?\\([0-9]+\\)?$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist '("^Assertion failed: .*, file \\(.*?\\), line \\([0-9]+\\)\.$" 1 2 3))

;; QML errors
(add-to-list 'compilation-error-regexp-alist '("^\\[\\(.*\\)\\] \\[\\(.*?\\)\\] file://\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\) \\(.*\\)$" 3 4 5 2))
(add-to-list 'compilation-error-regexp-alist '("^file://\\(.*?\\):\\([0-9]+\\) \\(.*\\)$" 1 2))
(add-to-list 'compilation-error-regexp-alist '("^file://\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\) \\(.*\\)$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist '("^qt.qml.context: file://\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\) \\(.*\\)$" 1 2))

;;
;; Web mode
;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (c-set-offset 'innamespace 0)
            (setq indent-tabs-mode nil)))

;;
;; Clojure
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)

(defun @cider-connect ()
  (interactive)
  (cider-connect "localhost" "7888"))

(defun @cider-enable-figwheel-cljs ()
  (interactive)
  (cider-interactive-eval
   "(use 'figwheel-sidecar.repl-api) (cljs-repl)"
   nil
   nil))

;;(use-package parinfer
;;  :ensure t
;;  :config
;;  (parinfer-strategy-add 'instantly
;;    '(parinfer-smart-tab:dwim-right
;;      parinfer-smart-tab:dwim-left))
;;  :bind
;;  (:map parinfer-mode-map
;;        ("<tab>" . parinfer-smart-tab:dwim-right)
;;        ("S-<tab>" . parinfer-smart-tab:dwim-left)
;;        ("<backtab>" . parinfer-smart-tab:dwim-left)
;;        ("C-," . parinfer-toggle-mode)
;;        :map parinfer-region-mode-map
;;        ("<tab>" . parinfer-smart-tab:dwim-right)
;;        ("S-<tab>" . parinfer-smart-tab:dwim-left)
;;        ("<backtab>" . parinfer-smart-tab:dwim-left))
;;  :init
;;  (progn
;;    (setq parinfer-extensions
;;          '(defaults
;;             pretty-parens
;;             smart-tab
;;             smart-yank))))

;;
;; Octave
;;
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;
;; Go
;;
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)
            (setq indent-tabs-mode 0)
            (set (make-local-variable 'company-backends) '(company-go))))

(eval-after-load 'go-mode
  '(substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

;;
;; Gyp
;;
(require 'gyp)

;;
;; Nix
;;
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;;
;; USDA
;;
;;(require 'usda-mode)
;;(add-to-list 'auto-mode-alist '("\\.usda\\'" . text-mode))

(defun @reformat-xml ()
  "Reformats xml to make it readable (respects current selection)."
  (interactive)
  (save-excursion
    (let ((beg (point-min))
          (end (point-max)))
      (if (and mark-active transient-mark-mode)
          (progn
            (setq beg (min (point) (mark)))
            (setq end (max (point) (mark))))
        (widen))
      (setq end (copy-marker end t))
      (goto-char beg)
      (while (re-search-forward ">\\s-*<" end t)
        (replace-match ">\n<" t t))
      (goto-char beg)
      (indent-region beg end nil))))

(provide 'init-devel)
