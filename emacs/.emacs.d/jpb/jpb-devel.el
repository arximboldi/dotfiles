;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

(defun jpb-fill-mode ()
  (setq fill-column 80)
  (fci-mode))

;;
;; GIT
;;
;; (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

;;
;; Yasnippet
;;

(require 'yasnippet)
(yas-global-mode 1)

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


;;
;; C++
;;

;; Disable until errors solved
;; (cmake-ide-setup)
;; (setq cmake-ide-build-dir "build")

(modern-c++-font-lock-global-mode t)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;;
;; Fix C++ lambdas
;;
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

;;
;; Fix C++ enum class
;;

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)
            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; flycheck
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; eldoc
(add-hook 'irony-mode-hook 'irony-eldoc)

;;
;; Compilation
;;

(setq compilation-window-height 12)

(defun compile-here ()
  "Set the compilation directory to the current one"
  (interactive)
  (setq compile-command
	(concat "cd " (file-name-directory (buffer-file-name)) "; make")))

(defun compile-this ()
  "Set the compile command to make this file if there is no makefile around"
  (interactive)
  (unless (or (file-exists-p "makefile")
	      (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
	 (concat "make -k "
		 (file-name-sans-extension (buffer-file-name))))))

(defun compile-at (str)
  "Set the compile command to build a selected directory"
  (interactive "DCompilation directory: ")
  (setq gud-gdb-command-name
        (concat "cd " str "; gdb -i=mi"))
  (setq compile-command
	(concat "cd " str "; make")))

(defun compile-leave ()
  (interactive)
  (setq compilation-finish-function nil))

(defun compile-close ()
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

(defun gdb-file (fname)
  "Set the compile command to build a selected directory"
  (interactive "fExecutable: ")
  (gdb (concat "gdb -i=mi " fname)))

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
;; Formatings
;;

; style I want to use in c++ mode
(c-add-style "jpb"
	     '("stroustrup"
	       (indent-tabs-mode . nil)        ; use spaces rather than tabs
	       (c-basic-offset . 4)            ; indent by four spaces
	       (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
				   (brace-list-open . 0)
                                   (innamespace . 0)
				   (statement-case-open . +)))))

(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "jpb")))

(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (c-set-offset 'innamespace 0)
            (setq indent-tabs-mode nil)))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-hook 'c-mode-common-hook #'auto-fill-mode)

(defun jpb-enable-cpp-headers ()
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)))

(defun jpb-disable-cpp-headers ()
  (interactive)
  (setq auto-mode-alist (remove '("\\.h\\'" . c++-mode) auto-mode-alist)))

;;
;; QML
;;

(add-to-list 'auto-mode-alist '("\\.qml\\'" . js-mode))

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
;; Rainbow
;;
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; Coffee
;;
(require 'coffee-mode)

;;
;; Update copyright
;;

(add-hook 'before-save-hook 'copyright-update)

(add-to-list 'compilation-error-regexp-alist '("^In file \\(.*?\\):\\([0-9]+\\)$" 1 2))
(add-to-list 'compilation-error-regexp-alist '("^    #[0-9]+ 0x[[:xdigit:]]+ in .* \\(.*?\\):\\([0-9]+\\)$" 1 2))

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

;;
;; Clojure
;;
(defun jpb-cider-connect ()
  (interactive)
  (cider-connect "localhost" "7888"))

;; (use 'figwheel-sidecar.repl-api)
;; (cljs-repl)
(defun jpb-cider-enable-figwheel-cljs ()
  (interactive)
  (cider-interactive-eval
   "(use 'figwheel-sidecar.repl-api) (cljs-repl)"
   nil
   nil))

;;
;; Octave
;;
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(provide 'jpb-devel)
