;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

;;
;; GIT
;;
(add-hook 'magit-status-mode-hook 'magit-filenotify-mode)

;;
;; Flymake
;;

(require 'flymake)

(defun flymake-pyflakes-init ()
  ;; Make sure it's not a remote buffer or flymake would not work
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(setq flymake-allowed-file-name-masks '(("\\.py\\'"  flymake-pyflakes-init)
					("\\.tex\\'" flymake-simple-tex-init)
					("\\.xml\\'" flymake-xml-init)
					("\\.cpp\\'" flymake-simple-make-init)))

(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-g0" "-r" "-l" (expand-file-name "~/.emacs.d/chktexrc")
		       "-I" "-q" "-v0" file-name)))

(push '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)" 1 2 3 4)
      flymake-err-line-patterns)


;;
;; Yasnippet
;;

(require 'yasnippet)
(yas-global-mode 1)

;;
;; Python
;;
(add-hook 'python-mode-hook
	  (lambda ()
	    (unless (eq buffer-file-name nil) (flymake-mode t))))


;;
;; LaTeX
;;

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flymake-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(add-hook 'rst-mode-hook 'auto-fill-mode)
(add-hook 'rst-mode-hook 'flyspell-mode)

(setq reftex-plug-into-AUCTeX t)
(setq-default TeX-master nil)


;;
;; C++
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
(defun jpb-gdb ()
  (interactive)
  (if (buffer-live-p gud-comint-buffer)
      (gdb-restore-windows)
    (call-interactively 'gdb)))
(setq gdb-many-windows t)

;;
;; Formatings
;;

(add-hook 'c++-mode-hook
          (function (lambda ()
		      (c-set-style "stroustrup")
		      (setq c-basic-offset 4)
		      (c-set-offset 'innamespace 0)
		      (setq indent-tabs-mode nil))))

(add-hook 'php-mode-hook
          (function (lambda ()
		      (c-set-style "bsd")
		      (setq c-basic-offset 4)
		      (c-set-offset 'innamespace 0)
		      (setq indent-tabs-mode nil))))

(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (auto-fill-mode)
		      ;;(doxymacs-mode)
		      )))

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
    (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
    (when (gtags-root-dir)
      (gtags-update)))
(add-hook 'after-save-hook #'gtags-update-hook)

(defun compile-gtags (str)
  "compile etags for the current project"
  (interactive "DSources directory: ")
  (compile (concat "cd " (expand-file-name str) "; gtags")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;;
;; Rainbow
;;

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(cl-loop
 for index from 1 to (- rainbow-delimiters-max-face-count 1) do
 (let* ((face (intern (format "rainbow-delimiters-depth-%d-face" index)))
        (total rainbow-delimiters-max-face-count)
        (perc (max (- (+ total 40) (* index 4)) 20)))
   (set-face-attribute face nil :foreground (format "gray%d" perc))))

(provide 'jpb-devel)

;;
;; Coffee
;;
(require 'coffee-mode)

;;
;; Update copyright
;;

(add-hook 'before-save-hook 'copyright-update)
