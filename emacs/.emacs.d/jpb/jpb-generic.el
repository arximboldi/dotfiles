;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolivar Puente
;;

;;
;; Emacs default options in another file.
;; Needs to be first thing evah!
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;
;; Term
;;

;; Needed for multi-term to work
(if (not (fboundp 'ad-advised-definition-p))
    (defun ad-advised-definition-p (definition)
      "Return non-nil if DEFINITION was generated from advice information."
      (if (or (ad-lambda-p definition)
              (macrop definition)
              (ad-compiled-p definition))
          (let ((docstring (ad-docstring definition)))
            (and (stringp docstring)
                 (get-text-property 0 'dynamic-docstring-function docstring))))))

(require 'multi-term)
(dolist (hook (list 'term-mode-hook))
  (add-hook hook '(lambda () (yas-minor-mode -1))))

;;
;; Shell
;;
(require 'ansi-color)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook
          '(lambda ()
             (toggle-truncate-lines 1)
             (set-process-query-on-exit-flag (get-process "shell") nil)))
(setq comint-prompt-read-only t)

(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Fix emacs not finding commands in my custom path
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (set-exec-path-from-shell-PATH))

;; Fix shit

(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)

(require 'uniquify)
(require 'zoom-frm)

;; Override stale locks for desktop
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(defun rename-current-file-or-buffer ()
  (interactive)
  (if (not (buffer-file-name))
      (call-interactively 'rename-buffer)
    (let ((file (buffer-file-name)))
      (with-temp-buffer
        (set-buffer (dired-noselect file))
        (dired-do-rename)
        (kill-buffer nil))))
  nil)

(defun join-lines ()
  (interactive)
  (setq fill-column 100000)
  (fill-paragraph nil)
  (setq fill-column 78))

(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun scroll-down-keep-cursor ()
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
  (interactive)
  (scroll-up 1))

(defun small-shell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (shell))

(defun small-term ()
  (interactive)
  (multi-term-dedicated-toggle))

(defun small-eshell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (eshell))

(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'jpb-generic)
