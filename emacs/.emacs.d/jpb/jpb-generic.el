;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolivar Puente
;;

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

;; Emacs default options in another file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

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

(defun small-eshell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (eshell))

(defun small-window (shellcmd)
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (shellcmd))

(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'jpb-generic)
