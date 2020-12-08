;;; emacs config file -- Juan Pedro Bolívar Puente

;;
;; Emacs default options in another file.
;; Needs to be first thing evah!
;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;
;; Setup desktop mode and notify the world when emacs is ready
;;

(defun @notify (msg)
  (when (not (eq system-type 'darwin))
    (start-process "notify" nil
                   "notify-send" "-i" "emacs" "Emacs" msg)))

(add-hook 'emacs-startup-hook
          (lambda () (@notify "Ready to roll!")))

;;
;; Compat for apt-utils
;;
(defun make-local-hook (hook)
  nil)

;;
;; Term
;;
(add-hook 'term-mode-hook
          '(lambda() (set (make-local-variable 'global-hl-line-mode) nil)))

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

(add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))

;;
;; Shell
;;

;; (require 'xterm-color)
;; (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;; (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
;; (setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)

(ignore-errors
  (require 'ansi-color)
  (defun @colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook '@colorize-compilation-buffer))

(add-hook 'shell-mode-hook
          '(lambda ()
             (toggle-truncate-lines 1)
             (set-process-query-on-exit-flag (get-process "shell") nil)))

;; (defun colorize-compilation-buffer ()
;;   (when (eq major-mode 'compilation-mode)
;;     (ansi-color-apply-on-region compilation-filter-start (point-max))))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Fix emacs not finding commands in my custom path
(defun set-exec-path-from-shell-PATH ()
  (let* ((cmd
          ;; try use the shell from nixos
          (if (file-exists-p "/run/current-system/sw/bin/bash")
              "/run/current-system/sw/bin/bash -i -c 'echo $PATH' 2> /dev/null"
            ;; on mac we want to use the shell from brew, if installed
            (if (file-exists-p "/usr/local/bin/bash")
                "/usr/local/bin/bash -i -c 'echo $PATH' 2> /dev/null"
              "bash -i -c 'echo $PATH' 2> /dev/null")))
         (path-from-shell (shell-command-to-string cmd)))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Fix shit

(setq shell-file-name "bash")
(setq shell-command-switch "-c")

(defadvice compile (around use-bashrc activate)
  (let ((shell-command-switch "-ic"))
    ad-do-it))

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

(defun @join-lines ()
  (interactive)
  (setq fill-column 100000)
  (fill-paragraph nil)
  (setq fill-column 78))

(defun @reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun @indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun @scroll-down-keep-cursor ()
  (interactive)
  (scroll-down 1))

(defun @scroll-up-keep-cursor ()
  (interactive)
  (scroll-up 1))

(defun @small-shell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (shell))

(defun @small-shell-toggle ()
  (interactive)
  (shell-toggle nil)
  (when (eq (current-buffer) shell-toggle-shell-buffer)
    (shrink-window (- (window-height) 12))))

(defun @small-shell-toggle-cd ()
  (interactive)
  (shell-toggle-cd)
  (when (eq (current-buffer) shell-toggle-shell-buffer)
    (shrink-window (- (window-height) 12))))

(defun @small-term ()
  (interactive)
  (multi-term-dedicated-toggle))

(defun @small-eshell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (eshell))

(defun @helm-mu-completing-read (prompt
                                    collection
                                    &optional
                                    predicate require-match
                                    initial-input hist def
                                    inherit-input-method)
  (helm--completing-read-default prompt collection predicate t
                                initial-input hist def
                                inherit-input-method))
(setq mu4e-completing-read-function '@helm-mu-completing-read)

(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; Update current list of installed packages
;;
(defun @dotfiles-save-installed-packages ()
  (interactive)
  (with-temp-file "~/dotfiles/emacs-installed-packages"
    (print package-activated-list (current-buffer))))

;;
;; Show mode-line as inactive when no focus. Really helps when using
;; xmonad, IMHO
;;
(add-hook 'focus-out-hook
          (lambda ()
            (copy-face 'mode-line-inactive 'mode-line)))
(add-hook 'focus-in-hook
          (lambda ()
            (custom-theme-recalc-face 'mode-line)))

;;
;; On terminal frames, do not set the background color
;;
;; (defun on-frame-open (&optional frame)
;;   "If the FRAME created in terminal don't load background color."
;;   (unless (display-graphic-p frame)
;;     (set-face-background 'default "unspecified-bg" frame)))
;;
;; (add-hook 'after-make-frame-functions 'on-frame-open)

;;
;; Reload dir locasl
;; https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables#13096
;;

(defun @reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defmacro comment (&rest a))

(defun @switch-theme ()
  (interactive)
  (let ((prev-themes custom-enabled-themes))
    (call-interactively 'load-theme)
    (dolist (theme prev-themes)
      (disable-theme theme))))

(provide 'init-generic)
