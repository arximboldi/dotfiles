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
;; Setup desktop mode and notify the world when emacs is ready
;;
(defun jpb-notify (msg)
  (start-process "notify" nil
                 "notify-send" "-i" "emacs" "Emacs" msg))

(add-hook 'emacs-startup-hook
          (lambda () (jpb-notify "Ready to roll!")))

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
(require 'xterm-color)
(add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
(setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions))
(setq font-lock-unfontify-region-function 'xterm-color-unfontify-region)
(add-hook 'shell-mode-hook
          '(lambda ()
             (toggle-truncate-lines 1)
             (set-process-query-on-exit-flag (get-process "shell") nil)))

(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Fix emacs not finding commands in my custom path
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "bash -i -c 'echo $PATH' 2> /dev/null")))
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

(defun small-shell-toggle ()
  (interactive)
  (shell-toggle nil)
  (when (eq (current-buffer) shell-toggle-shell-buffer)
    (shrink-window (- (window-height) 12))))

(defun small-shell-toggle-cd ()
  (interactive)
  (shell-toggle-cd)
  (when (eq (current-buffer) shell-toggle-shell-buffer)
    (shrink-window (- (window-height) 12))))

(defun small-term ()
  (interactive)
  (multi-term-dedicated-toggle))

(defun small-eshell ()
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (shrink-window (- (window-height) 12))
  (eshell))

(defun jpb-helm-mu-completing-read (prompt
                                    collection
                                    &optional
                                    predicate require-match
                                    initial-input hist def
                                    inherit-input-method)
  (helm--completing-read-default prompt collection predicate t
                                initial-input hist def
                                inherit-input-method))
(setq mu4e-completing-read-function 'jpb-helm-mu-completing-read)

(add-hook 'write-file-hooks 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; Update current list of installed packages
;;
(defun dotfiles-save-installed-packages ()
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

(defun reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;;
;; Fira code
;;
;; https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
;;

(defmacro comment (&rest a))

(comment
 ;; This works when using emacs --daemon + emacsclient
 (add-hook 'after-make-frame-functions
           (lambda (frame)
             (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))

 ;; This works when using emacs without server/client
 (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
 ;; I haven't found one statement that makes both of the above
 ;; situations work, so I use both for now

 (defconst fira-code-font-lock-keywords-alist
   (mapcar (lambda (regex-char-pair)
             `(,(car regex-char-pair)
               (0 (prog1 ()
                    (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ;; The first argument to concat is a string containing a literal tab
                                    ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
           '(("\\(www\\)"                   #Xe100)
             ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
             ("\\(\\*\\*\\*\\)"             #Xe102)
             ("\\(\\*\\*/\\)"               #Xe103)
             ("\\(\\*>\\)"                  #Xe104)
             ("[^*]\\(\\*/\\)"              #Xe105)
             ("\\(\\\\\\\\\\)"              #Xe106)
             ("\\(\\\\\\\\\\\\\\)"          #Xe107)
             ("\\({-\\)"                    #Xe108)
             ("\\(\\[\\]\\)"                #Xe109)
             ("\\(::\\)"                    #Xe10a)
             ("\\(:::\\)"                   #Xe10b)
             ("[^=]\\(:=\\)"                #Xe10c)
             ("\\(!!\\)"                    #Xe10d)
             ("\\(!=\\)"                    #Xe10e)
             ("\\(!==\\)"                   #Xe10f)
             ("\\(-}\\)"                    #Xe110)
             ("\\(--\\)"                    #Xe111)
             ("\\(---\\)"                   #Xe112)
             ("\\(-->\\)"                   #Xe113)
             ("[^-]\\(->\\)"                #Xe114)
             ("\\(->>\\)"                   #Xe115)
             ("\\(-<\\)"                    #Xe116)
             ("\\(-<<\\)"                   #Xe117)
             ("\\(-~\\)"                    #Xe118)
             ("\\(#{\\)"                    #Xe119)
             ("\\(#\\[\\)"                  #Xe11a)
             ("\\(##\\)"                    #Xe11b)
             ("\\(###\\)"                   #Xe11c)
             ("\\(####\\)"                  #Xe11d)
             ("\\(#(\\)"                    #Xe11e)
             ("\\(#\\?\\)"                  #Xe11f)
             ("\\(#_\\)"                    #Xe120)
             ("\\(#_(\\)"                   #Xe121)
             ("\\(\\.-\\)"                  #Xe122)
             ("\\(\\.=\\)"                  #Xe123)
             ("\\(\\.\\.\\)"                #Xe124)
             ("\\(\\.\\.<\\)"               #Xe125)
             ("\\(\\.\\.\\.\\)"             #Xe126)
             ("\\(\\?=\\)"                  #Xe127)
             ("\\(\\?\\?\\)"                #Xe128)
             ("\\(;;\\)"                    #Xe129)
             ("\\(/\\*\\)"                  #Xe12a)
             ("\\(/\\*\\*\\)"               #Xe12b)
             ("\\(/=\\)"                    #Xe12c)
             ("\\(/==\\)"                   #Xe12d)
             ("\\(/>\\)"                    #Xe12e)
             ("\\(//\\)"                    #Xe12f)
             ("\\(///\\)"                   #Xe130)
             ("\\(&&\\)"                    #Xe131)
             ("\\(||\\)"                    #Xe132)
             ("\\(||=\\)"                   #Xe133)
             ("[^|]\\(|=\\)"                #Xe134)
             ("\\(|>\\)"                    #Xe135)
             ("\\(\\^=\\)"                  #Xe136)
             ("\\(\\$>\\)"                  #Xe137)
             ("\\(\\+\\+\\)"                #Xe138)
             ("\\(\\+\\+\\+\\)"             #Xe139)
             ("\\(\\+>\\)"                  #Xe13a)
             ("\\(=:=\\)"                   #Xe13b)
             ("[^!/]\\(==\\)[^>]"           #Xe13c)
             ("\\(===\\)"                   #Xe13d)
             ("\\(==>\\)"                   #Xe13e)
             ("[^=]\\(=>\\)"                #Xe13f)
             ("\\(=>>\\)"                   #Xe140)
             ("\\(<=\\)"                    #Xe141)
             ("\\(=<<\\)"                   #Xe142)
             ("\\(=/=\\)"                   #Xe143)
             ("\\(>-\\)"                    #Xe144)
             ("\\(>=\\)"                    #Xe145)
             ("\\(>=>\\)"                   #Xe146)
             ("[^-=]\\(>>\\)"               #Xe147)
             ("\\(>>-\\)"                   #Xe148)
             ("\\(>>=\\)"                   #Xe149)
             ("\\(>>>\\)"                   #Xe14a)
             ("\\(<\\*\\)"                  #Xe14b)
             ("\\(<\\*>\\)"                 #Xe14c)
             ("\\(<|\\)"                    #Xe14d)
             ("\\(<|>\\)"                   #Xe14e)
             ("\\(<\\$\\)"                  #Xe14f)
             ("\\(<\\$>\\)"                 #Xe150)
             ("\\(<!--\\)"                  #Xe151)
             ("\\(<-\\)"                    #Xe152)
             ("\\(<--\\)"                   #Xe153)
             ("\\(<->\\)"                   #Xe154)
             ("\\(<\\+\\)"                  #Xe155)
             ("\\(<\\+>\\)"                 #Xe156)
             ("\\(<=\\)"                    #Xe157)
             ("\\(<==\\)"                   #Xe158)
             ("\\(<=>\\)"                   #Xe159)
             ("\\(<=<\\)"                   #Xe15a)
             ("\\(<>\\)"                    #Xe15b)
             ("[^-=]\\(<<\\)"               #Xe15c)
             ("\\(<<-\\)"                   #Xe15d)
             ("\\(<<=\\)"                   #Xe15e)
             ("\\(<<<\\)"                   #Xe15f)
             ("\\(<~\\)"                    #Xe160)
             ("\\(<~~\\)"                   #Xe161)
             ("\\(</\\)"                    #Xe162)
             ("\\(</>\\)"                   #Xe163)
             ("\\(~@\\)"                    #Xe164)
             ("\\(~-\\)"                    #Xe165)
             ("\\(~=\\)"                    #Xe166)
             ("\\(~>\\)"                    #Xe167)
             ("[^<]\\(~~\\)"                #Xe168)
             ("\\(~~>\\)"                   #Xe169)
             ("\\(%%\\)"                    #Xe16a)
             ;; ("\\(x\\)"                   #Xe16b) This ended up being hard to do properly so i'm leaving it out.
             ("[^:=]\\(:\\)[^:=]"           #Xe16c)
             ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
             ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

 (defun add-fira-code-symbol-keywords ()
   (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

 (add-hook 'prog-mode-hook
           #'add-fira-code-symbol-keywords))

(provide 'jpb-generic)
