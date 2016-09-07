;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolivar Puente
;;

(defun jpb-toggle-fullscreen ()
  (interactive)
  (toggle-menu-bar-mode-from-frame)
  (toggle-tool-bar-mode-from-frame))

;;
;; IDO
;;

;; (require 'ido-ubiquitous)
;; (ido-ubiquitous)

;; (require 'smex)

;; ;; Display ido results vertically, rather than horizontally
;; (setq ido-decorations
;;       (quote ("\n ▶ " "" "\n   " "\n   ..." "[" "]"
;; 	      " [No match]"
;; 	      " [Matched]"
;; 	      " [Not readable]"
;; 	      " [Too big]"
;; 	      " [Confirm]")))

;; (defun ido-disable-line-trucation ()
;;   (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; (defun jpb-smex-load-and-run ()
;;   (interactive)
;;   (or (boundp 'smex-cache)
;;       (smex-initialize))
;;   (global-set-key [(meta x)] 'smex)
;;   (smex))
;; (global-set-key "\M-x" 'jpb-smex-load-and-run)

;;
;; Helm
;;
(helm-mode 1)
(helm-flx-mode +1)
(helm-fuzzier-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(projectile-global-mode 1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;;
;; Email
;;
(global-set-key (kbd "C-x m") 'notmuch-mua-new-mail)
(define-key notmuch-show-mode-map (kbd "C-.") 'browse-url-at-point)

(define-key notmuch-search-mode-map "S"
  (lambda (&optional beg end)
    "Mark thread as spam"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag (list "+spam" "-inbox") beg end)
    (notmuch-search-next-thread)))

(define-key notmuch-search-mode-map "f"
  (lambda (&optional beg end)
    "Mark thread as important"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag (list "+flagged") beg end)
    (notmuch-search-next-thread)))

(define-key notmuch-search-mode-map "v"
  (lambda (&optional beg end)
    "Mark thread as read"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag (list "-unread") beg end)
    (notmuch-search-next-thread)))

(define-key notmuch-search-mode-map "d"
  (lambda (&optional beg end)
    "Mark thread for deletion"
    (interactive (notmuch-search-interactive-region))
    (notmuch-search-tag (list "+deleted") beg end)
    (notmuch-search-next-thread)))

;;
;; Multiple cursors
;;
(global-set-key (kbd "C-s-c C-s-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;
;; Git
;;
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; Gtags
;;

;; (global-set-key (kbd "C-c C-f")   'gtags-find-file)
;; (global-set-key (kbd "C-c C-t")   'gtags-find-pattern)
;; (global-set-key (kbd "M-.")       'gtags-find-tag)
;; (global-set-key (kbd "M-,")       'gtags-find-rtag)
;; (global-set-key (kbd "C-x 4 .")   'gtags-find-tag-other-window)

;; (add-hook 'gtags-select-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "RET") 'gtags-select-tag)))

(require 'dumb-jump)
(dumb-jump-mode)
(global-set-key (kbd "M-.") 'dumb-jump-go)
(global-set-key (kbd "C-M-.") 'dumb-jump-go-other-window)
(global-set-key (kbd "M-,") 'dumb-jump-back)

;;
;; Key bindings
;;

(global-set-key [f7]    'small-shell-toggle)
(global-set-key [C-f7]  'small-shell-toggle-cd)
(global-set-key [f8]    'compile)
(global-set-key [f9]    'jpb-gdb)
(global-set-key [C-f10] 'indent-buffer)
(global-set-key [f12]   'zoom-window-zoom)

;; Windmode rocks
(global-set-key [C-M-left]  'windmove-left)
(global-set-key [C-M-right] 'windmove-right)
(global-set-key [C-M-down]  'windmove-down)
(global-set-key [C-M-up]    'windmove-up)

;; Scrolling
(global-set-key '[C-M-up] 'scroll-down-keep-cursor)
(global-set-key '[C-M-down] 'scroll-up-keep-cursor)

;; Zooming
(global-set-key (kbd "C-+")      'zoom-frm-in)
(global-set-key [C-kp-add]       'zoom-frm-in)
(global-set-key (kbd "C--")      'zoom-frm-out)
(global-set-key [C-kp-subtract]  'zoom-frm-out)

;; Go to minibuffer
(global-set-key (kbd "C-c m") 'jpb-goto-minibuffer)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

;; Indent on enter
(eval-after-load "cc-mode"
  '(progn
     (add-hook 'c-mode-common-hook 'set-newline-and-indent)
     (add-hook 'c++-mode-common-hook 'set-newline-and-indent)))

(eval-after-load "js-mode"
  '(progn
     (add-hook 'js-mode-hook 'set-newline-and-indent)))

(eval-after-load "python-mode"
  '(progn
     (add-hook 'python-mode-hook
	       (lambda ()
		 (local-set-key (kbd "C-c C-e") 'py-execute-region)
		 ;;(local-set-key (kbd "C-c C-t")  'gtags-find-pattern)
                 ))))

(eval-after-load "shell"
  '(progn
     (add-hook 'shell-mode-hook
	       '(lambda ()
		  (define-key shell-mode-map "\C-m"
		    '(lambda ()
		       (interactive)
		       (comint-send-input)
		       (condition-case err (kill-buffer "*Completions*") ())
		       (message "Input sent")))))))

(global-set-key [f5] 'toggle-php-html-mode)


;;
;; Camelscore
;;
;;   http://www.emacswiki.org/emacs/CamelCase
;;

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore (s)
      (cond ;;((string-match-p "\:"  s) (camelcase s))
	    ((string-match-p "-" s)   (camelcase s)) ;; (colonize s))
	    ((string-match-p "_" s)   (dasherize s))
	    (t                        (underscore s))))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         ;; (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         ;; (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (beg (and (skip-chars-backward "[:alnum:]_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]_-") (point)))
         (txt (buffer-substring beg end))
         (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml)))))

(global-set-key (kbd "C-c a") 'camelscore-word-at-point)

(provide 'jpb-keys)
