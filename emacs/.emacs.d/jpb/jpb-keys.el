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

(require 'ido-ubiquitous)
(ido-ubiquitous)

(require 'smex)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
	      " [No match]"
	      " [Matched]"
	      " [Not readable]"
	      " [Too big]"
	      " [Confirm]")))

(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(defun jpb-smex-load-and-run ()
  (interactive)
  (or (boundp 'smex-cache)
      (smex-initialize))
  (global-set-key [(meta x)] 'smex)
  (smex))
(global-set-key "\M-x" 'jpb-smex-load-and-run)

;;
;; Gtags
;;

(global-set-key (kbd "C-c C-f")   'gtags-find-file)
(global-set-key (kbd "C-c C-t")   'gtags-find-pattern)
(global-set-key (kbd "M-.")       'gtags-find-tag)
(global-set-key (kbd "M-,")       'gtags-find-rtag)
(global-set-key (kbd "C-x 4 .")   'gtags-find-tag-other-window)

(add-hook 'gtags-select-mode-hook
	  (lambda ()
	    (local-set-key (kbd "RET") 'gtags-select-tag)))

;;
;; Key bindings
;;

(global-set-key [f7]    'small-term)
(global-set-key [f8]    'compile)
(global-set-key [f9]    'jpb-gdb)
(global-set-key [C-f10] 'indent-buffer)

;; Windmode rocks
(global-set-key [s-left]  'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-down]  'windmove-down)
(global-set-key [s-up]    'windmove-up)

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
		 (local-set-key (kbd "C-c C-t")  'gtags-find-pattern)))))

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

(provide 'jpb-keys)
