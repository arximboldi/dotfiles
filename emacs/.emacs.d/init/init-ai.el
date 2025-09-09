
(require 'gptel)

(setq gptel-backends (list (gptel-make-gh-copilot "Copilot")))
(setq gptel-backend (gptel-make-gh-copilot "Copilot"))
(setq gptel-default-backend "Copilot")
(setq gptel-default-mode 'org-mode)

(defun @gptel-switch-to ()
    "Switch to the existing GPTel chat buffer, or raise an error if none is found."
    (interactive)
    (let ((buf (get-buffer "*Copilot*")))
      (if buf
          (switch-to-buffer buf)
        (gptel "Copilot"))))

(defun @gptel-send-buffer ()
  "Send the entire current buffer content to the GPTel buffer."
  (interactive)
  (gptel-send-region (point-min) (point-max)))

(defun @gptel-send-region (start end)
  "Send the selected region between START and END to the current GPTel buffer."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (buf (get-buffer "*Copilot*")))
    (if buf
        (with-current-buffer buf
          (goto-char (point-max))
          (insert text)
          (gptel-send))
      (message "GPTel buffer doesn't exist. Use `C-c g c` to create one."))))

(with-eval-after-load 'gptel
  ;; Local keybindings in gptel-mode
  (define-key gptel-mode-map (kbd "C-<return>") #'gptel-send)

  (define-key gptel-mode-map (kbd "C-c m")   #'gptel-menu)
  (define-key gptel-mode-map (kbd "C-c M-n") #'gptel-history-next)
  (define-key gptel-mode-map (kbd "C-c M-p") #'gptel-history-previous)

  (global-set-key (kbd "C-c g g") #'gptel-send)

  ;; Global keybindings for various GPTel commands
  (global-set-key (kbd "C-c g c") #'gptel)             ;; Open GPTel (or switch to it)
  (global-set-key (kbd "C-c g s") #'gptel-send)             ;; Open GPTel (or switch to it)
  (global-set-key (kbd "C-c g m") #'gptel-menu)             ;; Open GPTel (or switch to it)
  (global-set-key (kbd "C-c g r") #'@gptel-send-region) ;; Send selected region
  (global-set-key (kbd "C-c g b") #'@gptel-send-buffer) ;; Send entire buffer

  (global-set-key (kbd "C-c g t") #'@gptel-switch-to))

(use-package elysium)
(use-package magit-gptcommit) ;; doesn't support 'gptel on main branch yet...

;;(use-package aider
;;  :config
;;  (global-set-key (kbd "C-c a") 'aider-transient-menu))

;;(use-package copilot
;;  :ensure t
;;  :hook (prog-mode . copilot-mode))

(require 'copilot)

(add-hook 'prog-mode-hook 'copilot-mode)

;;(define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "M-p") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "M-]") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "M-[") 'copilot-previous-completion)
(define-key copilot-completion-map (kbd "M-o") 'copilot-clear-overlay)
;;(define-key copilot-completion-map (kbd "C-c C-c") 'copilot-clear-overlay)
(global-set-key (kbd "M-P") 'copilot-panel-complete)

(add-to-list 'copilot-indentation-alist '(prog-mode 4))
(add-to-list 'copilot-indentation-alist '(org-mode 2))
(add-to-list 'copilot-indentation-alist '(text-mode 2))
(add-to-list 'copilot-indentation-alist '(closure-mode 2))
(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word))
;; (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))
;; (define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion))
;; (define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion))

;;(use-package claude-code :ensure t
;;  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
;;  :config (claude-code-mode)
;;  :bind-keymap ("C-c c" . claude-code-command-map))

(provide 'init-ai)
