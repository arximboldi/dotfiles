
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

  (global-set-key (kbd "C-c C-g") 'gptel-send)

  ;; Global keybindings for various GPTel commands
  (global-set-key (kbd "C-c g c") #'gptel)             ;; Open GPTel (or switch to it)
  (global-set-key (kbd "C-c g s") #'gptel-send)             ;; Open GPTel (or switch to it)
  (global-set-key (kbd "C-c g m") #'gptel-menu)             ;; Open GPTel (or switch to it)
  (global-set-key (kbd "C-c g r") #'@gptel-send-region) ;; Send selected region
  (global-set-key (kbd "C-c g b") #'@gptel-send-buffer) ;; Send entire buffer

  (global-set-key (kbd "C-c g t") #'@gptel-switch-to))

(use-package elysium)
(use-package magit-gptcommit) ;; doesn't support 'gptel on main branch yet...

(use-package aider
  :config
  (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-ai)
