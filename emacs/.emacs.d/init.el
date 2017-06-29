;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolivar Puente
;;

(server-start)
(remove-hook 'kill-buffer-query-functions
	     'server-kill-buffer-query-function)

(defun author-name  () "Juan Pedro Bolívar Puente")
(defun author-email () "raskolnikov@es.gnu.org")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/cc-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))
(add-to-list 'load-path (expand-file-name "~/.guix-profile/share/emacs/site-lisp"))
(require 'guix-autoloads nil t)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(require 'init-generic)
(require 'init-mail)
(require 'init-devel)
(require 'init-cpp11)
(require 'init-keys)
(when (eq system-type 'darwin)
  (require 'init-macos))

(desktop-save-mode 1)
(add-hook 'find-file-hook 'desktop-auto-save-set-timer)
(add-hook 'after-save-hook 'desktop-auto-save-set-timer)
