;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolivar Puente
;;

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/jpb") 0)

(server-start)
(remove-hook 'kill-buffer-query-functions
	     'server-kill-buffer-query-function)

(defun author-name  () "Juan Pedro Bolívar Puente")
(defun author-email () "raskolnikov@es.gnu.org")

(add-to-list 'load-path (expand-file-name "~/.emacs.d/jpb"))
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

(require 'jpb-generic)
(require 'jpb-mail)
(require 'jpb-devel)
(require 'jpb-cpp11)
(require 'jpb-keys)

(desktop-save-mode 1)
(add-hook 'find-file-hook 'desktop-auto-save-set-timer)
(add-hook 'after-save-hook 'desktop-auto-save-set-timer)
