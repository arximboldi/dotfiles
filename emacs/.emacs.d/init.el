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

(add-to-list 'load-path (expand-file-name "~/.emacs.d/jpb"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install"))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'jpb-generic)
(require 'jpb-mail)
(require 'jpb-devel)
(require 'jpb-keys)

(desktop-save-mode 1)
(desktop-load-default)
(desktop-read)
