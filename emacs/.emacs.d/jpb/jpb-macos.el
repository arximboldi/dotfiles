;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/usr/local/bin/gls")

(setq
 ns-command-modifier 'meta         ; Apple/Command key is Meta
 ns-alternate-modifier nil         ; Option is the Mac Option key
 ns-use-mac-modifier-symbols  nil) ; display standard Emacs modifier symbols

(provide 'jpb-macos)
