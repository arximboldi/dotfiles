;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

(setq user-mail-address "magnicida@gmail.com"
      user-full-name "Juan Pedro Bolivar Puente")

(setq mail-user-agent 'message-user-agent)
(setq smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)
(setq send-mail-function 'smtpmail-send-it)

(require 'gnus)
(setq gnus-select-method '(nntp "nntp.aioe.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))

(setq gnus-always-read-dribble-file t)

(provide 'jpb-mail)
