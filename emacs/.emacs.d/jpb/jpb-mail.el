;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "magnicida@gmail.com"
      user-full-name "Juan Pedro Bolivar Puente")
(setq smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465)
(setq send-mail-function 'smtpmail-send-it)

(provide 'jpb-mail)
