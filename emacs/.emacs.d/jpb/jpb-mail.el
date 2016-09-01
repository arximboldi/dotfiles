;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

(setq user-mail-address "raskolnikov@gnu.org"
      user-full-name "Juan Pedro Bolivar Puente")

;;
;; Multiple identitites
;;

;; Each entry is:
;; - name
;; - inherited name
;; - Name <emi@a.il>
;; - 'Organization' header
;; - Extra headers
;; - Body
;; - Signature
(setq gnus-alias-identity-alist
      '(("gmail" nil
         "Juan Pedro Bolivar Puente <magnicida@gmail.com>"
         nil (("Fcc" . "~/mail/gmail/Sent")) nil nil)
        ("gnu" nil
         "Juan Pedro Bolivar Puente <raskolnikov@gnu.org>"
         nil (("Fcc" . "~/mail/gnu/Sent")) nil nil)
        ("gmail" nil
         "Juan Pedro Bolivar Puente <juanpe@riseup.net>"
         nil (("Fcc" . "~/mail/riseup/Sent")) nil nil)))

(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
(setq gnus-alias-default-identity "gnu")

(setq sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

(setq send-mail-function 'sendmail-send-it)

;;
;; News reading
;;

(require 'gnus)
(setq gnus-select-method '(nntp "nntp.aioe.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))

(setq gnus-always-read-dribble-file t)

;;
;; Jabber
;;

(setq jabber-account-list
      '(("magnicida@gmail.com"
         (:network-server . "talk.google.com")
         (:port . 443)
         (:connection-type . ssl))))

(provide 'jpb-mail)
