;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolívar Puente
;;

(setq user-mail-address "raskolnikov@gnu.org"
      user-full-name "Juan Pedro Bolivar Puente")

(require 'notmuch)

(setq notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox and not tag:lists" :key "i" :sort-order newest-first)
     (:name "lists" :query "tag:inbox and tag:lists" :key "l" :sort-order newest-first)
     (:name "gnu" :query "tag:inbox and tag:gnu" :key "g" :sort-order newest-first)
     (:name "riseup" :query "tag:inbox and tag:riseup" :key "r" :sort-order newest-first)
     (:name "gmail" :query "tag:inbox and tag:gmail" :key "G" :sort-order newest-first)
     (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first)
     (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
     (:name "sent" :query "tag:sent" :key "t" :sort-order newest-first)
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a" :sort-order newest-first)))

;;
;; Multiple identitites
;;

;; Each entry is:
;;
;;  - name
;;  - inherited name
;;  - Name <emi@a.il>
;;  - 'Organization' header
;;  - Extra headers
;;  - Body
;;  - Signature

(setq gnus-alias-identity-alist
      `(("gmail" nil
         "Juan Pedro Bolívar Puente <magnicida@gmail.com>" nil
         (("Fcc" . "mail/gmail/Sent"))
         nil nil)
        ("gnu" nil
         "Juan Pedro Bolívar Puente <raskolnikov@gnu.org>" nil
         (("Fcc" . "mail/gnu/Sent"))
         nil nil)
        ("riseup" nil
         "Juan Pedro Bolívar Puente <juanpe@riseup.net>"
         nil (("Fcc" . "mail/riseup/Sent"))
         nil nil)))

(setq gnus-alias-default-identity "gnu")
(setq gnus-alias-identity-rules
      '(("gmail" ("any" "magnicida@gmail.com" both) "gmail")
        ("riseup" ("any" "juanpe@riseup.net" both) "riseup")))

(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(setq notmuch-crypto-process-mime t)
(setq notmuch-poll-script "~/usr/bin/sync-email")
(setq notmuch-show-logo nil)
(setq notmuch-search-oldest-first nil)

(setq sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

(setq send-mail-function 'sendmail-send-it)

;;
;; News reading
;;

(require 'gnus)
(setq gnus-select-method '(nntp "news.eternal-september.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))

(setq gnus-always-read-dribble-file t)

(setq message-directory "~/mail/")
(setq gnus-directory "~/news/")
(setq nnfolder-directory "~/mail/archive")

;;
;; Jabber
;;

(setq jabber-account-list
      '(("magnicida@gmail.com"
         (:network-server . "talk.google.com")
         (:port . 443)
         (:connection-type . ssl))))

(provide 'jpb-mail)
