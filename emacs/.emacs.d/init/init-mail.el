;;; emacs config file -- Juan Pedro Bolívar Puente

(setq user-mail-address "juanpe@sinusoid.es"
      user-full-name "Juan Pedro Bolivar Puente")

(require 'notmuch)


(defun @getmail ()
  (interactive)
  (start-process "getmail" "*getmail*" "getmail"))

(defun @email ()
  (interactive)
  (notmuch-search-by-tag "inbox"))

(setq notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox and not tag:lists" :key "i" :sort-order newest-first)
     (:name "lists" :query "tag:inbox and tag:lists" :key "l" :sort-order newest-first)
     (:name "gnu" :query "tag:inbox and tag:gnu" :key "g" :sort-order newest-first)
     (:name "riseup" :query "tag:inbox and tag:riseup" :key "r" :sort-order newest-first)
     (:name "gmail" :query "tag:inbox and tag:gmail" :key "G" :sort-order newest-first)
     (:name "bronze" :query "tag:inbox and tag:bronze" :key "b" :sort-order newest-first)
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
      `(;; ("gmail" nil
        ;;  "Juan Pedro Bolívar Puente <magnicida@gmail.com>" nil
        ;;  (("Fcc" . "gmail/Sent"))
        ;;  nil nil)
        ("bronze" nil
         "Juan Pedro Bolívar Puente <juanpe@bronze.ai>"
         nil (("Fcc" . "bronze/Sent"))
         nil nil)
        ("gnu" nil
         "Juan Pedro Bolívar Puente <raskolnikov@gnu.org>" nil
         (("Fcc" . "gnu/Sent"))
         nil nil)
        ("riseup" nil
         "Juan Pedro Bolívar Puente <juanpe@riseup.net>"
         nil (("Fcc" . "riseup/Sent"))
         nil nil)
        ("sinusoides" nil
         "Juan Pedro Bolívar Puente <juanpe@sinusoid.es>"
         nil (("Fcc" . "runbox/Sent"))
         nil nil)
        ("sinusoidal" nil
         "Juan Pedro Bolívar Puente <juanpe@sinusoid.al>"
         nil (("Fcc" . "runbox/Sent"))
         nil nil)))

(setq notmuch-identities gnus-alias-identity-alist)

(setq gnus-alias-default-identity "bronze")

(setq gnus-alias-identity-rules
      '(;("gmail" ("any" "magnicida@gmail.com" both) "gmail")
        ("bronze"     ("any" "@bronze.ai" both) "bronze")
        ("riseup"     ("any" "@riseup.net" both) "riseup")
        ("sinusoides" ("any" "@sinusoid.es" both) "sinusoides")
        ("sinusoidal" ("any" "@sinusoid.al" both) "sinusoidal")
        ;;("gnu"        ("any" "@gnu.org" both) "gnu")
        ;;("es.gnu"     ("any" "@es.gnu.org" both) "gnu")
        ))

(add-hook 'message-setup-hook 'gnus-alias-determine-identity)
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(setq notmuch-crypto-process-mime t)
(setq notmuch-poll-script "~/usr/bin/getmail")
(setq notmuch-show-logo nil)
(setq notmuch-search-oldest-first nil)

(require 'sendmail)

(setq sendmail-program "/run/current-system/sw/bin/msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

(setq send-mail-function 'sendmail-send-it)

;;
;; News reading
;;

(require 'gnus)
(require 'nnfolder)

(setq gnus-select-method '(nntp "news.eternal-september.org"))
;;(setq gnus-select-method '(nntp "news.qwest.net"))
;;(setq gnus-select-method '(nntp "usenet.ptd.net"))
(setq gnus-secondary-select-methods
      '((nntp "news.gnus.org")
        (nntp "news.gmane.org")
        (nnfolder "archive"
                  (nnfolder-directory   "~/mail/archive")
                  (nnfolder-active-file "~/mail/archive/active")
                  (nnfolder-get-new-mail nil)
                  (nnfolder-inhibit-expiry t))))

(setq gnus-always-read-dribble-file t)

(setq message-directory "~/mail/")
(setq message-auto-save-directory "~/mail/drafts")
(setq gnus-directory "~/news/")
(setq mail-source-directory "~/mail/")
(setq nnfolder-directory "~/mail/archive")

(setq elmo-archive-folder-path "~/mail"
      elmo-localdir-folder-path "~/mail"
      elmo-search-namazu-default-index-path "~/mail")

(setq gnus-article-sort-functions
      '((not gnus-article-sort-by-date)
        (not gnus-article-sort-by-number)))

;;
;; Wanderlust
;;

(setq wl-message-ignored-field-list '("^.*:"))
(setq wl-message-visible-field-list
      '("^Newsgroups:"
        "^Newsgroup:"
        "^To:"
        "^Cc:"
        "^From:"
        "^Subject:"
        "^Date:"))
(setq wl-summary-width 256)
(defun @wl-summary-prepared-hook ()
  (save-excursion
    (wl-summary-rescan "number" t)))
(add-hook 'wl-summary-prepared-hook '@wl-summary-prepared-hook)

(require 'elmo)
(require 'elmo-search)
(or (assq 'notmuch-elmo elmo-search-engine-alist)
      (elmo-search-register-engine
       'notmuch-elmo 'local-file
       :prog "notmuch"
       :args '("search" "--output=files" "tag:elmo"
               elmo-search-replace-single-quotes)
       :charset 'utf-8))

(setq elmo-folder-update-confirm nil
      elmo-folder-update-threshold 5000
      elmo-msgdb-directory "~/mail/elmo"
      elmo-search-default-engine 'notmuch-elmo)
(setq wl-prefetch-threshold 20000000
      wl-quicksearch-folder "[]"
      wl-score-files-directory "~/mail/elmo/"
      wl-summary-line-format "%T %Y-%M-%D %h:%m %5(%C%)%t %18(%f%)  %s"
      wl-summary-width 256
      wl-default-spec ""
      wl-highlight-highlight-citation-too t
      wl-interactive-exit nil
      wl-nntp-posting-server "news.gmane.org")

;;
;; Jabber
;;

(setq jabber-account-list
      '(("magnicida@gmail.com"
         (:network-server . "talk.google.com")
         (:port . 443)
         (:connection-type . ssl))))

(provide 'init-mail)
