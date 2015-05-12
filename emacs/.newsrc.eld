;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-options '"")

(setq gnus-newsrc-last-checked-date '"Tue, 12 May 2015 00:37:36 +0200")

(setq gnus-newsrc-alist '(("nntp+news.gmane.org:gmane.linux.audio.devel" 6 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.linux.audio.announce" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.linux.audio.users" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.gnu.psychosynth.devel" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.gnu.psychosynth.user" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.lang.haskell.cafe" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.lang.haskell.beginners" 3 nil nil "nntp:news.gmane.org") ("nndraft:drafts" 6 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode)))) ("comp.std.c++" 3 nil) ("nntp+news.gmane.org:gmane.comp.lang.c++.isocpp.features" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.lang.c++.isocpp.general" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.lang.c++.isocpp.proposals" 3 nil nil "nntp:news.gmane.org") ("nntp+news.gmane.org:gmane.comp.lang.c++.isocpp.ub" 3 nil nil "nntp:news.gmane.org") ("comp.lang.c++.moderated" 3 nil)))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "~/Mail/archive") (nnfolder-active-file "~/Mail/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))

(setq gnus-zombie-list '("nntp+news.gmane.org:gmane.recreation.dance.tango.tango-a" "nntp+news.gmane.org:gmane.comp.lib.hpx.devel" "nntp+news.gmane.org:gmane.comp.gis.open-location-code" "nntp+news.gmane.org:gmane.comp.lib.hpx.user" "nntp+news.gmane.org:gmane.comp.apache.falcon.scm" "nntp+news.gmane.org:gmane.ietf.netmod" "nntp+news.gmane.org:gmane.comp.handhelds.freecalypso" "nntp+news.gmane.org:gmane.comp.apache.falcon.devel"))
