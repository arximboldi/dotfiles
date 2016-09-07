;;; wombat-theme.el --- Custom face theme for Emacs  -*-coding: utf-8 -*-

;; Copyright (C) 2011-2016 Free Software Foundation, Inc.

;; Author: Kristoffer Grönlund <krig@koru.se>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme jpb-wombatish
  "Medium-contrast faces with a dark gray background.
Adapted, with permission, from a Vim color scheme by Lars H. Nielsen.
Basic, Font Lock, Isearch, Gnus, Message, and Ansi-Color faces
are included.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'jpb-wombatish
   `(default ((,class (:background "#242424" :foreground "#f6f3e8"))))
   `(cursor ((,class (:background "yellow"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#242424" :foreground "#999999"))))
   `(highlight ((,class (:background "#353535"))))
   `(region ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(secondary-selection ((,class (:background "#333366" :foreground
                                               "#f6f3e8"))))
   `(isearch ((,class (:background "#040404" :foreground "#857b6f"))))
   `(lazy-highlight ((,class (:background "#485058" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#444444" :foreground "#f6f3e8"))))
   `(mode-line-inactive ((,class (:background "#444444" :foreground
                                              "#857b6f"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#e5786d"))))
   `(escape-glyph ((,class (:foreground "#ddaa6f" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#e5786d"))))
   `(font-lock-comment-face ((,class (:foreground "#99968b"))))
   `(font-lock-constant-face ((,class (:foreground "#e5786d"))))
   `(font-lock-function-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-keyword-face ((,class (:foreground "#8ac6f2" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#95e454"))))
   `(font-lock-type-face ((,class (:foreground "#92a65e" :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground "#cae682"))))
   `(font-lock-warning-face ((,class (:foreground "#ccaa8f"))))
   ;; Button and link faces
   `(link ((,class (:foreground "#8ac6f2" :underline t))))
   `(link-visited ((,class (:foreground "#e5786d" :underline t))))
   `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low ((,class (:foreground "#99968b"))))
   `(gnus-header-content ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject ((,class (:foreground "#cae682"))))
   `(gnus-header-name ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc ((,class (:foreground "#95e454"))))
   `(message-header-other ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to ((,class (:foreground "#cae682"))))
   `(message-cited-text ((,class (:foreground "#99968b"))))
   `(message-separator ((,class (:foreground "#e5786d" :weight bold))))

   '(custom-button ((t (:background "#333" :foreground "#fff" :box
                                    (:line-width 2 :style released-button)))))
   '(custom-button-mouse ((t (:background "#555" :foreground "#fff"
                                          :box (:line-width 2 :style released-button)))))
   '(diff-added ((t (:inherit diff-changed :foreground "green yellow"))))
   '(diff-file-header ((t (:inverse-video t :weight bold))))
   '(diff-hunk-header ((t (:inherit (magit-item-highlight highlight
                                                          diff-header)))))
   '(diff-removed ((t (:inherit diff-changed :foreground "tomato"))))
   ;; '(ido-first-match ((t (:foreground "#D64937" :weight bold))))
   ;; '(ido-only-match ((t (:foreground "lawn green"))))
   ;; '(ido-subdir ((t (:foreground "#729FCF"))))
   '(magit-blame-header ((t (:inherit magit-section-title :background "#111"))))
   '(magit-item-highlight ((t (:inherit secondary-selection
                                        :background "#111"))))
   '(magit-tag ((t (:background "#5d3703" :foreground "LemonChiffon1"))))
   '(minibuffer-prompt ((t (:foreground "#d64937"))))
   '(mode-line ((t (:background "#d64937" :foreground "black"))))
   '(mode-line-emphasis ((t (:foreground "black" :weight bold))))
   '(mode-line-inactive ((t (:inherit mode-line :background "#2d2d2d"
                                      :foreground "#999" :weight light))))
   '(scroll-bar ((t nil)))
   '(vertical-border ((((type x mac)) (:inherit mode-line-inactive
                                                :foreground "#444444"))))
   '(comint-highlight-prompt ((t nil :inherit nil)))
   '(vline ((t (:inherit highlight))))
   ;; helm stuff
   '(helm-M-x-key ((t (:foreground "#aaa" :underline t))))
   '(helm-buffer-directory ((t (:inherit helm-ff-directory))))
   '(helm-buffer-file ((t (:inherit helm-ff-file))))
   '(helm-buffer-not-saved ((t (:foreground "red"))))
   '(helm-candidate-number ((t (:background "white" :foreground "#333"))))
   '(helm-ff-directory ((t (:inherit font-lock-builtin-face))))
   '(helm-ff-dotted-directory ((t (:foreground "#777"))))
   '(helm-ff-executable ((t (:foreground "yellow green"))))
   '(helm-ff-file ((t nil)))
   '(helm-ff-prefix ((t (:foreground "red"))))
   '(helm-grep-file ((t (:foreground "medium orchid" :underline t))))
   '(helm-selection ((t (:inherit highlight :weight bold))))
   '(helm-separator ((t (:foreground "#444"))))
   '(helm-source-header ((t (:inherit font-lock-builtin-face :weight bold))))
   ;; widgets
   '(widget-field ((t (:background "#333333" :box (:line-width 1 :color "black" :style pressed-button)))))
   ;; notmuch stuff
   '(notmuch-search-count ((t (:inherit default :foreground "magenta"))))
   '(notmuch-search-date ((t (:inherit default :foreground "dark orchid"))))
   '(notmuch-search-matching-authors ((t (:inherit default :foreground "#aaa"))))
   '(notmuch-search-subject ((t (:inherit default))))
   '(notmuch-search-unread-face ((t (:underline "#666" :weight bold))))
   '(notmuch-tree-match-author-face ((t (:inherit notmuch-search-matching-authors))))
   '(notmuch-tree-match-date-face ((t (:inherit notmuch-search-date))) t)))

(custom-theme-set-variables
 'jpb-wombatish
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

(provide-theme 'jpb-wombatish)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; jpb-wombatish-theme.el ends here
