
(deftheme jpb-basic-theme
  "Default emacs theme but flatter")

(custom-theme-set-faces
   'jpb-basic-theme
   '(fringe ((t nil)))
   '(mode-line ((t (:background "grey90" :foreground "black"))))
   '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey60" :weight light))))
   '(vertical-border ((((type ns tty x mac)) (:inherit mode-line-inactive :inverse-video t))))
   '(font-lock-comment-face ((t (:foreground "grey50"))))
   )

(provide-theme 'jpb-basic-theme)
