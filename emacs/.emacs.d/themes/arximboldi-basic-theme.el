;;; emacs theme -- Juan Pedro Bolívar Puente

(deftheme arximboldi-basic
  "Default emacs theme but flatter")

(custom-theme-set-faces
 'arximboldi-basic
 '(fringe ((t nil)))
 '(mode-line ((t (:background "grey90" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey90" :foreground "grey60" :weight light))))
 '(vertical-border ((((type ns tty x mac)) (:inherit mode-line-inactive :inverse-video t))))
 '(font-lock-comment-face ((t (:foreground "grey50")))))

(provide-theme 'arximboldi-basic)
