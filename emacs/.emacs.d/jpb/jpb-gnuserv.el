;;
;; GNU Emacs configuration file
;; ----------------------------
;;
;;   Author: Juan Pedro Bolivar Puente
;;

;;
;; Fixes the raise-frame no-go problem
;;
(defadvice raise-frame (after make-it-work (&optional frame) activate)
    "Work around some bug? in raise-frame/Emacs/GTK/Metacity/something.
     Katsumi Yamaoka <yamaoka@jpl.org> posted this in
     http://article.gmane.org/gmane.emacs.devel:39702"
     (call-process
     "wmctrl" nil nil nil "-i" "-R"
     (frame-parameter (or frame (selected-frame)) 'outer-window-id)))

;;
;;  Start GNUServe process when starting up.  This lets us send new files
;; to previously spawned emacs process.
;;

(load "gnuserv-compat")
(load-library "gnuserv")
(gnuserv-start)

;; When loading files reuse existing frames.
(setq gnuserv-frame (car (frame-list)))

;; Dont bother me about being a gnuserv file
(setq gnuserv-kill-quietly t)

;; Bring emacs to front
(add-hook 'gnuserv-visit-hook
	  (lambda ()
	    (raise-frame)))
	    ;;(x-focus-frame nil)))

(provide 'jpb-gnuserv)
