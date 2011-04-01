
(ql:quickload "cl-gtk2-gtk")
(ql:quickload "drakma")
(ql:quickload "cl-libxml2")
(ql:quickload "lisp-unit")
(ql:quickload "url-rewrite")


(load (compile-file "/home/tudor/git/furseby/src/core.lisp"))
(load (compile-file "/home/tudor/git/furseby/src/plugins/gutenberg.lisp"))


(defpackage :fur-seby
  (:use :cl :gtk :gdk :gobject :iter :drakma :html :puri :xpath :iter :url-rewrite
        :furseby.core
        :furseby.plugins.gutenberg))

(in-package :fur-seby)


(defun run ()
  (within-main-loop
    (let ((builder (make-instance 'builder)))
          (builder-add-from-file builder (namestring "/home/tudor/furseby.glade"))
      (let ((window (builder-get-object builder "window"))
            (search-field (builder-get-object builder "search-field"))
            (result-label (builder-get-object builder "result-label"))
            (result-view (builder-get-object builder "result-view"))
            (prev (builder-get-object builder "prev"))
            (copy (builder-get-object builder "copy"))
            (next (builder-get-object builder "next")))
           (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
           (widget-show window)))))


;(load (compile-file "/home/tudor/git/furseby/src/furseby.lisp"))
;(search-all-sites "Bovary")

;(run)

