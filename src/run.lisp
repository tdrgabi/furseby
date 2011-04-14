; ### Used libraries ###
(ql:quickload "cl-gtk2-gtk")
(ql:quickload "cl-libxml2")
(ql:quickload "lisp-unit")
(ql:quickload "url-rewrite")
; ### Loading packages ###
(defpackage :furseby.core
  (:use :cl :html :puri :xpath)
  (:export :search-site
           :search-all-sites
           :*sites*
           :*offline-dev*
           :make-site
           :make-book))
(defpackage :furseby.plugins.gutenberg
  (:use :cl :xpath :url-rewrite
        :furseby.core))
(defpackage :furseby.plugins.audio
  (:use :cl :xpath :url-rewrite
        :furseby.core))
(defpackage :furseby
  (:use :cl :gtk :gdk :gobject 
        :furseby.core
        :furseby.plugins.gutenberg
        :furseby.plugins.audio)
  (:export :run))
(load (compile-file "src/core.lisp"))
(load (compile-file "src/plugins/gutenberg.lisp"))
(load (compile-file "src/plugins/audio.lisp"))
(load (compile-file "src/furseby.lisp"))
(in-package COMMON-LISP-USER)
(defun main ()
  (run))



(sb-ext:save-lisp-and-die "furseby" :executable t :toplevel 'main)
