(ql:quickload "cl-gtk2-gtk")
(ql:quickload "cl-libxml2")
(ql:quickload "lisp-unit")
(ql:quickload "url-rewrite")
(load (compile-file "src/core.lisp"))
(load (compile-file "src/plugins/gutenberg.lisp"))
(load (compile-file "src/furseby.lisp"))

(sb-ext:save-lisp-and-die "furseby" :executable t :toplevel 'main)
