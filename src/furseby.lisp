; **Furseby** is an extensible webscrapper + GUI

; ## Introduction ##
; Everytime I find a site with free, public domain, books, I bookmark it and never visit it again. I will write a new plugin for each site I find, plugin that knows how to download and present the books I'm searching for. Then, I could simply type a search criteria like "Dante" and the search will be performed on all sites. The goal is to make this async, but for now the searches will be done in serial.

; ## Get the code ##
; [github.com/tdrgabi/furseby](http://github.com/tdrgabi/furseby)
;
; The main file, furseby.lisp, contains the gui code. It includes the core.lisp file which has all the functions required to post the query, parse and get results and the only plugin, currently available, the gutenberg plugin. The whole idea is that for a new site we add just the plugin, there is no need to write any other code. The plugin will define some xpath query and return the results in a predefined form. Thus the gui can show the results.

; ### Packages ###
; * [furseby](furseby.html) gui code
; * [furseby.core](core.html) core functions for applying criterias to all sites and collecting results
; * [furseby.plugins.gutenberg](gutenberg.html) sample plugin to search project gutenberg
 
; ## The code in detail ##
; This package has the standard structure. Libraries, compile and load own packages, define current package and some gui code
;The gui was built with some GUI designer, which outputs .glade files. Then, using cl-gtk2-gtk library, we build the objects from the glade file
 
; ### Used libraries ###

(ql:quickload "cl-gtk2-gtk")
(ql:quickload "cl-libxml2")
(ql:quickload "lisp-unit")
(ql:quickload "url-rewrite")

; ### Loading packages ###

(load (compile-file "src/core.lisp"))
(load (compile-file "src/plugins/gutenberg.lisp"))

; ### Package definition ###

(defpackage :furseby
  (:use :cl :gtk :gdk :gobject 
        :furseby.core
        :furseby.plugins.gutenberg))
(in-package :furseby)


; ### GUI code ###

; All the important controls will be global to the package, if I use it this way, functions like perform-search are a lot simpler
(defparameter *window* nil)
(defparameter *search-field* nil)
(defparameter *result-view* nil)

; I will define a function which reads the text from search-field and perform the search on it
(defun perform-search ()
  ;in criteria we hold the user input
  (let ((criteria (entry-text *search-field*)))
    (format t "Will search for: '~a'~%" criteria)
    ; results are the return values, concatenated from all sources
    (let ((results (search-all-sites criteria)))
      (format t "Results: '~a'~%" results))))

; This is the keypress event handler on search-field, will call the search on ENTER (36)
(defun search-on-enter (event)
   (let ((c (event-key-hardware-keycode event) ))
        (when (equal c 36)
              (perform-search))))

(defun run ()
  (within-main-loop
    (let ((builder (make-instance 'builder)))
          (builder-add-from-file builder (namestring (make-pathname :name "furseby"
                                                                    :type "glade"
                                                                    :directory '(:relative "gui"))))
      (let ((result-label (builder-get-object builder "result-label"))
            (prev (builder-get-object builder "prev"))
            (copy (builder-get-object builder "copy"))
            (next (builder-get-object builder "next")))
           (setf *window* (builder-get-object builder "window"))
           (setf *search-field* (builder-get-object builder "search-field"))
           (setf *result-view* (builder-get-object builder "result-view"))
           (g-signal-connect *search-field* "key-press-event" (lambda (w e) (declare (ignore w)) (search-on-enter e)))
           ; on window close keep the gtk running. helps with debugging
           (g-signal-connect *window* "destroy" (lambda (w) (declare (ignore w)) (gtk-main-quit)))
           (widget-show *window*)))))

;### How to run ###

;bellow are repl functions only, currently the only way to test the program

(run)
 
(sb-thread:release-foreground)

(trace gobject:pointer)

; ## Left To do ##
; * use tree instead of textview (currently in gui but not used)
; * show some results in gui (almost there)
; * research how packages are normally loaded (google.com/codesearch) since quickload takes a while
; * error handling in core

