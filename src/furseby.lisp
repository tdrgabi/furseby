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

(load (compile-file "/home/tudor/git/furseby/master/src/core.lisp"))
(load (compile-file "/home/tudor/git/furseby/master/src/plugins/gutenberg.lisp"))

; ### Package definition ###

(defpackage :furseby
  (:use :cl :gtk :gdk :gobject 
        :furseby.core
        :furseby.plugins.gutenberg))
(in-package :furseby)


; ### GUI code ###

(defun run ()
  (within-main-loop
    (let ((builder (make-instance 'builder)))
          (builder-add-from-file builder (namestring (make-pathname :name "furseby"
                                                                    :type "glade"
                                                                    :directory '(:relative "gui"))))
      ; match the controls that matter to vars
      (let ((window (builder-get-object builder "window"))
            (search-field (builder-get-object builder "search-field"))
            (result-label (builder-get-object builder "result-label"))
            (result-view (builder-get-object builder "result-view"))
            (prev (builder-get-object builder "prev"))
            (copy (builder-get-object builder "copy"))
            (next (builder-get-object builder "next")))
           ; on window close keep the gtk running. helps with debugging
           (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (leave-gtk-main)))
           (widget-show window)))))


;### How to run ###

; Sample results from a plugin:

'(("Flaubert, Gustave, 1821-1880 [Author]Schurig, Arthur, 1870-1929 [Translator]"  "Frau Bovary" "/ebooks/15711")
 ("Flaubert, Gustave, 1821-1880 [Author]" "Madame Bovary" "/ebooks/14155")
 ("Flaubert, Gustave, 1821-1880 [Author]Schurig, Arthur, 1870-1929 [Translator]"  "Frau Bovary" "/ebooks/15711")
 ("Flaubert, Gustave, 1821-1880 [Author]" "Madame Bovary" "/ebooks/2413")
 ("Flaubert, Gustave, 1821-1880 [Author]"  "Madame BovaryA Tale of Provincial Life" "/ebooks/27575"))


; ## Left To do ##
; * error handling in core
; * show some results in gui
; * research how packages are normally loaded (google.com/codesearch) since quickload takes a while

;(search-all-sites "Bovary")

;(run)
 
