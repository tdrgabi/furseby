; == Overview ==

; The standard thing which a plugin must return is a site object which contains
; {
;   url - the fixed part of the url which will be combined with the search criteria
;   xpath - a list of xpaths queries to use on the html returned from url
;   parse-func - the function which will parse the nodes resulting from applying xpath to the html from url
;   url-func - the function that gets the base url + criteria and will build the full url
; }

; This plugin will search project gutenberg
(defpackage :furseby.plugins.gutenberg
  (:use :cl :xpath :url-rewrite
        :furseby.core))

(in-package :furseby.plugins.gutenberg)

; Each plugin will define a function which returns the search url
(defun get-gutenberg-url (base url)
  (concatenate 'string base (url-encode url)))

; helpful functions to extract relevant parts from the node
(defun get-authors (col)
    (apply #'concatenate 'string (mapcar #'xtree:text-content (find-list col "a"))))
(defun get-title (col)
    (xtree:text-content (first (find-list col "a"))))
(defun get-link (col)
    (xtree:attribute-value (first (find-list col "a")) "href"))

; This is a helpful function for the nodes parsing, it can parse one row
(defun parse-gutenberg-row (row)
  (list (get-authors (third row)) (get-title (fourth row)) (get-link (fourth row))))

; A function which will parse the nodes it receives from the search result and combine the results
(defun parse-gutenberg-nodes (nodes)
  (mapcar (lambda (x) (parse-gutenberg-row (find-list x ".td"))) nodes))

; And finally, we create a new site, defining a list of xpatsh
(pushnew (make-site :url "http://www.gutenberg.org/catalog/world/results?title="
                    :xpath '("//table[@class='pgdbfiles']/tr[@class='evenrow']"
                             "//table[@class='pgdbfiles']/tr[@class='oddrow']") 
                    :parse-func #'parse-gutenberg-nodes
                    :url-func #'get-gutenberg-url) *sites*)

