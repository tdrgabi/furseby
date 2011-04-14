; == Overview ==

; The standard thing which a plugin must return is a site object which contains
; {
;   url - the fixed part of the url which will be combined with the search criteria
;   xpath - a list of xpaths queries to use on the html returned from url
;   parse-func - the function which will parse the nodes resulting from applying xpath to the html from url
;   url-func - the function that gets the base url + criteria and will build the full url
; }

; This plugin will search project gutenberg
(in-package :furseby.plugins.gutenberg)

; Each plugin will define a function which returns the search url
(defun get-gutenberg-url (base url)
  (if (equal furseby.core:*offline-dev* 1)
      base
      (concatenate 'string base (url-encode url))))

; helpful functions to extract relevant parts from the node
(defun get-authors (col)
    (apply #'concatenate 'string (mapcar #'xtree:text-content (find-list col "a"))))
(defun get-title (col)
    (xtree:text-content (first (find-list col "a"))))
(defun get-link (col)
    (xtree:attribute-value (first (find-list col "a")) "href"))

; This is a helpful function for the nodes parsing, it can parse one row
(defun parse-gutenberg-row (row)
  (make-book :author (get-authors (third row)) 
             :title (get-title (fourth row)) 
             :url (get-link (fourth row))))

; A function which will parse the nodes it receives from the search result and combine the results
(defun parse-gutenberg-nodes (nodes)
  ; we take all the columns for each row in find-list, like "author" "link" "text"
  (mapcar (lambda (x) (parse-gutenberg-row (find-list x "td"))) nodes))

(defun get-url ()
  (if (equal furseby.core:*offline-dev* 1)
      "samples/gutenberg.html"
      "http://www.gutenberg.org/catalog/world/results?title="))

; And finally, we create a new site, defining a list of xpatsh
(pushnew (make-site :url (get-url)
                    ;we extract a table with class pgdbfiles, and from him take all tr with class evenrow and oddrow
                    :xpath '("//table[@class='pgdbfiles']/tbody/tr[@class='evenrow']"
                             "//table[@class='pgdbfiles']/tbody/tr[@class='oddrow']") 
                    :parse-func #'parse-gutenberg-nodes
                    :url-func #'get-gutenberg-url) *sites*)

