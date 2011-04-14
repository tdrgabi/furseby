; == Overview ==

; This package will contain all the core functions, to lookup plugins, apply the search query on all of them, gather results and return them to the GUI (furseby) package

(in-package :furseby.core)
(defparameter *offline-dev* 1)

; Each site, defined in a plugin, will know it's search url, which xpath to apply to the result, so that only the nodes that match are passed to plugin and which functions to call to parse the nodes
(defstruct site url xpath parse-func url-func)

; Each found result will be of type book
(defstruct book author title url)

; The global parameter which holds a list of *sites*. The search result will be applied to all of the current known sites
(defparameter *sites* '())

; apply the criteria to a site function
(defun search-site (site criteria)
  (let* (
        ; the full-url will be built, by the site-url-fun defined in plugin. 
        ; We only provide the criteria and the site will return an url
        (full-url (funcall (site-url-func site) (site-url site) criteria))
        ; doc is the html, in xpath object form, that we get once we try to retrieve the url
        ; *TODO* some error handling will be needed here
        (doc (parse-html (puri:parse-uri full-url)))
        ; nodes are the results that we get once we applied the xpaths from plugin
        (nodes (apply #'append (mapcar (lambda (x) (apply-xpath-to-document full-url doc x)) (site-xpath site)))))
  ; and finally we get to parse those nodes and return the result
  (funcall (site-parse-func site) nodes) ))

(defun apply-xpath-to-document (url doc xpath)
  (if (equal nil doc)
      (progn 
        (format t "~a~a~a~a~%" "The document from " url " is nil, will not apply xpath nodes:" xpath)
        nil)
      (find-list doc xpath)))


; this will only go through all the sites, apply the search-site function and collect the results
(defun search-all-sites (criteria)
  (mapcar (lambda (site) (search-site site criteria)) *sites*))
