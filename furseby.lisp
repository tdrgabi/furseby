(ql:quickload "cl-gtk2-gtk")
(ql:quickload "drakma")
(ql:quickload "cl-libxml2")

(ql:system-apropos "libxml2")

(defpackage :fur-seby
  (:use :cl :gtk :gdk :gobject :iter :drakma :html :puri :xpath))
(in-package :fur-seby)
(defparameter *search-criteria* "http://www.gutenberg.org/author/dante%20aligheri")

(escape-string *search-criteria*)

(http-request *search-criteria*)

(parse-html *search-criteria*)

(setq *document* (parse-html (puri:parse-uri *search-criteria*)))

(find-list *document* "//div")

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

(run)

(in-package :restas.core)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass restas-plugin-system (asdf:system)
  ((require-plugins :initarg :depends-on-plugins :initform nil)
   (http-map :initarg :http-map :initform nil)
   (chrome-map :initarg :chrome-map :initform nil)
   (preferences :initarg :preferences :initform nil)
   (baseurl :initarg :baseurl :initform nil)
   (database :initarg :db :initform nil)
   (host :initarg :host :initform nil)
   (environment :initarg :environment :initform nil)
   ))

(defclass restas-plugin ()
  ((restas-plugin-system :initarg :system :reader restas-plugin-system)
   (appserver :initarg :appserver :reader restas-plugin-appserver)
   (gb-pool :initform nil :accessor plugin-pool)
   (plugin-stage-map :initform nil :accessor plugin-stage-map)
   (plugin-xpath-functions :initform nil :accessor plugin-xpath-functions)
   (plugin-xslt-elements :initform nil :accessor plugin-xslt-elements)
   (plugin-template-handlers :initform nil :accessor plugin-template-handlers)))


(defmethod initialize-instance :after ((plugin restas-plugin) &key)
  (setf (slot-value plugin 'gb-pool) (make-instance 'gp:pool)))

(defun plugin-baseurl (plugin)
  (let ((str (string-trim "/" (restas.preferences:getpref (slot-value (restas-plugin-system plugin)
                                                                      'baseurl)))))
    (if (string= str "")
        ""
        (format nil "~A/" str))))

(defun plugin-url-map (plugin protocol)
  (let* ((slot (case protocol
                (:http 'http-map)
                (:chrome 'chrome-map)))
         (path (slot-value (restas-plugin-system plugin) slot)))
    (if path
        (merge-pathnames path
                         (asdf:component-pathname (restas-plugin-system plugin))))))

(defun plugin-binginds (plugin)
  (slot-value (restas-plugin-system plugin)
              'environment))

(defun plugin-preferences (plugin)
  (slot-value (restas-plugin-system plugin)
              'preferences))

(defun plugin-host (plugin)
  (slot-value (restas-plugin-system plugin)
              'host))

(defun plugin-database (plugin)
  (slot-value (restas-plugin-system plugin)
              'database))

(defmacro define-restas-plugin (name &body options)
  (let ((plugin-name (intern (format nil "~A-RESTAS-PLUGIN" name) :keyword)))
    `(asdf:defsystem ,plugin-name
       :class restas-plugin-system
       ,@options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugin-stage-map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-plugin-stage-handler (stage-type handler)
  (setf (plugin-stage-map *plugin*)
        (acons stage-type
               handler
               (slot-value *plugin* 'plugin-stage-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugin-template-handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-plugin-template-handler (type handler)
  (setf (plugin-template-handlers *plugin*)
        (acons type
               handler
               (slot-value *plugin* 'plugin-template-handlers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register-plugin-xpath-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-plugin-xpath-function (fun name namespace)
  (push (list fun name namespace)
        (plugin-xpath-functions *plugin*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-plugins (appserver)
  (let ((plugins-dir (or (restas.preferences:getpref :restas.plugins.dir)
                         (merge-pathnames "plugins/" (asdf:component-pathname (asdf:find-system :restas))))))
    (iter (for dir in (fad:list-directory plugins-dir))
          (let ((plugin-path (merge-pathnames "restas.manifest"
                                              dir)))
            (when (fad:file-exists-p plugin-path)
              (let ((*package* (find-package :restas.core)))
                (load plugin-path))
              (let ((plugin (asdf:find-system (format nil
                                                      "~a-restas-plugin"
                                                      (car (last (pathname-directory plugin-path)))))))
                (if plugin (collect (make-instance 'restas-plugin
                                                   :system plugin
                                                   :appserver appserver)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-plugin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-url (url baseurl)
  (cl-ppcre:regex-replace-all "/+"
                              (format nil "~A/~A" baseurl url)
                              "/"))

(defun split-template (tmpl)
  (iter (for path in (routes::split-template (ppcre:regex-replace-all "//+"
                                                                      (string-left-trim "/" tmpl)
                                                                      "/")))
        (collect (let ((spec (routes::parse-path path)))
                   (if (cdr spec)
                       (unify:make-unify-template 'unify::concat spec)
                       (car spec))))))

(defun process-route-node (node baseurl plugin rserver protocol)
  (let ((host (restas.preferences:getpref (plugin-host plugin))))
    (iter (for child in-child-nodes node with (:type :xml-element-node :ns *restas-ns*))
          (let ((bindings (list (cons :method (string->keyword (xtree:local-name child)))))
                (tmpl (format-url (xtree:attribute-value node "url")
                                  (or baseurl (plugin-baseurl plugin)))))
            (connect-route rserver
                           (make-instance 'pipeline-route
                                          :template tmpl
                                          :routenode child
                                          :routeplugin plugin
                                          :extra-bindings (if host
                                                              (acons :host host bindings)
                                                              (list (cons :method (string->keyword (xtree:local-name child))))))
                           protocol)))))

(defun process-location-node (node baseurl plugin rserver protocol)
  (let ((url (format-url (xtree:attribute-value node "url")
                         (or baseurl (plugin-baseurl plugin)))))
    (iter (for child in-child-nodes node with (:type :xml-element-node :ns *restas-ns* :local-name "location"))
          (process-location-node child
                                 url
                                 plugin
                                 rserver
                                 protocol))
    (iter (for child in-child-nodes node with (:type :xml-element-node :ns *restas-ns* :local-name "route"))
          (process-route-node child
                              url
                              plugin
                              rserver
                              protocol))))


(defgeneric plugin-init (plugin name))

(defmethod plugin-init (plugin name))

(defun load-plugin (plugin rserver)
  (let ((*plugin* plugin)
        (*application-server* rserver))
    (asdf:operate 'asdf:load-op (restas-plugin-system plugin)))
  (let ((prefpath (plugin-preferences plugin)))
    (when prefpath
      (let ((*plugin* plugin)
            (*application-server* rserver)
            (*package* (find-package :restas.preferences)))
        (load (merge-pathnames prefpath (asdf:component-pathname (restas-plugin-system plugin)))))))
  (flet ((init-url-map (plugin protocol)
           (let* ((map (plugin-url-map plugin protocol))
                  (doc (if map (gp:object-register (xtree:parse (merge-pathnames map
                                                                                 (asdf:component-pathname (restas-plugin-system plugin))))
                                                   (plugin-pool plugin)))))
             (when doc
               (xtree:process-xinclude doc)
               (restas.core::process-location-node (xtree:root doc)
                                                   nil
                                                   plugin
                                                   rserver
                                                   protocol)))))
    (init-url-map plugin :http)
    (init-url-map plugin :chrome)))
