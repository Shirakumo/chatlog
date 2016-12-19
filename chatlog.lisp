#|
 This file is a part of chatlog
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:chatlog
  (:nicknames #:org.tymoonnext.chatlog)
  (:use #:cl #:radiance)
  (:domain "irclog")
  (:export))
(in-package #:org.tymoonnext.chatlog)

(defvar *default-types* "mnaot")

(defmacro with-connection (() &body body)
  `(postmodern:with-connection (list (defaulted-config "irc" :chatlog :database)
                                     (defaulted-config "irc" :chatlog :user)
                                     (or (config :chatlog :pass) (error 'radiance-error :message "Configuration error."))
                                     (defaulted-config "localhost" :chatlog :host))
     ,@body))

(defun get-unix-time ()
  (local-time:timestamp-to-unix
   (local-time:now)))

(defun format-text (text)
  ;; Strip XML invalid characters... *sigh*.
  (cl-ppcre:regex-replace-all "[^	\\x0A -퟿-�𐀀-􏿿]+" text ""))

(defun format-machine-unixtime (unix)
  (format-machine-date (unix-to-universal unix)))

(defun time-link (unix &optional (types *default-types*) (keyword :around))
  (format NIL "/~a?~(~a~)=~a~@[&types=~a~]#~a"
          (escape-url (path (uri *request*))) keyword (format-machine-unixtime unix) types unix))

(defun title-time (unix)
  (if unix
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp unix)
       :format '("Link to " :long-weekday ", " :ordinal-day " of " :long-month " " (:year 4) " " (:hour 2) #\: (:min 2) #\: (:sec 2))
       :timezone local-time:+utc-zone+)
      ""))

(defun escape-url (url)
  (cl-ppcre:regex-replace-all "#" url "%23"))

(lquery:define-lquery-function chatlog-template (node object)
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (@template (format NIL "~(~a~).ctml" object)) :root node)
  node)

(defun compute-where (types)
  (let ((types (loop for char across (or types "") collect (string char))))
    (values
     (format NIL "~@[ AND (~{\"type\"=$~a~^ OR ~})~]"
             (loop for i from 6
                   for type in types
                   collect i))
     types)))

(defun maybe-parse-timestamp (thing)
  (typecase thing
    (string
     (or (ignore-errors (parse-integer thing))
         (ignore-errors
          (local-time:timestamp-to-unix
           (local-time:parse-timestring thing :allow-missing-elements T)))))
    (number thing)
    (null thing)))

(defun parse-i (thing)
  (typecase thing
    (integer thing)
    (string (parse-integer thing :junk-allowed T))
    (null NIL)))

(defun parse-time-region (from to around)
  (let ((from (or* from (- (get-unix-time) (* 60 60 12))))
        (to (or* to (+ (get-unix-time) 60)))
        (around (or* around NIL)))
    (when around
      (ignore-errors
       (setf around (maybe-parse-timestamp around))
       (when around
         (setf from (- around (* 60 60 6)))
         (setf to (+ around (* 60 60 6))))))
    (values (maybe-parse-timestamp from)
            (maybe-parse-timestamp to))))

;; Make sure these keywords are known.
:SERVER :CHANNEL :NICK :TIME :TYPE :MESSAGE

(defmacro string-formatter (format-string)
  (let ((args (gensym "ARGS"))
        (stream (gensym "STREAM")))
    `(lambda (&rest ,args)
       (with-output-to-string (,stream)
         (apply (formatter ,format-string) ,stream ,args)))))

(defparameter *select-messages*
  (string-formatter
   "SELECT * ~
    FROM \"chatlog\" ~
    WHERE (\"channel-id\"=(SELECT \"id\" FROM \"channels\" WHERE 
                               \"channels\".\"server\"=$1
                           AND (\"channels\".\"channel\"=$2
                                OR trim(leading '#' from \"channels\".\"channel\")=$2))
       AND \"time\">=$3 ~
       AND \"time\"<=$4 ~a) ~
    ORDER BY \"time\" ASC, \"id\" ASC ~
    LIMIT $5"))

(defparameter *select-channels*
  (string-formatter
   "SELECT \"server\",\"channel\" ~
    FROM \"channels\" ~
    ORDER BY \"server\",\"channel\""))

(defun query (query &rest parameters)
  (cl-postgres:prepare-query postmodern:*database* "" query)
  (cl-postgres:exec-prepared
   postmodern:*database* "" parameters
   (cl-postgres:row-reader (fields)
     (loop while (cl-postgres:next-row)
           collect (let ((table (make-hash-table :test 'equalp)))
                     (loop for field across fields
                           do (setf (gethash (cl-postgres:field-name field) table)
                                    (cl-postgres:next-field field))) table)))))

(defun fetch (server channel types from to amount)
  (let ((amount (or (parse-i amount) 500)))
    (when (< 1000 amount)
      (error 'api-argument-invalid :argument "amount" :message "Amount cannot be higher than 10000."))
    (multiple-value-bind (where args) (compute-where types)
      (with-connection ()
        (apply #'query (funcall *select-messages* where) server channel from to amount args)))))

(defun channels ()
  (with-connection ()
    (postmodern:query (funcall *select-channels*))))

(define-api chatlog/get (server channel &optional types from to around (amount "1000") (format "objects")) ()
  (multiple-value-bind (from to) (parse-time-region from to around)
    (let ((events (fetch server channel types from to amount)))
      (cond ((string-equal format "objects")
             (api-output events))
            ((string-equal format "rendered")
             (setf (content-type *response*) "text/plain")
             (with-output-to-string (stream)
               (loop initially (format stream "~&--- ~a/~a ~a" server channel (format-machine-unixtime (gethash "time" (first events))))
                     for event in events
                     do (format stream "~&~a <~a> ~a"
                                (format-machine-unixtime (gethash "time" event)) (gethash "nick" event) (gethash "message" event))
                     finally (format stream "~&--- ~a/~a ~a" server channel (format-machine-unixtime (gethash "time" event))))))))))

(define-api chatlog/channels () ()
  (api-output (channels)))

(define-page view "irclog/^([a-zA-Z]+)/(#*[a-zA-Z_\\-]+)(/([^.]*))?$" (:uri-groups (server channel NIL page) :clip (@template "view.ctml"))
  (setf (content-type *response*) "application/xhtml+xml")
  (cond
    ((string-equal (or* page "log") "log")
     (let* ((type (or (get-var "type[]") NIL))
            (types (or* (get-var "types") (format NIL "~{~a~}" type) *default-types*)))
       (multiple-value-bind (from to) (parse-time-region (get-var "from") (get-var "to") (get-var "around"))
         (r-clip:process T :messages (fetch server channel types from to 1000) :from from :to to :types types :page (format NIL "~a/~a" server channel)))))
    ((string-equal page "stats")
     )))

(define-page index "irclog/^$" (:clip (@template "index.ctml"))
  (setf (content-type *response*) "application/xhtml+xml")
  (r-clip:process
   (lquery:$ (node))
   :channels (mapcar (lambda (entry) (format NIL "/~a/~a" (first entry) (second entry)))
                     (channels))))

(define-route log.irc->irclog :mapping (uri)
  (when (equalp (domains uri) '("irc" "log"))
    (setf (domains uri) (list "irclog"))))
