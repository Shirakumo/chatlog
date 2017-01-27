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
(defvar *timestep* (* 60 60 12))

(defmacro with-connection (() &body body)
  `(postmodern:with-connection (list (defaulted-config "irc" :database)
                                     (defaulted-config "irc" :user)
                                     (or (config :pass) (error 'internal-error :message "Configuration error."))
                                     (defaulted-config "localhost" :host))
     ,@body))

(defun get-unix-time ()
  (local-time:timestamp-to-unix
   (local-time:now)))

(defun format-text (text)
  ;; Strip XML invalid characters... *sigh*.
  (cl-ppcre:regex-replace-all "[^	\\x0A -퟿-�𐀀-􏿿]+" text ""))

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
  (let ((from (maybe-parse-timestamp from))
        (to (maybe-parse-timestamp to))
        (around (maybe-parse-timestamp around)))
    (cond (around
           (setf from (- around (round *timestep* 2)))
           (setf to (+ around (round *timestep* 2))))
          ((and from (not to))
           (setf to (+ from *timestep*)))
          ((and to (not from))
           (setf from (- to *timestep*)))
          ((and (not from) (not to))
           (setf from (- (get-unix-time) (round *timestep* 2)))
           (setf to (+ (get-unix-time) (round *timestep* 2)))))
    (values from to)))

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
    ORDER BY \"server\" DESC,\"channel\" DESC"))

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
  (let ((channels ()))
    (loop for (server channel) in (with-connection ()
                                    (postmodern:query (funcall *select-channels*)))
          do (let ((cons (assoc server channels :test #'string=)))
               (if cons
                   (setf (cdr cons) (cons channel (cdr cons)))
                   (push (list server channel) channels))))
    channels))

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

(define-page view "irclog/^([a-zA-Z]+)/(#*[a-zA-Z_\\-]+)$" (:uri-groups (server channel) :clip "view.ctml")
  (setf (content-type *response*) "application/xhtml+xml")
  (let* ((type (or (get-var "type[]") NIL))
         (types (or* (get-var "types") (format NIL "~{~a~}" type) *default-types*)))
    (multiple-value-bind (from to) (parse-time-region (get-var "from") (get-var "to") (get-var "around"))
      (r-clip:process
       T :messages (fetch server channel types from to 1000)
         :from from
         :to to
         :timestep *timestep*
         :types types
         :page (format NIL "~a/~a" server channel)))))

(define-page index "irclog/^$" (:clip "index.ctml")
  (setf (content-type *response*) "application/xhtml+xml")
  (r-clip:process
   T :channels (channels)))

(define-route log.irc->irclog :mapping (uri)
  (when (equalp (domains uri) '("irc" "log"))
    (setf (domains uri) (list "irclog"))))
