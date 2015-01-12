#|
 This file is a part of chatlog
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:chatlog
  (:nicknames #:org.tymoonnext.chatlog)
  (:use #:cl #:radiance)
  (:export))
(in-package #:org.tymoonnext.chatlog)

(defvar *default-types* "mnaot")

(defmacro with-connection (() &body body)
  `(postmodern:with-connection (list (or (uc:config-tree :chatlog :database) "irc")
                                     (or (uc:config-tree :chatlog :user) "irc")
                                     (or (uc:config-tree :chatlog :pass) (error 'radiance-error :message "Configuration error."))
                                     (or (uc:config-tree :chatlog :host) "localhost"))
     ,@body))

(defun get-unix-time ()
  (local-time:timestamp-to-unix
   (local-time:now)))

(defun format-time (unix)
  (if unix
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp unix)
       :format '((:hour 2) #\: (:min 2) #\: (:sec 2))
       :timezone local-time:+utc-zone+)
      ""))

(defun format-long-time (unix)
  (if unix
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp unix)
       :format '((:year 4) #\- (:month 2) #\- (:day 2) #\T (:hour 2) #\: (:min 2) #\: (:sec 2))
       :timezone local-time:+utc-zone+)
      ""))

(defun format-text (text)
  (string-trim '(#\Newline #\Return #\Space #\Linefeed #\Tab #\Soh) text))

(defun time-link (unix &optional (types *default-types*))
  (format NIL "/~a?around=~a~@[&types=~a~]#~a" (path (uri *request*)) (format-long-time unix) types unix))

(defun title-time (unix)
  (if unix
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp unix)
       :format '("Link to " :long-weekday ", " :ordinal-day " of " :long-month " " (:year 4) " " (:hour 2) #\: (:min 2) #\: (:sec 2))
       :timezone local-time:+utc-zone+)
      ""))

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

:SERVER :CHANNEL :NICK :TIME :TYPE :MESSAGE
(defun fetch (server channel types from to amount order)
  (unless (or (string= order "ASC") (string= order "DESC"))
    (error 'api-argument-invalid :argument "order" :message "Order must be either DESC or ASC."))
  (let ((from (or (parse-i from) (- (get-unix-time) (* 60 60 12))))
        (to (or (parse-i to) (get-unix-time)))
        (amount (or (parse-i amount) 500))
        (channel (format NIL "#~a" channel)))
    (when (< 10000 amount)
      (error 'api-argument-invalid :argument "amount" :message "Amount cannot be higher than 10000."))
    (multiple-value-bind (where args) (compute-where types)
      (let ((table (or (uc:config-tree :chatlog :table) "chatlog")))
        (with-connection ()
          (cl-postgres:prepare-query
           postmodern:*database* ""
           (format NIL "SELECT * FROM \"~a\" WHERE (\"server\"=$1 AND \"channel\"=$2 AND \"time\">=$3 AND \"time\"<=$4 ~a) ORDER BY \"time\" ~a LIMIT $5"
                   table where order))
          (cl-postgres:exec-prepared
           postmodern:*database* "" (append (list server channel from to amount) args)
           (cl-postgres:row-reader (fields)
             (loop while (cl-postgres:next-row)
                   collect (let ((table (make-hash-table :test 'equalp)))
                             (loop for field across fields
                                   do (setf (gethash (cl-postgres:field-name field) table)
                                            (cl-postgres:next-field field))) table)))))))))

(defun channels ()
  (let ((table (or (uc:config-tree :chatlog :table) "chatlog")))
    (with-connection ()
      (postmodern:query (format NIL "SELECT \"server\",\"channel\" FROM \"~a\" GROUP BY \"channel\",\"server\"" table)))))

(define-api chatlog/get (server channel &optional types from to (amount "500") (order "DESC")) ()
  (api-output (fetch server channel types from to amount order)))

(define-api chatlog/channels () ()
  (api-output (channels)))

(define-page view #@"log.irc/^([a-zA-Z]+)/#?([a-zA-Z]+)(/([^.]*))?$" (:uri-groups (server channel NIL page) :lquery (template "view.ctml"))
  (setf (content-type *response*) "text/html")
  (cond
    ((string-equal (or* page "log") "log")
     (let* ((type (or (get-var "type[]") NIL))
            (types (or* (get-var "types") (format NIL "~{~a~}" type) *default-types*))
            (from (or* (get-var "from") (- (get-unix-time) (* 60 60 12))))
            (to (or* (get-var "to") (+ (get-unix-time) 60)))
            (around (or* (get-var "around") NIL)))
       (when around
         (ignore-errors
          (setf around (maybe-parse-timestamp around))
          (when around
            (setf from (- around (* 60 60 6)))
            (setf to (+ around (* 60 60 6))))))
       (setf from (maybe-parse-timestamp from))
       (setf to (maybe-parse-timestamp to))
       (r-clip:process
        (lquery:$ (node))
        :messages (fetch (or server (uc:config-tree :chatlog :default-server) (error 'radiance-error :message "Configuration error."))
                         (or channel (uc:config-tree :chatlog :default-channel) (error 'radiance-error :message "Configuration error."))
                         types from to 10000 "ASC")
        :from from :to to :types types)))
    ((string-equal page "stats")
     )))

(define-page index #@"log.irc/^$" (:lquery (template "index.ctml"))
  (setf (content-type *response*) "text/html")
  (r-clip:process
   (lquery:$ (node))
   :channels (sort (mapcar #'(lambda (entry) (format NIL "/~a/~a" (first entry) (subseq (second entry) 1))) (channels))
                   #'string<)))
