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
  ;; Strip XML invalid characters... *sigh*.
  (cl-ppcre:regex-replace-all "[^	\\x0A -퟿-�𐀀-􏿿]+" text ""))

(defun time-link (unix &optional (types *default-types*) (keyword :around))
  (format NIL "/~a?~(~a~)=~a~@[&types=~a~]#~a" (path (uri *request*)) keyword (format-long-time unix) types unix))

(defun title-time (unix)
  (if unix
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp unix)
       :format '("Link to " :long-weekday ", " :ordinal-day " of " :long-month " " (:year 4) " " (:hour 2) #\: (:min 2) #\: (:sec 2))
       :timezone local-time:+utc-zone+)
      ""))

(lquery:define-lquery-function chatlog-template (node object)
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (template (format NIL "~(~a~).ctml" object)) :root node)
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
    FROM \"~a\" ~
    WHERE (\"server\"=$1 ~
       AND \"channel\"=$2 ~
       AND \"time\">=$3 ~
       AND \"time\"<=$4 ~a) ~
    ORDER BY \"time\" ASC, \"id\" ASC ~
    LIMIT $5"))

(defparameter *select-channels*
  (string-formatter
   "SELECT \"server\",\"channel\" ~
    FROM \"~a\" ~
    GROUP BY \"channel\",\"server\""))

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
  (let ((from (or (parse-i from) (- (get-unix-time) (* 60 60 12))))
        (to (or (parse-i to) (get-unix-time)))
        (amount (or (parse-i amount) 500))
        (channel (format NIL "#~a" channel)))
    (when (< 10000 amount)
      (error 'api-argument-invalid :argument "amount" :message "Amount cannot be higher than 10000."))
    (multiple-value-bind (where args) (compute-where types)
      (let ((table (or (uc:config-tree :chatlog :table) "chatlog")))
        (with-connection ()
          (apply #'query (funcall *select-messages* table where) server channel from to amount args))))))

(defun channels ()
  (with-connection ()
    (postmodern:query (funcall *select-channels* (or (uc:config-tree :chatlog :table) "chatlog")))))

(define-api chatlog/get (server channel &optional types from to (amount "500") (order "DESC")) ()
  (api-output (fetch server channel types from to amount order)))

(define-api chatlog/channels () ()
  (api-output (channels)))

(define-page view #@"log.irc/^([a-zA-Z]+)/#?([a-zA-Z]+)(/([^.]*))?$" (:uri-groups (server channel NIL page) :lquery (template "view.ctml"))
  (setf (content-type *response*) "application/xhtml+xml")
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
       (r-clip:process T :messages (fetch server channel types from to 10000) :from from :to to :types types :page (format NIL "~a/#~a" server channel))))
    ((string-equal page "stats")
     )))

(define-page index #@"log.irc/^$" (:lquery (template "index.ctml"))
  (setf (content-type *response*) "application/xhtml+xml")
  (r-clip:process
   (lquery:$ (node))
   :channels (sort (mapcar #'(lambda (entry) (format NIL "/~a/~a" (first entry) (subseq (second entry) 1))) (channels))
                   #'string<)))
