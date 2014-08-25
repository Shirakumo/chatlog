#|
 This file is a part of chatlog
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:chatlog
  (:nicknames #:org.tymoonnext.chatlog)
  (:use #:cl #:radiance)
  (:export))
(in-package #:org.tymoonnext.chatlog)

(defmacro with-connection (() &body body)
  `(postmodern:with-connection (list (or (uc:config-tree :chatlog :database) "irc")
                                     (or (uc:config-tree :chatlog :user) "irc")
                                     (or (uc:config-tree :chatlog :pass) (error 'radiance-error :message "Configuration error."))
                                     (or (uc:config-tree :chatlog :host) "localhost"))
     ,@body))

(defun format-time (unix)
  (if unix
      (local-time:format-timestring
       NIL (local-time:unix-to-timestamp unix)
       :format '((:hour 2) #\: (:min 2) #\: (:sec 2)))
      ""))

(defun format-text (text)
  (string-trim '(#\Newline #\Return #\Space #\Linefeed #\Tab #\Soh) text))

(defun compute-where (types user)
  (let ((types (loop for char across (or types "") collect (string char))))
    (values
     (format NIL "~@[ AND (~{\"type\"=$~a~^ OR ~})~]~@[~* AND \"nick\"=$6~]"
             (loop for i from (if user 6 7)
                   for type in types
                   collect i) user)
     (if user
         (cons user types)
         types))))

:SERVER :CHANNEL :NICK :TIME :TYPE :MESSAGE
(defun fetch (server channel types from to user amount order)
  (unless (or (string= order "ASC") (string= order "DESC"))
    (error 'api-argument-invalid :argument "order" :message "Order must be either DESC or ASC."))
  (let ((from (or (parse-integer (or from "") :junk-allowed T) (- (get-unix-time) (* 60 60 12))))
        (to (or (parse-integer (or to "") :junk-allowed T) (get-unix-time)))
        (amount (or (parse-integer (or amount "") :junk-allowed T) 500))
        (channel (format NIL "#~a" channel)))
    (when (< 10000 amount)
      (error 'api-argument-invalid :argument "amount" :message "Amount cannot be higher than 10000."))
    (multiple-value-bind (where args) (compute-where types user)
      (let ((table (or (uc:config-tree :chatlog :table) "chatlog")))
        (with-connection ()
          (cl-postgres:prepare-query
           postmodern:*database* ""
           (format NIL "SELECT * FROM \"~a\" WHERE (\"server\"=$1 AND \"channel\"=$2 AND \"time\">=$3 AND \"time\"<=$4 ~a) ORDER BY \"time\" ~a LIMIT $5" table where order))
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
      (postmodern:query (format NIL "SELECT \"channel\",\"server\" FROM \"~a\" GROUP BY \"channel\",\"server\"" table)))))

(define-api chatlog/get (server channel &optional types from to user (amount "500") (order "DESC")) ()
  (api-output (fetch server channel types from to user amount order)))

(define-api chatlog/channels () ()
  (api-output (channels)))

(define-page view #@"log.irc/^(([a-zA-Z]+)/([a-zA-Z]+)(/([^.]*))?)?$" (:uri-groups (NIL server channel NIL user) :lquery (template "index.html"))
  (r-clip:process
   (lquery:$ (node))
   :messages (fetch (or server (uc:config-tree :chatlog :default-server) (error 'radiance-error :message "Configuration error."))
                    (or channel (uc:config-tree :chatlog :default-channel) (error 'radiance-error :message "Configuration error."))
                    (get-var "types") (get-var "from") (get-var "to") user "10000" "ASC")))
