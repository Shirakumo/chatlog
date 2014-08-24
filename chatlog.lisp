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

(defun format-time (unix)
  (local-time:format-timestring
   NIL (local-time:unix-to-timestamp unix)
   :format '((:hour 2) #\: (:min 2) #\: (:sec 2))))

(defun compute-where (types user)
  (values
   (format NIL "~@[ AND (~{\"type\"=$~a~^ OR ~})~]~@[~* AND \"nick\"=$6~]"
           (loop for i from (if user 6 7)
                 for type across (or types "")
                 collect i) user)
   (if user
       (cons user types)
       types)))

(defun fetch (server channel types from to user amount order)
  (unless (or (string= order "ASC") (string= order "DESC"))
    (error 'api-argument-invalid :argument "order" :message "Order must be either DESC or ASC."))
  (let ((from (or (parse-integer (or from "") :junk-allowed T) (- (get-unix-time) (* 60 60 12))))
        (to (or (parse-integer (or to "") :junk-allowed T) (get-unix-time)))
        (amount (or (parse-integer (or amount "") :junk-allowed T) 500))
        (channel (format NIL "#~a" channel)))
    (multiple-value-bind (where args) (compute-where types user)
      (let ((database (or (uc:config-tree :chatlog :database) "irc"))
            (user (or (uc:config-tree :chatlog :user) "irc"))
            (pass (or (uc:config-tree :chatlog :pass) (error 'radiance-error :message "Configuration error.")))
            (host (or (uc:config-tree :chatlog :host) "localhost"))
            (table (or (uc:config-tree :chatlog :table) "chatlog")))
        (l:info :test "~s ~s" (format NIL "SELECT * FROM \"~a\" WHERE (\"server\"=$1 AND \"channel\"=$2 AND \"time\">=$3 AND \"time\"<=$4 ~a) ORDER BY \"time\" ~a LIMIT $5" table where order)
                (append (list server channel from to amount) args))
        (postmodern:with-connection (list database user pass host)
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

(define-api chatlog/get (server channel &optional types from to user (amount "500") (order "DESC")) ()
  (api-output (fetch server channel types from to user amount order)))

(define-page view #@"log.irc/^$" (:lquery (template "index.html"))
  (r-clip:process (lquery:$ (node)) :messages (fetch "TYNET" "#Stevenchan" "m" NIL NIL NIL NIL "ASC")))
