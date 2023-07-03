(in-package #:modularize-user)
(define-module #:chatlog
  (:nicknames #:org.tymoonnext.chatlog)
  (:use #:cl #:radiance)
  (:domain "irclog")
  (:export))
(in-package #:org.tymoonnext.chatlog)

(defvar *default-types* "mnaot")
(defvar *timestep* (* 60 60 12))
(defvar *connections*)
(defvar *connections-lock* (bt:make-lock "connections"))
(defvar *connections-condition* (bt:make-condition-variable :name "connections"))

(defun open-connections (&key (connections (defaulted-config 5 :connections))
                              (db (defaulted-config "irc" :database))
                              (user (defaulted-config "irc" :user))
                              (pass (config :pass))
                              (host (defaulted-config "localhost" :host)))
  (bt:with-lock-held (*connections-lock*)
    (with-simple-restart (ignore "Leave things as they are.")
      (when (boundp '*connections*)
        (error "Connections already opened."))
      (setf *connections* NIL)
      (handler-bind ((error (lambda (err)
                              (declare (ignore err))
                              (makunbound '*connections*))))
        (loop repeat connections
              do (push (or (postmodern:connect db user pass host)
                           (error "Failed to connect to chatlog database."))
                       *connections*))))))

(defun close-connections ()
  (bt:with-lock-held (*connections-lock*)
    (unless (boundp '*connections*)
      (error "Connections already closed."))
    (loop for connection = (pop *connections*)
          while connection
          do (ignore-errors (postmodern:disconnect connection)))
    (makunbound '*connections*)))

(defun acquire-connection ()
  (loop for i from 0 to 100
        until (boundp '*connections*)
        do (sleep 0.1))
  (bt:with-lock-held (*connections-lock*)
    (loop for connection = (pop *connections*)
          until connection
          do (bt:condition-wait *connections-condition* *connections-lock*
                                :timeout 5)
          finally (unless (postmodern:connected-p connection)
                    (postmodern:reconnect connection))
                  (return connection))))

(defun release-connection (connection)
  (bt:with-lock-held (*connections-lock*)
    (push connection *connections*))
  (bt:condition-notify *connections-condition*))

(defun call-with-connection (function)
  (if postmodern:*database*
      (funcall function)
      (let ((postmodern:*database* (acquire-connection)))
        (unwind-protect (funcall function)
          (release-connection postmodern:*database*)))))

(defmacro with-connection (() &body body)
  `(call-with-connection (lambda () ,@body)))

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

(defun compute-where (types &optional (start-var 6))
  (let ((types (loop for char across (or types "") collect (string char))))
    (values
     (format NIL "~@[ AND (~{\"type\"=$~a~^ OR ~})~]"
             (loop for i from start-var
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

(defun parse-search (thing)
  (typecase thing
    (string
     (let ((thing (string-trim " " thing)))
       (when (string/= thing "")
         (format NIL "%~a%" (cl-ppcre:regex-replace-all " +" thing "%")))))
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
    WHERE (\"channel-id\"=(SELECT \"id\" FROM \"channels\" WHERE ~
                               \"channels\".\"server\"=$1 ~
                           AND (\"channels\".\"channel\"=$2 ~
                                OR trim(leading '#' from \"channels\".\"channel\")=$2)) ~
       AND \"time\">=$3 ~
       AND \"time\"<=$4 ~
       ~a) ~
    ORDER BY \"time\" ASC, \"id\" ASC ~
    LIMIT $5"))

(defparameter *search-messages*
  (string-formatter
   "SELECT * ~
    FROM \"chatlog\" ~
    WHERE (\"channel-id\"=(SELECT \"id\" FROM \"channels\" WHERE ~
                               \"channels\".\"server\"=$1 ~
                           AND (\"channels\".\"channel\"=$2 ~
                                OR trim(leading '#' from \"channels\".\"channel\")=$2)) ~
       AND \"time\">=$3 ~
       AND \"time\"<=$4 ~
       AND \"nick\" ILIKE $5 ~
       AND \"message\" ILIKE $6 ~
       ~a) ~
    ORDER BY \"time\" ASC, \"id\" ASC ~
    LIMIT $7"))

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

(define-condition rate-exceeded (error)
  ((time-left :initarg :time-left :accessor time-left)))

(rate:define-limit chatlog-search (time-left :timeout 10)
  (error 'rate-exceeded :time-left time-left))

(defun fetch (server channel types from to by search amount)
  (let ((amount (or (parse-i amount) 500)))
    (when (< 1000 amount)
      (error 'api-argument-invalid :argument "amount" :message "Amount cannot be higher than 10000."))
    (multiple-value-bind (where args) (compute-where types (if (or search by) 8 6))
      (with-connection ()
        (if (or search by)
            (rate:with-limitation (chatlog-search)
              (apply #'query (funcall *search-messages* where) server channel from to (or by "%") (or search "%") amount args))
            (apply #'query (funcall *select-messages* where) server channel from to amount args))))))

(defun channels ()
  (let ((channels ()))
    (loop for (server channel) in (with-connection ()
                                    (postmodern:query (funcall *select-channels*)))
          do (let ((cons (assoc server channels :test #'string=)))
               (if cons
                   (setf (cdr cons) (cons channel (cdr cons)))
                   (push (list server channel) channels))))
    channels))

(define-api chatlog/get (server channel &optional types from to around by search (amount "1000") (format "objects")) ()
  (multiple-value-bind (from to) (parse-time-region from to around)
    (let ((events (fetch server channel types from to (when (and by (string/= by "")) by) (parse-search search) amount)))
      (cond ((string-equal format "objects")
             (api-output events))
            ((string-equal format "rendered")
             (setf (content-type *response*) "text/plain")
             (with-output-to-string (stream)
               (loop initially (format stream "~&--- ~a/~a ~a" server channel (format-fancy-date (unix-to-universal-time (gethash "time" (first events)))))
                     for event in events
                     do (format stream "~&~a <~a> ~a"
                                (format-clock-time (unix-to-universal-time (gethash "time" event))) (gethash "nick" event) (gethash "message" event))
                     finally (format stream "~&--- ~a/~a ~a" server channel (format-fancy-date (unix-to-universal-time (gethash "time" (first events))))))))))))

(define-api chatlog/channels () ()
  (api-output (channels)))

(define-page view "irclog/^([a-zA-Z-]+)/(#*[a-zA-Z0-5_\\-]+)$" (:uri-groups (server channel) :clip "view.ctml")
  (setf (content-type *response*) "application/xhtml+xml")
  (let* ((search (post/get "search"))
         (by (post/get "by"))
         (type (or (get-var "type[]") NIL))
         (types (or* (get-var "types") (format NIL "~{~a~}" type) *default-types*)))
    (multiple-value-bind (from to) (parse-time-region (get-var "from") (get-var "to") (get-var "around"))
      (handler-case
          (r-clip:process
           T :messages (fetch server channel types from to (when (and by (string/= by "")) by) (parse-search search) 1000)
             :from from
             :to to
             :timestep *timestep*
             :types types
             :page (format NIL "~a/~a" server channel))
        (rate-exceeded (err)
          (r-clip:process (@template "rate-exceeded.ctml")
                          :time-left (time-left err)))))))

(defun format-datetime-timestring (unix)
  (local-time:format-timestring NIL (local-time:unix-to-timestamp unix)
                                :format '((:year 4) #\- (:month 2) #\- (:day 2) #\T (:hour 2) #\: (:min 2) #\: (:sec 2))))

(define-page log-search "irclog/search" (:clip "search.ctml")
  (cond ((string= (post/get "action") "search")
         (redirect (uri-to-url (make-uri :path (post/get "channel") :domains '("irclog"))
                               :representation :external
                               :query `(("from" . ,(post/get "from"))
                                        ("to" . ,(post/get "to"))
                                        ("search" . ,(post/get "search"))
                                        ("by" . ,(post/get "by"))))))
        (T
         (setf (content-type *response*) "application/xhtml+xml")
         (multiple-value-bind (from to) (parse-time-region (get-var "from") (get-var "to") (get-var "around"))
           (r-clip:process
            T :channel (post/get "channel")
              :channels (loop for (server . channels) in (channels)
                              nconc (loop for channel in channels
                                          collect (format NIL "~a/~a" server channel)))
              :from (format-datetime-timestring from)
              :to (format-datetime-timestring to)
              :search (post/get "search")
              :by (post/get "by"))))))

(define-page index "irclog/^$" (:clip "index.ctml")
  (setf (content-type *response*) "application/xhtml+xml")
  (r-clip:process
   T :channels (channels)))

(define-route log.irc->irclog :mapping (uri)
  (when (equalp (domains uri) '("irc" "log"))
    (setf (domains uri) (list "irclog"))))

(define-trigger server-start ()
  (open-connections))

(define-trigger server-stop ()
  (close-connections))
