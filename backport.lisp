#|
This file is a part of chatlog
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:chatlog-backport
  (:use #:cl)
  (:export
   #:insert
   #:process-clozure
   #:process-weechat
   #:process-file
   #:process-with-db))
(in-package #:chatlog-backport)

(defvar *server* "freenode")
(defvar *channel* "#lisp")

(defun insert (time server channel nick type message)
  "Perform the actual insert."
  (format T "~&Inserting ~a ~a/~a <~a> [~a] ~a" time server channel nick type message)
  (postmodern:execute (format NIL "INSERT INTO \"chatlog\" (\"time\", \"server\", \"channel\", \"nick\", \"type\", \"message\") VALUES($1,$2,$3,$4,$5,$6)")
                      time server channel nick type message))

(defun process-clozure (line)
  "Process a line read from a clozure log file."
  (cl-ppcre:register-groups-bind (time nick NIL message action rest)
      ("^([0-9-T:Z]+) ([a-zA-Z0-9-_`]+)(:(.*)| (is now known as|quit|joined|left) (.*))$" line)
    (let ((time (local-time:timestamp-to-unix
                 (local-time:parse-timestring time))))
      (flet ((insert (type message)
               (insert time *server* *channel* nick type message)))
        (cond (message
               (insert "m" message))
              ((string-equal action "is now known as")
               (insert "n" (format NIL " ** NICK ~a" rest)))
              ((string-equal action "quit")
               (insert "q" (format NIL " ** QUIT ~a" rest)))
              ((string-equal action "joined")
               (insert "j" " ** JOIN"))
              ((string-equal action "left")
               (insert "p" " ** PART")))))))

(defun process-weechat (line)
  (cl-ppcre:register-groups-bind (time NIL joiner NIL quitter reason NIL oldnick newnick NIL sender message)
      ("^(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\s(-->\\s[+~@]?([^ ]+) )?(<--\\s[+~@]?([^ ]+) \\(.*?\\) has quit \\((.*)\\)$)?(--\\s[+~@]?([^ ]+) is now known as [+~@]?([^ ]+))?(>[+~@]?([^>]+)>\\s(.*))?" line)
    (let ((time (local-time:timestamp-to-unix
                 ;; Piece of shit timezone missing defaults. Kludge it by assuming it's our timezone too.
                 (local-time:parse-timestring time :date-time-separator #\  :offset (nth-value 9 (local-time:decode-timestamp (local-time:now)))))))
      (flet ((insert (type message)
               (insert time *server* *channel* (or joiner quitter oldnick sender) type message)))
        (cond (joiner (insert "j" " ** JOIN"))
              (quitter (insert "q" (format NIL " ** QUIT ~a" reason)))
              (oldnick (insert "n" (format NIL " ** NICK ~a" newnick)))
              (sender (insert "m" message)))))
    (values time joiner quitter reason oldnick newnick sender message)))

(defun process-file (file &key (processor #'process-clozure))
  "Process a log file line by line."
  (with-open-file (stream file :direction :input
                               :element-type 'character
                               :if-does-not-exist :error)
    (with-simple-restart (abort "Abort processing the file altogether.")
      (loop for line = (read-line stream NIL NIL)
            while line
            do (with-simple-restart (skip "Skip processing this line.")
                 (when (string/= line "")
                   (funcall processor line)))))))

(defun process-with-db (file &key (host "127.0.0.1")
                                  (port 5432)
                                  (user "irc")
                                  (pass)
                                  (db "irc")
                                  (processor #'process-clozure)
                                  (server *server*)
                                  (channel *channel*))
  "Process a log file with the db connection established."
  (let ((*server* server)
        (*channel* channel))
    (postmodern:with-connection (list db user pass host :port port)
      (process-file file :processor processor))))
