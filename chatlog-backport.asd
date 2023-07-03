(asdf:defsystem chatlog-backport
  :version "0.0.1"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Backporting functions to insert logs from a log file."
  :serial T
  :components ((:file "backport"))
  :depends-on (:cl-ppcre :postmodern :local-time))
