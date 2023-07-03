(defsystem chatlog
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :name "chatlog"
  :version "0.0.1"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Frontend for Colleen's chatlog-pg module."
  :serial T
  :components ((:file "chatlog"))
  :depends-on ((:interface :rate)
               :r-clip
               :postmodern
               :local-time
               :ubiquitous
               :cl-ppcre))
