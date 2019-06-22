#|
 This file is a part of chatlog
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem chatlog
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :name "chatlog"
  :version "0.0.1"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Frontend for Colleen's chatlog-pg module."
  :serial T
  :components ((:file "chatlog"))
  :depends-on ((:interface :rate)
               :r-clip
               :postmodern
               :local-time
               :ubiquitous
               :cl-ppcre))
