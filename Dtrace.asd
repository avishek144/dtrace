;;;; Dtrace.asd
;;
;;;; Copyright (c) 2025 Avishek Gorai <avishekgorai@myyahoo.com>


(asdf:defsystem #:Dtrace
  :description "This package constains two functions DTRACE which provides more detailed trace display than most implementation specific TRACE.  It also has some parameters which control the behaviour of DTRACE."
  :author "Avishek Gorai <avishekgorai@myyahoo.com>"
  :license  "GNU General Public License version 3 or later"
  :version "1.5"
  :serial t
  :components ((:file "package")
               (:file "Dtrace")))
