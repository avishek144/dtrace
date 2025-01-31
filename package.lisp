;;;; package.lisp
;;
;;;; Copyright (c) 2025 Avishek Gorai <avishekgorai@myyahoo.com>


(defpackage #:Dtrace
  (:use #:cl)
  (:export dtrace
           duntrace
           *dtrace-print-length*
           *dtrace-print-level*
           *dtrace-print-circle*
           *dtrace-print-pretty*
           *dtrace-print-array*)
  (:documentation "This package contains DTRACE and DUNTRACE. Which works like TRACE and UNTRACE but produces more detailed trace display."))
