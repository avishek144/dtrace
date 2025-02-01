;; package.lisp
;; Copyright (C) 2024 by Avishek Gorai <avishekgorai@myyahoo.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



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
