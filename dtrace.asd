;;; dtrace.asd -- Dtrace system definition.
;;; Copyright (C) 2024, 2025 by Avishek Gorai <avishekgorai@myyahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



(asdf:defsystem #:dtrace
  :description "This package constains two functions DTRACE which provides more detailed trace display than most implementation specific TRACE.  It also has some parameters which control the behaviour of DTRACE."
  :author "Avishek Gorai <avishekgorai@myyahoo.com>"
  :license  "GNU General Public License version 3 or later"
  :version "1.5"
  :serial t
  :components ((:file "package")
               (:file "dtrace")))
