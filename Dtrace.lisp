;; Dtrace.lisp -- Provides more detailed trace display than most implementations.
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


(in-package "DTRACE")

(export (quote
         (dtrace duntrace *dtrace-print-length*
                 *dtrace-print-level* *dtrace-print-circle*
                 *dtrace-print-pretty* *dtrace-print-array*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DTRACE and subordinate routines.

(defparameter *dtrace-print-length* 7)
(defparameter *dtrace-print-level* 4)
(defparameter *dtrace-print-circle* t)
(defparameter *dtrace-print-pretty* nil)
(defparameter *dtrace-print-array* *print-array*)

(defvar *traced-functions* nil)
(defvar *trace-level* 0)

(defmacro dtrace (&rest function-names)
  "Turns on detailed tracing for the specified functions.  Undo with DUNTRACE."
  (if (null function-names)
      (list (quote quote) *traced-functions*)
      (list (quote quote) (mapcan (function dtrace1) function-names))))

(defun dtrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~&~S is an invalid function name." name)
    (return-from dtrace1 nil))
  (unless (fboundp name)
    (format *error-output* "~&~S undefined function." name)
    (return-from dtrace1 nil))
  (eval `(untrace ,name))        ;; if they're tracing it, undo their trace
  (duntrace1 name)               ;; if we're tracing it, undo our trace
  (when (special-operator-p name)
    (format *error-output*
	    "~&Can't trace ~S because it's a special form." name)
    (return-from dtrace1 nil))
  (if (macro-function name)
      (trace-macro name)
      (trace-function name))
  (setf *traced-functions* (nconc *traced-functions* (list name)))
  (list name))

;;; The functions  below reference DISPLAY-xxx routines that can be made
;;; implementation specific for fancy graphics.  Generic versions of
;;; these routines are defined later in this file.

(defmacro with-dtrace-printer-settings (&body body)
  `(let ((*print-length* *dtrace-print-length*)
	 (*print-level* *dtrace-print-level*)
	 (*print-circle* *dtrace-print-circle*)
	 (*print-pretty* *dtrace-print-pretty*)
	 (*print-array* *dtrace-print-array*))
     ,@body))

(defun trace-function (name)
  (let* ((formal-arglist (fetch-arglist name))
	 (old-defn (symbol-function name))
	 (new-defn
	   (function (lambda (&rest argument-list)
	       (let ((result nil))
		 (display-function-entry name)
		 (let ((*trace-level* (1+ *trace-level*)))
		   (with-dtrace-printer-settings
		       (show-function-args argument-list formal-arglist))
		   (setf result (multiple-value-list
				 (apply old-defn argument-list))))
		 (display-function-return name result)
		 (values-list result))))))
    (setf (get name (quote original-definition)) old-defn)
    (setf (get name (quote traced-definition)) new-defn)
    (setf (get name (quote traced-type)) (quote defun))
    (setf (symbol-function name) new-defn)))

(defun trace-macro (name)
  (let* ((formal-arglist (fetch-arglist name))
	 (old-defn (macro-function name))
	 (new-defn
	   (function (lambda (macro-args env)
	       (let ((result nil))
		 (display-function-entry name (quote macro))
		 (let ((*trace-level* (1+ *trace-level*)))
		   (with-dtrace-printer-settings
		       (show-function-args macro-args formal-arglist))
		   (setf result (funcall old-defn macro-args env)))
		 (display-function-return name (list result) (quote macro))
		 (values result))))))
    (setf (get name (quote original-definition)) old-defn)
    (setf (get name (quote traced-definition)) new-defn)
    (setf (get name (quote traced-type)) (quote defmacro))
    (setf (macro-function name) new-defn)))

(defun show-function-args (actuals formals &optional (argcount 0))
  (cond ((null actuals) nil)
	((null formals) (handle-args-numerically actuals argcount))
	(t (case (first formals)
	     (&optional (show-function-args
			 actuals (rest formals) argcount))
	     (&rest (show-function-args
		     (list actuals) (rest formals) argcount))
	     (&key (handle-keyword-args actuals))
	     (&aux (show-function-args (rest actuals)
				       (rest formals)
				       (1+ argcount)))))))

(defun handle-args-numerically (actuals argcount)
  (dolist (x actuals)
    (incf argcount)
    (display-arg-numeric x argcount)))

(defun handle-one-arg (val varspec)
  (cond ((atom varspec) (display-one-arg val varspec))
	(t (display-one-arg val (first varspec))
	   (if (third varspec)
	       (display-one-arg t (third varspec))))))

(defun handle-keyword-args (actuals)
  (cond ((null actuals))
	((keywordp (first actuals))
	 (display-one-arg (second actuals) (first actuals))
	 (handle-keyword-args (rest (rest actuals))))
	(t (display-one-arg actuals "Extra args:"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DUNTRACE and subordinate routines.

(defmacro duntrace (&rest function-names)
  "Turns off tracing for the specified functions which were traced using DTRACE macro.  With no arguments, turns off all tracing by DTRACE."
  (setf *trace-level* 0) ;; safety precaution
  (list (quote quote)
	(mapcan (function duntrace1) (or function-names *traced-functions* ))))

(defun duntrace1 (name)
  (unless (symbolp name)
    (format *error-output* "~&~S is an invalid function name." name)
    (return-from duntrace1 nil))
  (setf *traced-functions* (delete name *traced-functions*))
  (let ((orign-defn (get name (quote original-definition) (quote none)))
	(traced-defn (get name (quote traced-definition)))
	(traced-type (get name (quote traced-type) (quote none))))
    (unless (or (eq orign-defn (quote none))
		(not (fboundp name))
		(not (equal traced-defn  ;; did it get redefined?
			    (ecase traced-type
			      (defun (symbol-function name))
			      (defmacro (macro-function name))))))
      (ecase traced-type
	(defun (setf (symbol-function name) orign-defn))
	(defmacro (setf (macro-function name) orign-defn)))))
  (remprop name (quote traced-definition))
  (remprop name (quote traced-type))
  (remprop name (quote original-definition))
  (list name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display routines
;;;
;;; The code below generates vanilla character output for ordinary
;;; displays.  It can be replaced with special graphics code if the
;;; implementation permits, e.g., on a PC you can use the IBM graphic
;;; character set to draw nicer-looking arrows.  On a color PC you
;;; can use different colors for arrows, for function names, for
;;; argument values, and so on.

(defparameter *entry-arrow-string* "----")
(defparameter *vertical-string*    "|   ")
(defparameter *exit-arrow-string*  "\\--")

(defparameter *trace-wraparound* 15)

(defun display-function-entry (name &optional ftype)
  (space-over)
  (draw-entry-arrow)
  (format *trace-output* "Enter ~S" name)
  (if (eq ftype (quote macro))
      (format *trace-output* " macro")))

(defun display-one-arg (val name)
  (space-over)
  (format *trace-output*
	  (typecase name
	    (keyword "  ~S ~S")
	    (string  "  ~A ~S")
	    (t "  ~S = ~S"))
	  name val))

(defun display-arg-numeric (val num)
  (space-over)
  (format *trace-output* "  Arg-~D = ~S" num val))

(defun display-function-return (name results &optional ftype)
  (with-dtrace-printer-settings
      (space-over)
    (draw-exit-arrow)
    (format *trace-output* "~S ~A"
	    name
	    (if (eq ftype (quote macro)) "expanded to" "returned"))
    (cond ((null results))
	  ((null (rest results))
	   (format *trace-output* " ~S" (first results)))
	  (t (format *trace-output* " values ~{~S, ~}~s"
		     (butlast results)
		     (car (last results)))))))



(defun space-over ()
  (format *trace-output* "~&")
  (dotimes (i (mod *trace-level* *trace-wraparound*))
    (format *trace-output* "~A" *vertical-string*)))

(defun draw-entry-arrow ()
  (format *trace-output* "~A" *entry-arrow-string*))

(defun draw-exit-arrow ()
  (format *trace-output* "~A" *exit-arrow-string*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The function FETCH-ARGLIST is implementation dependent.  It
;;; returns the formal argument list of a function as it would
;;; appear in a DEFUN or lambda expression, including any lambda
;;; list keywords.  This one works with any legal implementation.
;;; It returns nil, so the arguments are displayed as Arg-1, Arg-2,
;;; etc.

(defun fetch-arglist (fn) nil)
