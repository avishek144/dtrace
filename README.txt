This program is given in the book "Common LISP: A Gentle Introduction
to Symbolic Computation" by David S. Touretzky.  It provides more
detailed trace display than most implementation specific TRACE. It has
two macros DTRACE and DUNTRACE, whose syntax is the same as TRACE
and UNTRACE respectively.  They are defined in a package named DTRACE.

There is one implementation-dependent function: FETCH-ARGLIST.  It
takes a symbol as input and returns the argument list of the function
named by that symbol.  Here it is defined to return NIl, so, the
arguments will be displayed as Arg-1, Arg-2, and so on.  Modify this
using your implementation's function which returns returns the
argument list.

To use, just load Dtrace.lisp using LOAD function.

BUG: After calling dtrace, a warning shows up: Function is not TRACEd:
<The function name>.
