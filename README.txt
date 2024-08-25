This program is given in the book "Common LISP: A Gentle Introduction to Symbolic Computation" by David S. Touretzky.  It provides more detailed trace display than most implementation specific TRACE. It has two function DTRACE and DUNTRACE, whose syntax is the same as TRACE and UNTRACE respectively.  They are defined in a package named DTRACE.

This generic version contains one implementation-dependent function: FETCH-ARGLIST.  It takes a symbol as input and returns the argument list of the function named by that symbol.
