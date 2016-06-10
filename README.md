# pyffi

Chicken Scheme interface to the Python programming language.

## Documentation


The pyffi extension uses the [[http://www.python.org|Python]] C
interface to invoke Python procedures and methods from within Scheme
and to convert data objects between their Python and Scheme
representation.

Currently, pyffi supports the Python data types listed in the
table below. All other Python data types are represented as pointers.

int, float | numeric types, converted to and from the corresponding Scheme numeric values
list | list type, converted to and from Scheme list value
str | string type, converted to and from Scheme string value
unicode | Unicode string type, converted to and from Scheme utf8 string
dict | dictionary type, converted to and from Scheme alist value
instance | class instance type, converted to Scheme alist value
tuple | tuple type, converted to and from Scheme vector value

## Installation

The setup script of pyffi attempts to autodetect the location of
the Python header files and libraries for Python versions 2.3-2.7. The
autodetection routine checks some standard installation locations for
Linux, Mac OS X, and Windows. If autodetection fails, you may also
specify the header and library locations as follows:

```
 PYTHON_CFLAGS=-I/usr/include/python2.6 PYTHON_LFLAGS=-L/usr/lib PYTHON_LIBS=-lpython2.6 chicken-install pyffi
```

## Procedures


`(py-start)`

Initializes the Python interpreter, and creates an evaluation
environment. This procedure must be called before all other procedures
in the extension.

`(py-stop)`

Deallocates the memory reserved by the Python interpreter, and frees
all internal structures of the extension.

`(py-import name)`

Imports Python module `NAME`. If the import was unsuccessful, raises
an exception of type `'pyerror`.


`(py-eval expr)`

Evaluates the Python expression contained in the string `EXPR` and
returns the resulting value, either converted to Scheme
representation, or as a pointer to a Python value.

`(py-apply func arg1 ...)`

Applies the given arguments to the Python object `FUNC` and returns
the resulting value, either converted to Scheme representation, or as
a pointer to a Python value. The arguments must be either Scheme
values of types listed in the type conversion table above, or pointers
to Python values.


## Macros


`(define-pyfun expr arg1 ...)`

Defines a Scheme procedure named when called, invokes the Python
procedure of the same name. `EXPR` is either a string that contains
the procedure name, or a pair of the form `(FORM . NAME)` where `FORM`
is a Python expression that when evaluated returns a Python procedure,
and `NAME` is a string that contains the name of the Scheme
procedure. The arguments must be all symbols.



`(define-pyslot NAME [SCHEME-NAME])`

Defines an accessor/modifier for the Python object attribute
`NAME`. The optional argument `SCHEME-NAME` is an alternate name
for the Scheme procedure. The returned procedure takes in a Python
object and returns the value of attribute `NAME` contained in that
object. If the object has no such attribute, #f is returned. If a
value is supplied after the name of the object, the procedure acts as
a modifier for that slot.



`(define-pymethod NAME [SCHEME-NAME])`

Defines an accessor for the Python method `NAME`. The optional
argument `SCHEME-NAME` is an alternate name for the Scheme
procedure. The accessor is a procedure of the form `LAMBDA OBJ ARG1 ...` 
that takes in a Python object `OBJ` and invokes the method
`NAME` contained in that object, with the supplied arguments, which
must be either Scheme values of types listed in the type conversion
table above, or pointers to Python values. If the object has no such
method, #f is returned.



## Examples

```scheme
 ;;
 ;; Python-UNO example from pyffi.lisp by Dmitri Hrapof.
 ;;
 ;; Before running the following code, you must make sure OpenOffice is
 ;; running as a server:
 ;;
 ;;  soffice "-accept=socket,host=localhost,port=2002;urp;"
 ;;
 (require-extension pyffi)
 
 (py-start)
 
 (py-import "uno")
 
 (define-pyfun "uno.getComponentContext")
 
 (define-pyslot "ServiceManager")
 (define-pyslot "Text")
 
 ;; Can't yet find out appropriate class, the following is lame
 (define-pymethod "createInstanceWithContext")
 (define-pymethod "resolve")
 (define-pymethod "getCurrentComponent")
 (define-pymethod "createTextCursor")
 (define-pymethod "insertString")
 
 (define (message-uno str)
   (let* ((lc (uno.getComponentContext))
 	 (resolver (createInstanceWithContext 
 		    (ServiceManager lc)
 		    "com.sun.star.bridge.UnoUrlResolver" lc))
 	 (ctx (resolve resolver "uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext"))
 	 (desktop (createInstanceWithContext 
 		   (ServiceManager ctx)
 		   "com.sun.star.frame.Desktop" ctx))
 	 (model   (getCurrentComponent desktop))
 	 (text    (Text model))
 	 (cursor  (createTextCursor text)))
     (insertString text cursor str 0)))
 
 
 (message-uno "Hello, world!")
``` 

## Version history

- 3.0 :  using [[bind]] instead of [[easyffi]]
- 2.18 : setup script updated for Python 2.7 (thanks to Mario Domenech Goulart)
- 2.17 : Documentation converted to wiki format
- 2.16 : setup script updated for Python 2.6
- 2.15 : Changed getattr to not convert return result to Scheme value
- 2.14 : Eliminated dependency on matchable
- 2.13 : More fixes in converting Python values to Scheme
- 2.12 : Fixes in converting Python values to Scheme
- 2.11 : Ported to Chicken 4

## License

>
> Copyright 2007-2012 Ivan Raikov. Based on pyffi.lisp by Dmitri Hrapof.
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or (at
> your option) any later version.
> 
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.
> 
> A full copy of the GPL license can be found at
> <http://www.gnu.org/licenses/>.

