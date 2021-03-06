# pyffi

Chicken Scheme interface to the Python programming language.

## Documentation


The pyffi extension uses the Python (http://www.python.org) C
interface to invoke Python procedures and methods from within Scheme
and to convert data objects between their Python and Scheme
representation.

Currently, pyffi supports the Python data types listed in the
table below. All other Python data types are represented as pointers.

- int, float : numeric types, converted to and from the corresponding Scheme numeric values
- list : list type, converted to and from Scheme list value
- str : string type, converted to and from Scheme string value
- unicode : Unicode string type, converted to and from Scheme utf8 string
- dict : dictionary type, converted to and from Scheme alist value
- instance : class instance type, converted to Scheme alist value
- tuple : tuple type, converted to and from Scheme vector value

## Installation

The setup script of pyffi attempts to autodetect the location of the
Python header files and libraries for Python versions 2.3-2.7 or
3.x. The autodetection routine checks some standard installation
locations for Linux, Mac OS X, and Windows. If autodetection fails,
you may also specify the header and library locations as follows:

```
 PYTHON_CFLAGS=-I/usr/include/python3.6 PYTHON_LFLAGS=-L/usr/lib PYTHON_LIBS=-lpython3.6 chicken-install pyffi
```

In order to determine the correct paths, the command below can be used:

```
python3 -c "from sysconfig import get_paths; import pprint; pprint.pprint(get_paths())"
```

This returns something like:
```
{'data':        '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9',
 'include':     '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/include/python3.9',
 'platinclude': '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/include/python3.9',
 'platlib':     '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/lib/python3.9/site-packages',
 'platstdlib':  '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/lib/python3.9',
 'purelib':     '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/lib/python3.9/site-packages',
 'scripts':     '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/bin',
 'stdlib':      '/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/lib/python3.9'}
```

From that output include (for PYTHON_CFLAGS) and stdlib (for
PYTHON_LFLAGS) are of interest. With this information the following environment
variables can be passed to chicken-install:

```
PYTHON_CFLAGS=-I/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/include/python3.9 \
PYTHON_LFLAGS=-L/usr/local/Cellar/python@3.9/3.9.1_6/Frameworks/Python.framework/Versions/3.9/lib \
PYTHON_LIBS=-lpython3.9 \
chicken-install pyffi
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

`(py-object-type value)`

Returns the type string of the given Python object.

`(py-object-to value)`

Returns the Python representation of the given Scheme object.

`(py-object-from)`

Returns the Scheme representation of the given Python object, or the corresponding pointer.


## Macros


`(define-pyfun expr arg1 ...)`

Defines a Scheme procedure named when called, invokes the Python
procedure of the same name. `EXPR` is either a string that contains
the procedure name, or a pair of the form `(FORM NAME)` where `FORM`
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



`(define-pymethod NAME [SCHEME-NAME: NAME] [KW: ARGS])`

Defines an accessor for the Python method `NAME`. The optional keyword
argument `SCHEME-NAME` is an alternate name for the Scheme
procedure. The optional keyword argument `KW` is a list of keyword
argument names. The accessor is a procedure of the form `LAMBDA OBJ
ARG1 ...` that takes in a Python object `OBJ` and invokes the method
`NAME` contained in that object, with the supplied arguments, which
must be either Scheme values of types listed in the type conversion
table above, or pointers to Python values. If the object has no such
method, #f is returned.



## Examples

### h5py

```scheme
(import scheme (chicken base) pyffi)

(py-start)

(py-import "h5py")
(py-import "numpy")

(define-pyfun "h5py.File" name mode)
(define-pymethod "close" scheme-name: File.close) ;; File
(define-pymethod "create_dataset" scheme-name: File.create_dataset kw: (dtype)) ;; File

(define-pyslot "shape" Dataset.shape)
(define-pyslot "dtype" Dataset.dtype)
(define-pyslot "name" Dataset.name)

(define f (h5py.File "mytestfile.hdf5" "w"))

(define dset (File.create_dataset f "mydataset"  (vector 100) dtype: "i"))

(print (py-object-from (Dataset.name dset)))
(print (py-object-from (Dataset.shape dset)))
(print (py-object-type (Dataset.dtype dset)))

(File.close f)
```

### Matplotlib

```scheme
(import  pyffi)

(py-start)

(define (plot-data x-list y-list #!optional (filename "plot.png"))
  ;; rough translation of the code inside this function to python:
  ;; import matplotlib.pyplot
  ;; fig-ax-tuple = matplotlib.pyplot.subplots()
  ;; fig = fig-ax-tuple[0]
  ;; ax = fig-ax-tuple[1]
  ;; ax.plot(x-list, y-list)
  ;; fig.savefig(filename)
  (py-import "matplotlib.pyplot")
  (define-pyfun "matplotlib.pyplot.subplots")
  (define-pymethod "plot" x-list y-list)
  (define-pymethod "savefig" file)
  (let* ((fig-ax-tuple (matplotlib.pyplot.subplots))
         (fig (vector-ref fig-ax-tuple 0))
         (ax (vector-ref fig-ax-tuple 1)))
    (plot ax x-list y-list)
    (savefig fig filename)))

(plot-data (list 1 2 3) (list 1 4 9) "test-plot.png")

(py-stop)
``` 

## License

>
> Copyright 2007-2020 Ivan Raikov. Based on pyffi.lisp by Dmitri Hrapof.
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

