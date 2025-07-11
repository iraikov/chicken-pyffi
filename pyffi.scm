;;
;; Python-Scheme FFI. Based on pyffi.lisp by Dmitri Hrapof.
;; Adapted to Chicken Scheme by Ivan Raikov.
;; Chicken Scheme code copyright 2007-2025 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(module pyffi

   (py-start py-stop py-import py-eval py-apply py-object-type py-object-to py-object-from
	     raise-python-exception python-exception-string
	     *py-functions* 
	     (define-pyfun PyCallable_Check Py_DecRef)
	     (define-pyslot PyObject_GetAttrString PyObject_SetAttrString )
	     (define-pymethod PyObject_GetAttrString PyObject_CallObject PyObject_Call parse-argument-list ))


   (import scheme (chicken base) (chicken foreign) (chicken keyword) (chicken syntax) 
           (chicken blob) (chicken locative) (chicken string) (chicken condition)
           (only (chicken memory) pointer?) (only (chicken port) port-name)
           (only srfi-1 first second every break)
           srfi-4 srfi-69 bind utf8 utf8-lolevel utf8-srfi-13 utf8-srfi-14)

   (import-for-syntax (chicken base) (chicken keyword) (chicken string)
                      (only srfi-1 first second every break)
                      srfi-69)
   
(define (pyffi:error x . rest)
  (let ((port (open-output-string)))
    (if (port? x)
	(begin
	  (display "[" port)
	  (display (port-name x) port)
	  (display "] " port)))
    (let loop ((objs (if (port? x) rest (cons x rest))))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'pyffi (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))))))

(define (alist? x)  (and (list? x) (every pair? x)))

(define-record pytype name to from)

;; Fixed macros using implicit renaming (ir-macro-transformer)
;; Only using inject when we need to break hygiene for specific bindings

(define-syntax define-pytype
  (ir-macro-transformer
   (lambda (x inject compare)
     (let ((name (cadr x))
           (to   (caddr x))
           (from (cadddr x)))
       `(define ,name (make-pytype ',name ,to ,from))))))

(define-syntax translate-to-foreign
  (ir-macro-transformer
   (lambda (x inject compare)
     (let ((x (cadr x))
           (typ (caddr x)))
       `((pytype-to ,typ) ,x)))))

(define-syntax translate-from-foreign
  (ir-macro-transformer
   (lambda (x inject compare)
     (let ((x (cadr x))
           (typ (caddr x)))
       `((pytype-from ,typ) ,x)))))

(define-syntax define-pyfun
  (ir-macro-transformer
   (lambda (x inject compare)
     (let* ((expr (cadr x))
            (args (cddr x))
            (func (gensym 'func))  ; Generate a unique name for the Scheme variable
            (name (if (list? expr) 
                      (second expr) 
                      (string->symbol expr)))
            (form (if (list? expr) (first expr) expr)))
       `(define ,(cons name args)
          (let ((,func (hash-table-ref/default ,(inject '*py-functions*) ',name #f)))
            (if (not ,func)
                (begin
                  (set! ,func (py-eval ,form))
                  (if (not ,func)
                      (raise-python-exception))
                  (if (not (PyCallable_Check ,func))
                      (begin
                        (Py_DecRef ,func)
                        (raise-python-exception)))
                  (hash-table-set! ,(inject '*py-functions*) ',name ,func)))
            (py-apply ,func . ,args)))))))

(define-syntax define-pyslot
  (ir-macro-transformer
   (lambda (x inject compare)
     (let ((name (cadr x)) 
           (rest (cddr x)))
       (let-optionals rest ((scheme-name #f))
         (let ((proc-name (or scheme-name (string->symbol name)))
               (obj (gensym 'obj))
               (rest (gensym 'rest)))
           `(define (,proc-name ,obj . ,rest)
              (if (null? ,rest)
                  (PyObject_GetAttrString ,obj ,(->string name))
                  (PyObject_SetAttrString ,obj ,(->string name) (car ,rest))))))))))

(define-syntax define-pymethod
  (ir-macro-transformer
   (lambda (x inject compare)
     (let ((name (cadr x)) 
           (rest (cddr x)))
       (let ((scheme-name (member 'scheme-name: rest))
             (kw (member 'kw: rest)))
         (let ((proc-name (or (and scheme-name (cadr scheme-name)) (string->symbol name)))
               (obj (gensym 'obj))
               (rest (gensym 'rest)))
           (if (not kw)
               `(define (,proc-name ,obj #!rest ,rest)
                  (PyObject_CallObject
                   (PyObject_GetAttrString ,obj ,(->string name))
                   (list->vector ,rest)))
               (let ((keyword-symbols (cadr kw)))
                 `(define ,proc-name
                    ;; Create the keyword list once when the function is first defined
                    (let ((cached-accepted-kwargs 
                           (map (lambda (sym) (string->keyword (symbol->string sym))) 
                                ',keyword-symbols)))
                      ;; Return the actual function that uses the cached list
                      (lambda (,obj #!rest ,rest)
                        (receive (args kwargs)
                            (parse-argument-list ,rest cached-accepted-kwargs)
                          (PyObject_Call 
                           (PyObject_GetAttrString ,obj ,(->string name))
                           (list->vector args)
                           (if (null? kwargs) '() kwargs))))))))))))))

;; Scheme -> Python
(define (py-object-to value)
  (cond
   ((exact-integer? value)  (translate-to-foreign value py-int))
   ((cplxnum? value)  (translate-to-foreign value py-complex))
   ((real? value)     (translate-to-foreign value py-float))
   ((alist? value)    (translate-to-foreign value py-dict))
   ((list? value)     (if (eq? 'ascii (car value)) 
			  (translate-to-foreign (cadr value) py-ascii)
			  (translate-to-foreign value py-list)))
   ((string? value)   (translate-to-foreign value py-unicode))
   ((vector? value)   (translate-to-foreign value py-tuple))
   ((blob? value)     (translate-to-foreign value py-buffer))
   ((boolean? value)  (translate-to-foreign value py-bool))
   ((pointer? value)  value)
   (else (pyffi:error 'py-object-to "invalid value " value))))

;; Python -> Scheme
(define (py-object-from value)
  (if (not value) (raise-python-exception))
  (let ((typ-name  (py-object-type value)))
    (let ((typ-key (alist-ref typ-name *py-types* string=?)))
      (if typ-key
	  (translate-from-foreign value typ-key)
	  (begin
	    (Py_IncRef value)
	    value)))))

(define (py-object-type value)
  (pyffi_PyObject_Type_toCString value))

;; Embed, but do not parse
#>

#include <Python.h>

#if ((PY_MAJOR_VERSION == 2) && (PY_MINOR_VERSION <= 3))
void Py_IncRef (PyObject *x)
{
  Py_INCREF(x);
}

void Py_DecRef (PyObject *x)
{
  Py_DECREF(x);
}
#endif
PyObject *pyffi_PyImport_ImportModuleEx (char *, PyObject *, PyObject *, PyObject *);


PyObject *pyffi_PyRun_String (const char *str, int s, PyObject *g, PyObject *l)
{
   return PyRun_String (str, s, g, l);
}


PyObject *pyffi_PyImport_ImportModuleEx (char *name, PyObject *g, PyObject *l, PyObject *fl)
{
   return PyImport_ImportModuleEx (name,g,l,fl);
}


C_word PyBool_asBool(PyObject *x)
{
   if (x == (Py_True)) return C_SCHEME_TRUE;
   return C_SCHEME_FALSE;
}

#if PY_MAJOR_VERSION >= 3
#define PyInt_AsLong PyLong_AsLong
#define PyInt_FromLong PyLong_FromLong
#endif

<#


(define PyBool-AsBool (foreign-lambda scheme-object "PyBool_asBool" nonnull-c-pointer))

(bind-type pyobject (c-pointer "PyObject") py-object-to py-object-from)

(define pyffi_PyUnicode_ref (foreign-lambda integer "pyffi_PyUnicode_ref" pyobject integer))


;; Parse but do not embed
(bind #<<EOF
void Py_Initialize (void);
void Py_Finalize (void);

void Py_IncRef (PyObject *);
void Py_DecRef (PyObject *);

int PyCallable_Check (PyObject *);
PyObject *PyErr_Occurred (void);
void PyErr_Clear (void);

PyObject *PyDict_New (void);
pyobject PyDict_Keys (PyObject *);
int PyDict_Size  (PyObject *);
pyobject PyDict_GetItem (PyObject *, pyobject);
pyobject PyDict_GetItemString (PyObject *, const char *); 
pyobject PyDict_Items (PyObject *); 
int PyDict_SetItem (PyObject *, pyobject, pyobject);

double PyFloat_AsDouble (PyObject *); 
PyObject *PyFloat_FromDouble (double);

PyObject* PyComplex_FromDoubles(double real, double imag);
double PyComplex_RealAsDouble(PyObject *op);
double PyComplex_ImagAsDouble(PyObject *op);

pyobject PyImport_GetModuleDict (void);
PyObject *PyImport_Import (pyobject );

PyObject *PyImport_ImportModule (const char *name); 
PyObject *PyImport_AddModule (const char *name); 

long PyInt_AsLong (PyObject *);
PyObject *PyInt_FromLong (long);

PyObject *PyList_New (int);
int PyList_Size (PyObject *);
pyobject PyList_GetItem (PyObject *, int); 
int PyList_SetItem (PyObject *, int, pyobject);

int PyModule_AddObject (PyObject *, char *, PyObject *); 
pyobject PyModule_GetDict (PyObject *); 
pyobject PyObject_CallObject (PyObject *, pyobject);
pyobject PyObject_Call (PyObject *, pyobject, pyobject);

pyobject PyObject_GetAttrString (PyObject *, const char *); 
int PyObject_SetAttrString (PyObject *, const char *, pyobject); 
pyobject PyObject_Str (PyObject *);

PyObject *PyTuple_New (int);
int PyTuple_Size (PyObject *);
pyobject PyTuple_GetItem (PyObject *, int);
int PyTuple_SetItem (PyObject *, int, pyobject);

PyObject *PyBool_FromLong(long);

PyObject *PyBytes_FromStringAndSize(const char *, long);
int PyBytes_Check(PyObject *);
long PyBytes_Size(PyObject *);
char *PyBytes_AsString(PyObject *);
int PyByteArray_Check(PyObject *);
long PyByteArray_Size(PyObject *);
char *PyByteArray_AsString(PyObject *);

int PyUnicode_Check(PyObject *);

int PyObject_GetBuffer(PyObject *exporter, Py_buffer *view, int flags);
int PyBuffer_ToContiguous(void *buf, Py_buffer *src, int len, char order);
int PyBuffer_FromContiguous(Py_buffer *view, void *buf, int len, char order);
void PyBuffer_Release(Py_buffer *view);
EOF
)


;; Parse & embed
(bind* #<<EOF

PyObject *pyffi_PyImport_ImportModuleEx (char *, PyObject *, PyObject *, pyobject);
pyobject pyffi_PyRun_String (const char *str, int s, PyObject *g, PyObject *l);

PyObject *PyModule_GetDict_asPtr (PyObject *x)
{
 return PyModule_GetDict (x);
}

PyObject* pyffi_PyString_fromCString(const char *string)
{
#if PY_MAJOR_VERSION >= 3
  return PyUnicode_FromString(string);
#else
  return PyBytes_FromString(string);
#endif
}

char *pyffi_PyString_toCString(pyobject op) 
{
#if PY_MAJOR_VERSION >= 3
  const char *utf8 = PyUnicode_AsUTF8(op);
  if (!utf8) return NULL;
  return strdup(utf8);
#else
  const char *bytes = PyBytes_AsString(op);
  if (!bytes) return NULL;
  return strdup(bytes);
#endif
}

PyObject* pyffi_PyUnicode_fromCString(const char *string)
{
  return PyUnicode_FromString(string);
}

char *pyffi_PyUnicode_toCString(pyobject op) 
{
#if PY_MAJOR_VERSION >= 3
  const char *utf8 = PyUnicode_AsUTF8(op);
  if (!utf8) return NULL;
  return strdup(utf8);
#else
  const char *data = PyUnicode_AS_DATA(op);
  if (!data) return NULL;
  return strdup(data);
#endif
}

long pyffi_PyUnicode_GetLength(pyobject op) 
{
#if PY_MAJOR_VERSION >= 3
  return PyUnicode_GET_LENGTH(op);
#else
  return PyUnicode_GET_SIZE(op);
#endif
}

char *pyffi_PyObject_Type_toCString (pyobject x)
{
  PyObject *typ, *str;
  char *result;

  typ = PyObject_Type (x);
  if (!typ) return NULL;
  
  str = PyObject_Str (typ);
  Py_DecRef (typ);
  
  if (!str) return NULL;

#if PY_MAJOR_VERSION >= 3
  const char *utf8 = PyUnicode_AsUTF8(str);
  result = utf8 ? strdup(utf8) : NULL;
#else
  const char *bytes = PyBytes_AsString(str);
  result = bytes ? strdup(bytes) : NULL;
#endif

  Py_DecRef (str);
  return result;
}

char *pyffi_PyErr_Occurred_toCString (void)
{
  PyObject *exc, *str;
  char *result;

  exc = PyErr_Occurred ();  /* Borrowed reference */
  if (!exc) return NULL;
  
  str = PyObject_Str (exc);
  if (!str) return NULL;

#if PY_MAJOR_VERSION >= 3
  const char *utf8 = PyUnicode_AsUTF8(str);
  result = utf8 ? strdup(utf8) : NULL;
#else
  const char *bytes = PyBytes_AsString(str);
  result = bytes ? strdup(bytes) : NULL;
#endif

  Py_DecRef (str);  /* Only decref str, not exc */
  return result;
}

/* Safe Unicode character access */
int pyffi_PyUnicode_ref (PyObject *x, int i)
{
  if (!x || !PyUnicode_Check(x)) return 0;
  
  Py_ssize_t length = PyUnicode_GET_LENGTH(x);
  if (i < 0 || i >= length) return 0;

  Py_UCS4 char_code_point = PyUnicode_ReadChar(x, i);
  
  if (char_code_point == (Py_UCS4)-1 && PyErr_Occurred()) {
    return 0;
  }
  
  if (char_code_point > 0x10FFFF) return 0;  /* Invalid Unicode */
  
  return (int)char_code_point;
}

/* Memory cleanup utility */
void pyffi_free_string(char *str) 
{
  if (str) free(str);
}

int PyBuffer_Size(Py_buffer *buf)
{
    return buf->len;
}

EOF
)

(define (pyerror-exn x) (make-property-condition 'pyerror 'message x))

(define (raise-python-exception)
  (let* ((desc (pyffi_PyErr_Occurred_toCString)))
    (PyErr_Clear)
    (print-error-message desc)
    (signal (pyerror-exn desc))))

(define (python-exception-string)
  (let* ((desc (pyffi_PyErr_Occurred_toCString)))
    desc))

(define-pytype py-int PyInt_FromLong PyInt_AsLong)

(define-pytype py-tuple
  (lambda (value)
    (let* ((len (vector-length value))
	   (tup (PyTuple_New len)))
      (if (not tup) (raise-python-exception))
      (let loop ((i 0))
        (if (< i len) 
            (begin
              (if (not (zero? (PyTuple_SetItem tup i (vector-ref value i))))
                  (raise-python-exception))
              (loop (+ 1 i)))
            tup))))
  (lambda (value)
    (let* ((len (PyTuple_Size value))
	   (tup (make-vector len)))
      (let loop ((i 0))
	(if (< i len) 
	    (begin
	      (vector-set! tup i (PyTuple_GetItem value i))
	      (loop (+ 1 i)))
	    tup)))))


(define-pytype py-list
  (lambda (value)
    (let* ((len (length value))
	   (lst (PyList_New len)))
       (if (not lst) (raise-python-exception))
       (let loop ((i 0))
	 (if (< i len)
	     (begin
	       (if (not (zero? (PyList_SetItem lst i (list-ref value i))))
		   (raise-python-exception))
	       (loop (+ i 1)))
	     lst))))
  (lambda (value)
    (let ((len (PyList_Size value)))
      (let loop ((i 0) (lst (list)))
	(if (< i len)
	    (let ((item (PyList_GetItem value i)))
	      (loop (+ 1 i) (cons item lst)))
	    (begin
	      (reverse lst)))))))


(define-pytype py-bool
  (lambda (x) (PyBool_FromLong (if x 1 0)))
  PyBool-AsBool)

(define-pytype py-float PyFloat_FromDouble PyFloat_AsDouble)

(define-pytype py-complex 
  (lambda (value)
    (let* ((r (real-part value))
	   (i (imag-part value)))
      (PyComplex_FromDoubles r i)))
  (lambda (value)
    (let* ((r (PyComplex_RealAsDouble value))
           (i (PyComplex_ImagAsDouble value)))
      (make-rectangular r i))))

(define (utf8-string->py-unicode value)
  ;; Given a Scheme UTF8 string, converts it into Python Unicode string
  (let ((res (pyffi_PyUnicode_fromCString value)))
    res))

(define (py-unicode->utf8-string value)
  ;; Given a Python Unicode string, converts it into Scheme UTF8 string
  (let ((len (pyffi_PyUnicode_GetLength value)))
      (let loop ((i 0) (lst (list)))
        (if (< i len)
            (loop (+ 1 i) (cons (pyffi_PyUnicode_ref value i) lst))
            (list->string (map integer->char (reverse lst)))))))

(define-pytype py-ascii pyffi_PyString_fromCString pyffi_PyString_toCString)
(define-pytype py-unicode utf8-string->py-unicode py-unicode->utf8-string)

(define-pytype py-dict 
  ;; Given a Scheme alist, converts it into a Python dictionary
  (lambda (value)
    (let ((dct (PyDict_New)))
      (if (not dct) (raise-python-exception))
      (for-each
       (lambda (kv)
         (if (and (pair? kv) (pair? (cdr kv)))
             (let ((k (car kv)) (v (cadr kv)))
               (if (not (zero? (PyDict_SetItem dct k v)))
                   (raise-python-exception)))
             (pyffi:error 'py-dict "invalid alist pair " kv)))
       value)
      dct
      ))
  ;; Given a Python dictionary, converts it into a Scheme alist
  (lambda (value)
    (let ((its (PyDict_Items value)))
      (let loop ((its its) (alst (list)))
	  (if (null? its) alst
	      (let ((item (car its)))
		(let ((k (vector-ref item 0))
		      (v (vector-ref item 1)))
		  (loop (cdr its) (cons (list k v) alst)))))))))


(define-pytype py-instance 
  identity
  ;; Given a Python class instance, converts it into a Scheme alist
  (lambda (value) (PyObject_GetAttrString value "__dict__")))


(define-pytype py-buffer 
  ;; Given a Scheme blob, converts it into a Python buffer
  (lambda (value)
    (let ((buf (make-blob (blob-size value))))
      (if (not buf) (raise-python-exception))
      (PyBuffer_FromContiguous (make-locative buf) (make-locative value) (blob-size value) #\C)
      buf
      ))
  ;; Given a Python buffer, converts it into a Scheme blob
  (lambda (value)
    (let ((b (make-blob (PyBuffer_Size value))))
      (PyBuffer_ToContiguous (make-locative b) value (PyBuffer_Size value) #\C)
      b))
  )


(define *py-types*
   `(
     ("<class 'bool'>"      . ,py-bool)
     ("<class 'bool'>"      . ,py-bool)
     ("<class 'int'>"       . ,py-int)
     ("<class 'float'>"     . ,py-float)
     ("<class 'complex'>"   . ,py-complex)
     ("<class 'list'>"      . ,py-list)
     ("<class 'str'>"       . ,py-ascii)
     ("<class 'unicode'>"   . ,py-unicode)
     ("<class 'dict'>"      . ,py-dict)
     ("<class 'instance'>"  . ,py-instance)
     ("<class 'tuple'>"     . ,py-tuple)
     ("<class 'buffer'>"    . ,py-buffer)
     )
   )


(define-constant +py-file-input+    257)
(define-constant +py-single-input+  256)
(define-constant +py-eval-input+    258)

(define  *py-main-module* (make-parameter #f))
(define  *py-main-module-dict* (make-parameter #f))
(define  *py-functions* (make-hash-table eq? symbol-hash))

(define (py-start)
  (Py_Initialize)
  (*py-main-module* (PyImport_AddModule "__main__"))
  (*py-main-module-dict* (PyModule_GetDict_asPtr (*py-main-module*)))
  (Py_IncRef (*py-main-module-dict*)))

(define (py-stop)
  (Py_DecRef (*py-main-module-dict*))
  (*py-main-module* #f)
  (*py-main-module-dict* #f)
  (hash-table-clear! *py-functions*)
  (Py_Finalize))

(define (py-import name #!key (as #f))
  (let* ((p (string-split name "."))
         (id (if (null? p) name (car p))))
    (let ((m (pyffi_PyImport_ImportModuleEx name (*py-main-module-dict*) #f '())))
      (if m
          (if (< (PyModule_AddObject (*py-main-module*) id m) 0)
              (begin
                (Py_DecRef m)
                (raise-python-exception))
              (begin
                (if as
                  (PyModule_AddObject (*py-main-module*) as m)
                  (Py_IncRef m))))
          (raise-python-exception)))
    ))


(define (py-eval expr)
  (pyffi_PyRun_String expr +py-eval-input+ (*py-main-module-dict*) #f))

(define (py-apply func . rest)
  (PyObject_CallObject func (list->vector rest)))

	  
(define (alistify-kwargs lst accepted-keywords)
  (let loop ((lst lst) (kwargs '()))
    (cond
     ((null? lst) kwargs)
     ((null? (cdr lst)) (pyffi:error 'alistify-kwargs "Bad keyword argument."))
     ((not (keyword? (car lst))) (pyffi:error 'alistify-kwargs "Not a keyword: " (car lst)))
     ((not (member (car lst) accepted-keywords)) (pyffi:error 'alistify-kwargs "Unexpected keyword argument: " (car lst)))
     (#t
      (loop (cddr lst) (cons (list (keyword->string (car lst)) (cadr lst)) kwargs))))))

(define (parse-argument-list args accepted-keywords)
  (receive (args* kwargs*)
      (break keyword? args)
    (values args* (alistify-kwargs kwargs* accepted-keywords))))



)
