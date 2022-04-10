(import (chicken condition) (chicken format) (chicken sort) brev-separate pyffi test)

(define-ir-syntax*
  ((test-signals-condition test-name kind body ...)
`(test-assert ,test-name (handle-exceptions exn ((condition-predicate ,kind) exn) (begin ,@body #f)))))

(py-start)
(py-import "sys")
(define-pyfun ("lambda x: (x.real, x.imag)" complex->tuple) x)
(define-pyfun ("lambda x: sorted(list(x.items()))" dict-items) x)
(define-pyfun "repr" obj)
(define-pyfun ("lambda r, obj: eval(r) == obj and type(eval(r)) == type(obj)" repr-compare) r obj)

(test-begin "basic tests")
(test-assert "major version is either 3 or 2" (member (py-eval "sys.version_info[0]") '(3 2)))
(test-assert "sys.version is a string" (string? (py-eval "sys.version")))
(print "Python version: " (py-eval "sys.version"))
(test-end "basic tests")

(test-begin "python->scheme type conversion")
(test "python integer converted to scheme" 12345 (py-eval "12345"))
;; Pick a number that has an exact binary floating point representation.
(test "python float converts to scheme" 0.25 (py-eval "0.25"))
(test "python inf converts to scheme" +inf.0 (py-eval "float('inf')"))
(test "python -inf converts to scheme" -inf.0 (py-eval "float('-inf')"))
(test-assert "python nan converts to scheme" (nan? (py-eval "float('nan')")))
(test "python string converts to scheme" "hello world" (py-eval "'hello ' + 'world'"))
(test "python complex converts to scheme" 1.0+2.0i (py-eval "complex(1.0, 2.0)"))
(test "python tuple converts to scheme vector" #(10 20 30) (py-eval "(10, 20, 30)"))
(test "python list converts to scheme" '(10 20 30) (py-eval "[10, 20, 30]"))

(test "python dict converts to scheme alist"
'(("a" 1) ("b" 2) ("c" 3))
(sort (py-eval "{'a': 1, 'b': 2, 'c': 3}") (lambda (x y) (string<? (car x) (car y)))))

(test-end "python->scheme type conversion")

(test-begin "scheme->python type conversion")
(test-assert "scheme integer converted to python" (repr-compare "12345" 12345))
;; Pick a number that has an exact binary floating point representation.
(test-assert "Scheme float converts to python" (repr-compare "0.25" 0.25))
(test-assert "scheme inf converts to python" (repr-compare "float('inf')" +inf.0))
(test-assert "scheme inf converts to python" (repr-compare "float('-inf')" -inf.0))
(test "scheme nan converts to python" "nan" (repr +nan.0))
(test-assert "scheme string converts to python" (repr-compare "'hello'" "hello"))
(test-assert "scheme complex converts to python" (repr-compare "2.0+4.0j" 2.0+4.0i))
(test-assert "scheme vector converts to python tuple" (repr-compare "(10, 20, 30)" #(10 20 30)))
(test-assert "scheme list converts to python" (repr-compare "[10, 20, 30]" '(10 20 30)))
(test-assert "scheme alist converts to python dict"
  (repr-compare "{'a': 1, 'b': 2, 'c': 3}" '(("a" 1) ("b" 2) ("c" 3))))
(test-end "scheme->python type conversion")

(py-eval  #<<EOP
exec('''
class Foo:
  def bar(self, a1, a2=None, a3=None, a4=None, a5=None):
    return (self, a1, a2, a3, a4, a5)

  def kwo(self, a=None, b=None):
    return (self, a, b)

  def baz(self, x, y):
    return x + y

  def with_rest(self, *args):
    return list(reversed(args))

''')
EOP
)

(define test-object (py-eval "Foo()"))
(define-pymethod "bar" kw: (a2 a3 a4 a5))
(define-pymethod "kwo" kw: (a b))
(define-pymethod "baz")
(define-pymethod "with_rest")
(define py-none (py-eval "None"))

(test-begin "method parameter handling")
(test "rest without keyword arguments" '(5 4 3 2 1) (with_rest test-object 1 2 3 4 5))
(test-assert "rest parameter, no arguments passed" (null? (with_rest test-object)))
(test "fixed arity, correct number of arguments passed"
8 (baz test-object 3 5))
(test-signals-condition "fixed arity, too many arguments passed" 'pyerror
(baz test-object 3 4 5))
(test-signals-condition "fixed arity, too few arguments passed" 'pyerror
(baz test-object 3))
(test-signals-condition "one fixed parameter many keywords, no args passed" 'pyerror
(bar test-object))
(test "1+kwargs method gets no kwargs"
(vector test-object 5 py-none py-none py-none py-none) (bar test-object 5))
(test "1+kwargs method gets one kwarg"
(vector test-object 5 py-none py-none 7 py-none) (bar test-object 5 a4: 7))
(test "kwargs-only method gets no kwargs"
(vector test-object py-none py-none) (kwo test-object))
(test "kwargs-only method gets one kwarg"
(vector test-object py-none 4) (kwo test-object b: 4))
(test-signals-condition "unexpected keyword" 'exn
(kwo test-object surprise: 3))
(test-signals-condition "keyword passed without value" 'exn (kwo test-object a:))
(test-signals-condition "non-keyword passed after keyword" 'exn (kwo test-object a: 7 9))
(test-end "method parameter handling")

(unless (zero? (test-failure-count))
  (print "=====")
  (printf "===== ~a ~a failed!\n"
          (test-failure-count)
          (if (> (test-failure-count) 1) "tests" "test"))
  (print "====="))
(test-exit)
