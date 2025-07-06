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

(test-begin "keyword stress tests")

;; Test multiple keywords in different orders
(test "multiple keywords in original order"
  (vector test-object 5 2 3 4 py-none) 
  (bar test-object 5 a2: 2 a3: 3 a4: 4))

(test "multiple keywords in reverse order"
  (vector test-object 5 py-none 3 4 5) 
  (bar test-object 5 a5: 5 a4: 4 a3: 3))

(test "multiple keywords in mixed order"
  (vector test-object 5 py-none py-none 7 9) 
  (bar test-object 5 a5: 9 a4: 7))

;; Test all keywords provided
(test "all keywords provided"
  (vector test-object 5 10 20 30 40) 
  (bar test-object 5 a2: 10 a3: 20 a4: 30 a5: 40))

;; Test single keywords (different ones)
(test "only a2 keyword"
  (vector test-object 5 100 py-none py-none py-none) 
  (bar test-object 5 a2: 100))

(test "only a3 keyword"
  (vector test-object 5 py-none 200 py-none py-none) 
  (bar test-object 5 a3: 200))

(test "only a5 keyword"
  (vector test-object 5 py-none py-none py-none 500) 
  (bar test-object 5 a5: 500))

;; Test kwargs-only method with multiple keywords
(test "kwargs-only method with both keywords"
  (vector test-object 11 22) 
  (kwo test-object a: 11 b: 22))

(test "kwargs-only method with keywords in reverse order"
  (vector test-object 33 44) 
  (kwo test-object b: 44 a: 33))

;; Test edge cases with keyword validation
(test-signals-condition "invalid keyword for bar method" 'exn
  (bar test-object 5 invalid: 123))

(test-signals-condition "valid keyword for wrong method" 'exn
  (kwo test-object a4: 7))  ; a4 is valid for bar but not kwo

(test-signals-condition "multiple invalid keywords" 'exn
  (bar test-object 5 invalid1: 1 invalid2: 2))

;; Test keyword argument edge cases  
(test-signals-condition "keyword without value" 'exn
  (bar test-object 5 a4:))

(test-signals-condition "positional arg after keyword" 'exn
  (bar test-object 5 a4: 7 extra-positional))

;; TODO: test that a missing kw arg value raises an exception; causes a segfault in the Github CI environment
;(test-signals-condition "missing value of keyword arg" 'exn
;  (kwo test-object a: 1 b:))

;; TODO: test duplicate keywords; should be caught by Scheme parser, but it goes through on 5.4.0
;(test-signals-condition "duplicate keyword arguments" 'exn
;  (bar test-object 5 a4: 7 a4: 8))

;; Test complex values as keyword arguments
(test "complex values as keyword arguments"
  (vector test-object 5 py-none '(1 2 3) "hello" 3.14) 
  (bar test-object 5 a3: '(1 2 3) a4: "hello" a5: 3.14))

;; Test with python objects as keyword values
(define py-list-obj (py-eval "[1, 2, 3]"))
(test "python objects as keyword values"
  (vector test-object 5 py-none py-list-obj py-none py-none) 
  (bar test-object 5 a3: py-list-obj))

;; Test zero arguments to kwargs-only method
(test "kwargs-only method with no arguments"
  (vector test-object py-none py-none) 
  (kwo test-object))

;; Test method without keyword support still works
(test "method without keywords works normally"
  8 (baz test-object 3 5))

(test-signals-condition "method without keywords rejects keywords" 'exn
  (baz test-object 3 5 extra: 7))

;; Test rest arguments still work
(test "rest arguments without keywords"
  '(5 4 3 2 1) 
  (with_rest test-object 1 2 3 4 5))

(test-signals-condition "rest method rejects keywords" 'exn
  (with_rest test-object 1 2 3 keyword: 4))

;; Many keywords test
(define-pymethod "many_kw_test" kw: (k1 k2 k3 k4 k5 k6 k7 k8 k9 k10))

;; define this method in Python first
(py-eval #<<EOP
exec('''
def many_kw_test(self, k1=None, k2=None, k3=None, k4=None, k5=None, 
                 k6=None, k7=None, k8=None, k9=None, k10=None):
    return (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10)

Foo.many_kw_test = many_kw_test
''')
EOP
)

(test "many keywords - all"
  (vector 1 2 3 4 5 6 7 8 9 10)
  (many_kw_test test-object k1: 1 k2: 2 k3: 3 k4: 4 k5: 5 
                k6: 6 k7: 7 k8: 8 k9: 9 k10: 10))

(test "many keywords - some"
  (vector 1 py-none py-none 4 py-none 6 py-none py-none 9 py-none)
  (many_kw_test test-object k1: 1 k4: 4 k6: 6 k9: 9))

(test "many keywords - reverse order"
  (vector py-none py-none py-none py-none py-none py-none py-none py-none py-none 42)
  (many_kw_test test-object k10: 42))

(test-end "keyword stress tests")

(unless (zero? (test-failure-count))
  (print "=====")
  (printf "===== ~a ~a failed!\n"
          (test-failure-count)
          (if (> (test-failure-count) 1) "tests" "test"))
  (print "====="))
(test-exit)
