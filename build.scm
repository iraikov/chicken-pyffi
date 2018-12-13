
(import (chicken base) (chicken format) (chicken process)
        (chicken process-context) srfi-13 compile-file)

(define args (command-line-arguments))

(define (python-try-compile header cppflags ldflags)
  (and (try-compile 
	(string-append "#include " header "\n" 
                       "int main() { Py_Initialize(); return 0; }\n")
	ldflags: ldflags
	cflags: cppflags
        verbose: #t)
       (cons cppflags ldflags)))

(define-syntax python-test 
  (syntax-rules ()
    ((_ (flags ...))
     (condition-case (python-try-compile flags ...)
		     (t ()    #f)))))

(define cpp+ld-options
  (let ((cflags (get-environment-variable "PYTHON_CFLAGS"))
	(lflags (get-environment-variable "PYTHON_LFLAGS"))
	(libs   (get-environment-variable "PYTHON_LIBS")))
    (if (and cflags lflags libs)
	(python-test ("<Python.h>" cflags (string-append lflags " " libs)))
	(or (python-test ("<Python.h>"
                          "-I/System/Library/Frameworks/Python.framework/Headers"
                          "-framework Python"))
	    (python-test ("<Python.h>" "-I/usr/include/python2.7" "-lpython2.7"))
	    (python-test ("<Python.h>" "-I/usr/include/python2.6" "-lpython2.6"))
	    (python-test ("<Python.h>" "" "-lpython27"))
	    (python-test ("<Python.h>" "" "-lpython26"))
	    (python-test ("<Python.h>" "-I/usr/include/python" "-lpython"))
	    (error "unable to figure out location of Python")))
    ))

(define c-options  (car cpp+ld-options))
(define ld-options (cdr cpp+ld-options))

(define cmd (intersperse
             (append args (list (sprintf "-L \"~A\"" ld-options)
                                (sprintf "-C \"~A\"" c-options)))
             " "))
(system (string-concatenate cmd))
