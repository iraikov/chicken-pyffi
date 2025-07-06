
(import (chicken base) (chicken format) (chicken process)
        (chicken process-context) srfi-1 srfi-13 compile-file pkg-config)

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

(define (python-pkg-config-test package)
  (and-let* ((cflags (pkg-config:cflags package))
	     (libs (pkg-config:libs package)))
    (python-test ("<Python.h>" (string-join cflags " " 'infix) (string-join libs " " 'infix)))))

(define cpp+ld-options
  (let ((cflags (get-environment-variable "PYTHON_CFLAGS"))
	(lflags (get-environment-variable "PYTHON_LFLAGS"))
	(libs   (get-environment-variable "PYTHON_LIBS")))
    (if (and cflags lflags libs)
	(python-test ("<Python.h>" cflags (string-append lflags " " libs)))
	(or
	 (any python-pkg-config-test '("python3-embed" "python3" "python"))
	 (python-test ("<Python.h>"
			  "-I/System/Library/Frameworks/Python.framework/Headers"
			  "-framework Python"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.12" "-lpython3.12"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.11" "-lpython3.11"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.10" "-lpython3.10"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.9m" "-lpython3.9m"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.9" "-lpython3.9"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.8m" "-lpython3.8m"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.8" "-lpython3.8"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.7m" "-lpython3.7m"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.7" "-lpython3.7"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.6m" "-lpython3.6m"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.6" "-lpython3.6"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.5m" "-lpython3.5m"))
	    (python-test ("<Python.h>" "-I/usr/include/python3.5" "-lpython3.5"))
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
