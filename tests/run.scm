(import pyffi)

(py-start)
(py-import "sys")
(assert (string? (py-eval "sys.version")))
(print "Python version: " (py-eval "sys.version"))
