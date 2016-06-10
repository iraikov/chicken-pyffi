(use pyffi)

(py-start)
(py-import "sys")
(assert (string? (py-eval "sys.version")))

