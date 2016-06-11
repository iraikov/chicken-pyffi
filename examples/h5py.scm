;;
;; Example from Python h5py documentation.
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

(import chicken)
(require-extension pyffi)

(py-start)

(py-import "h5py")
(py-import "numpy")


(define-pyfun "h5py.File" name mode)
(define-pymethod "close") ;; File
(define-pymethod "create_dataset" kw: (dtype)) ;; File

(define-pyslot "shape")
(define-pyslot "dtype")
(define-pyslot "name")

(define f (h5py.File "mytestfile.hdf5" "w"))

(define dset (create_dataset f "mydataset"  (vector 100) dtype: "i"))

(print (py-object-from (name dset)))
(print (py-object-from (shape dset)))
(print (py-object-type (dtype dset)))

(close f)
