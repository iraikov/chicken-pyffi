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
