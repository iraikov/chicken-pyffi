;;
;; Python-UNO example from pyffi.lisp by Dmitri Hrapof.
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

(require-extension pyffi)

;;
;; Before running the following code, you must make sure OpenOffice is
;; running as a server:
;;
;;  soffice "-accept=socket,host=localhost,port=2002;urp;"
;;

(py-start)

(py-import "uno")

(define-pyfun "uno.getComponentContext")

(define-pyslot "ServiceManager")
(define-pyslot "Text")

;; Can't yet find out appropriate class, the following is lame
(define-pymethod "createInstanceWithContext")
(define-pymethod "resolve")
(define-pymethod "getCurrentComponent")
(define-pymethod "createTextCursor")
(define-pymethod "insertString")

(define (message-uno str)
  (let* ((lc (uno.getComponentContext))
	 (resolver (createInstanceWithContext 
		    (ServiceManager lc)
		    "com.sun.star.bridge.UnoUrlResolver" lc))
	 (ctx (resolve resolver "uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext"))
	 (desktop (createInstanceWithContext 
		   (ServiceManager ctx)
		   "com.sun.star.frame.Desktop" ctx))
	 (model   (getCurrentComponent desktop))
	 (text    (Text model))
	 (cursor  (createTextCursor text)))
    (insertString text cursor str 0)))

(message-uno "Hello, world!")
