;;
;; Example from swriter.py in the Python-UNO distribution..
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

;;
;; Before running the following code, you must make sure OpenOffice is
;; running as a server:
;;
;;  soffice "-accept=socket,host=localhost,port=2002;urp;"
;;

(py-start)

(py-import "uno")
(py-import "unohelper")

;; UNO structs later needed to create a document
(define PARAGRAPH_BREAK 0)

;(py-import "com.sun.star.text.ControlCharacter")
;(py-import "com.sun.star.text.TextContentAnchorType")
;(py-import "com.sun.star.awt")


(define-pyfun "uno.getComponentContext")

(define-pyslot "ServiceManager")
(define-pyslot "Text")
(define-pyslot "Rows")
(define-pyslot "Bool")

(define-pymethod "createInstanceWithContext")
(define-pymethod "createInstance")
(define-pymethod "resolve")
(define-pymethod "getCurrentComponent")
(define-pymethod "getStyleFamilies")
(define-pymethod "loadComponentFromURL")
(define-pymethod "createTextCursor")
(define-pymethod "insertString")
(define-pymethod "insertByName")
(define-pymethod "insertControlCharacter")
(define-pymethod "insertTextContent")
(define-pymethod "getCellByName")
(define-pymethod "setPropertyValue")
(define-pymethod "setString")
(define-pymethod "initialize")
(define-pymethod "getByIndex")
(define-pymethod "getByName")

(define (insertTextIntoCell table cellName text color )
  (let* ((tableText (getCellByName table cellName))
	 (cursor    (createTextCursor tableText)))
    (setPropertyValue cursor "CharColor"  color )
    (setString tableText  text )))

(define-syntax uno-new-session
  (lambda (x r c)
    (let ((%let* (r 'let*)))
    `(,%let* 
         ((lc (uno.getComponentContext))
	  (resolver (createInstanceWithContext 
		     (ServiceManager lc)
		     "com.sun.star.bridge.UnoUrlResolver" lc))
	  (ctx (resolve resolver "uno:socket,host=localhost,port=2002;urp;StarOffice.ComponentContext"))
	  (desktop (createInstanceWithContext 
		    (ServiceManager ctx)
		    "com.sun.star.frame.Desktop" ctx))
	  ;(model   (getCurrentComponent desktop))
	  (model   (loadComponentFromURL  desktop "private:factory/swriter" "_blank"  0  (make-vector 0) ))
	  (text    (Text model))
	  (cursor  (createTextCursor text)))
	 
     . ,(cdr x)))))

(define-syntax uno-current-session
  (lambda (x r c)
    (let ((%let* (r 'let*)))
      `(,%let* 
	((lc (uno.getComponentContext))
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
	. ,(cdr x)))))

(uno-new-session
 (insertString text cursor "The first line in the newly created text document.\n"  0)
 (insertString text cursor "Now we are in the second line.\n" 0)
 (let* (;; create a new paragraph style
	(xStyle     (createInstance model "com.sun.star.style.ParagraphStyle"))
	;; get the style families
	(xFamilies  (getStyleFamilies model))
	;; access the 'ParagraphStyles' Family
	(xFamily    (getByName xFamilies "ParagraphStyles")))
   ;; give the new style a light blue background
   (setPropertyValue xStyle "ParaBackColor" 13421823)
   (insertByName xFamily "Chicken Paragraph Style" xStyle))
 (let* (;; create a new character style
	(xStyle     (createInstance model "com.sun.star.style.CharacterStyle"))
	;; get the style families
	(xFamilies  (getStyleFamilies model))
	;; access the 'CharacterStyles' Family
	(xFamily    (getByName xFamilies "CharacterStyles")))
   ;; give the new style a light blue background
   (setPropertyValue xStyle "CharFontName" "Courier")
   (insertByName xFamily "Chicken Character Style" xStyle))

 (insertControlCharacter text cursor PARAGRAPH_BREAK 0)
 (setPropertyValue cursor "ParaStyleName" "Chicken Paragraph Style")
 (setPropertyValue cursor "CharStyleName" "Chicken Character Style")
 (insertString text cursor "Now we are in the second paragraph.\n" 0)

 (insertControlCharacter text cursor PARAGRAPH_BREAK 0)
 (setPropertyValue cursor "ParaStyleName" "Heading 1")
 (setPropertyValue cursor "CharStyleName" "Default")
 (insertString text cursor "This is a Level 1 title.\n" 0)

 (let* ((xNum (createInstance model "com.sun.star.text.NumberingRules")))
   (insertControlCharacter text cursor PARAGRAPH_BREAK 0)
   (setPropertyValue cursor "ParaStyleName" "Chicken Paragraph Style")
   (setPropertyValue cursor "CharStyleName" "Chicken Character Style")
   (setPropertyValue cursor "NumberingRules" xNum)
   (insertString text cursor "This is a numbered paragraph.\n" 0)
   (insertString text cursor "This is line two in the numbered paragraph.\n" 0)
   )

 (insertControlCharacter text cursor PARAGRAPH_BREAK 0)

 ;; create a text table
 (let ((table (createInstance model "com.sun.star.text.TextTable")))
   ;; initialize 4x4 table 
   (initialize table 4 4)
   (insertTextContent text cursor table 0 )
   (let ((rows (Rows table))
	 (textColor 0))
     (setPropertyValue table "BackTransparent" #f)
     (setPropertyValue table "BackColor" 13421823 )
     (let ((row (getByIndex rows 0)))
       (setPropertyValue row "BackTransparent" #f)
       (setPropertyValue row "BackColor" 6710932))
     (insertTextIntoCell table "A1" "FirstColumn" textColor )
     (insertTextIntoCell table "B1" "SecondColumn" textColor )
     (insertTextIntoCell table "C1" "ThirdColumn" textColor )
     (insertTextIntoCell table "D1" "SUM" textColor ))))

