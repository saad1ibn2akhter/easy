;;;                                                                    ;
;;;  RTRANS.LSP                                                        ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999    ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;; General Note:  THIS FILE IS A MEMBER OF THE RCTR-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;; This file contains various reactor functions.                      ;
;;;--------------------------------------------------------------------;
;;;
;;; translation reactor utilities
;;;
;;;--------------------------------------------------------------------;
;;;       Function:  SAVE-PROPERTY                                     ;
;;;                                                                    ;
;;;    Description:  This function saves property in object extension  ;
;;;                  dictionary the property name is saved in          ;
;;;                  reactor's data                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-obj = a valid vla object.                              ;
;;;        property = a string that denotes the item's property to     ;
;;;                   save.                                            ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(save-property                                         ;
;;;			  vla-Object1                                  ;
;;;			         "Center")                             ;
;;;--------------------------------------------------------------------;
(defun save-property (vla-obj property)
  (if (and (eq 'VLA-OBJECT (type vla-obj))
	   (vlax-read-enabled-p vla-obj)
	   ;;(vlax-property-available-p vla-obj property)
      )
    (vlax-ldata-put
      vla-obj
      property
      (vlax-get vla-obj property)
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  SAVE-CENTER-REACTOR                               ;
;;;                                                                    ;
;;;    Description:  This function saves the "Center" property         ;
;;;                  of a vla object.                                  ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      save-property                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;                  such as:                                          ;
;;;                                                                    ;
;;;          Usage:  Should not be used alone and is intended to be    ;
;;;                  be used within a reactor call back event.         ;
;;;                (save-center-reactor                                ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      PropertyString)                               ;
;;;--------------------------------------------------------------------;
(defun save-center-reactor (notifier reactor arg-list)
  (save-property notifier "Center")
)

;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-CENTER-REACTION                         ;
;;;                                                                    ;
;;;    Description:  This function translates the "Center" property    ;
;;;                  of a vla object.                                  ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                        translate-vla-object                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;		(translate-center-reaction                             ;
;;;			  notifier                                     ;
;;;			         arg-list)                             ;
;;;--------------------------------------------------------------------;
(defun translate-center-reaction
       (notifier reactor arg-list / property from to)
  (if (vlax-read-enabled-p notifier)
    (progn
      (setq from (vlax-ldata-get notifier "Center")
	    to	 (vlax-get notifier "Center")
      )
      (if
	(not (equal from to))
	 (foreach obj (vlr-data reactor)
	   (translate-vla-object obj (subtract-vector to from))
	 )
      )
    )
  )
)

;;; geometry utils
;;;--------------------------------------------------------------------;
;;;       Function:  ADD-VECTOR                                        ;
;;;                                                                    ;
;;;    Description:  This function returns the addition of             ;
;;;                  two vectors.                                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               v1   =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;               v2   =  a valid vector list such as:                 ;
;;;                       '( 2 2 2 )                                   ;
;;;                                                                    ;
;;; Returned Value:  A vector list with the subtraction performed      ;
;;;                  from v1 and v2.                                   ;
;;;			(add-vector '(5 5 5 ) '(2 2 2))                ;
;;; 					Returns:                       ;
;;;					(7 7 7)                        ;
;;;		                                                       ;
;;;          Usage: (add-vector '(5 5 5 ) '(2 2 2 ))                   ;
;;;--------------------------------------------------------------------;
(defun add-vector (v1 v2)
  (vlax-3d-point (mapcar '+ v1 v2))
)

;;;--------------------------------------------------------------------;
;;;       Function:  SUBTRACT-VECTOR                                   ;
;;;                                                                    ;
;;;    Description:  This function returns the subtraction of two      ;
;;;                  vectors.                                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               v1   =  a valid vector list such as:                 ;
;;;                       '( 5 5 5 )                                   ;
;;;               v2   =  a valid vector list such as:                 ;
;;;                       '( 1 1 1 )                                   ;
;;;                                                                    ;
;;; Returned Value:  A vector list with the subtraction performed      ;
;;;                  from v1 and v2.                                   ;
;;;			(subtract-vector '(5 5 5 ) '(1 1 1))           ;
;;; 					Returns:                       ;
;;;					(4 4 4)                        ;
;;;		                                                       ;
;;;          Usage: (subtract-vector '(5 5 5 ) '(1 1 1))               ;
;;;--------------------------------------------------------------------;
(defun subtract-vector (v1 v2)
  (vlax-3d-point (mapcar '- v1 v2))
)

;;; matrix operations
;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-TRANSLATION-MATRIX                           ;
;;;                                                                    ;
;;;    Description:  This function converts a variant vector list      ;
;;;                  (a list of three numbers) into a vector matrix.   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                                                                    ;
;;;                  Example:  A vector list '( 5 5 5 ) is passed to   ;
;;;                  make-translation-matrix. The function then        ;
;;;                  translates this value to a matrix list.           ;
;;;                  using the following logic.                        ;
;;;                                                                    ;
;;;			make a translation matrix from                 ;
;;;			1,2 or 3 dim vector v represented as:          ;
;;;			 	list (x), (x y) or (x y z)             ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vector =  a valid vector list such as:                     ;
;;;                   '( 5 5 5) or '( 1.2 4.5 200.00)                  ;
;;;      or vector =  a valid safearray variant vector list of doubles ;
;;;                                                                    ;
;;; Returned Value:  A matrix List such as:                            ;
;;;		      (make-translation-matrix '( 5 5 5 ))             ;
;;;		                                                       ;
;;;		            Returns List In As A Variant Array:        ;
;;;				((1.0 0.0 0.0 5.0)                     ;
;;;				  (0.0 1.0 0.0 5.0)                    ;
;;;				  (0.0 0.0 1.0 5.0)                    ;
;;;				  (0.0 0.0 0.0 1.0)                    ;
;;;				)                                      ;
;;;		                                                       ;
;;;       Usage: (make-translation-matrix '( 5 5 5 ))   or             ;
;;;              (make-translation-matrix (vlax-3d-point '( 5 5 5 )))  ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun make-translation-matrix (vector / tm TransDataA TransData)

  (if (> (vlax-variant-type vector) 8192)
    (setq vector (vlax-safearray->list (vlax-variant-value vector)))
  )
  (setq tm 
    (list (list 1 0 0 (car vector))
	  (list 0 1 0 (cadr vector))
	  (list 0 0 1 (caddr vector))
	  '(0 0 0 1)
    )
  )
;; Convert to a Variant Array of Doubles here ->
 (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 3) (cons 0 3)))
 (vlax-safearray-fill TransDataA tm)
 (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))
  )

;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-VLA-OBJECT                              ;
;;;                                                                    ;
;;;    Description:  This function translates the current              ;
;;;                  transformation values of an object by a supplied  ;
;;;                  vector list.  This vector list is a list of three ;
;;;                  numbers which determine the new values for the    ;
;;;                  existing transformation value.                    ;
;;;                  Translate-Vla-Object is similar to                ;
;;;                  translate-object except this function performs    ;
;;;                  error checking before passing the information     ;
;;;                  to translate-object.                              ;
;;;                                                                    ;
;;;                  Note: This function performs                      ;
;;;                        error checking.                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      translate-object                              ;
;;;                                                                    ;
;;;                  Example:  A line beginning is anchored at 0,0,0.  ;
;;;                  Its ending point is 1,0,0. The transformation     ;
;;;                  value is '(5 5 5). Hence add 5 to the X value, 5  ;
;;;                  to the Y value and 5 to the Z value. The result   ;
;;;                  will be:                                          ;
;;;                       The beginning point will have 5,5,5          ;
;;;                       The ending point will have 6,5,5             ;
;;;                                                                    ;
;;;                  The example above demonstrates a different method ;
;;;                  for moving an object.                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           vla-obj  =  a vla object that can contain                ;
;;;                       transformation verctors.                     ;
;;; translation-vector =  a valid vector list such as:                 ;
;;;                       '( 5 5 5) or '( 1.2 4.5 200.00)              ;
;;;                                                                    ;
;;;                                                                    ;
;;; Returned Value:  A vla object                                      ;
;;;                                                                    ;
;;;          Usage: (translate-vla-object vla-Object '( 5 5 5))        ;
;;;--------------------------------------------------------------------;
(defun translate-vla-object (vla-obj translation-vector)
  (if (and
	vla-obj
	(eq 'VLA-OBJECT (type vla-obj))
	(vlax-write-enabled-p vla-obj)	; test if object can be modified
      )
    (translate-object vla-obj translation-vector)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-OBJECT                                  ;
;;;                                                                    ;
;;;    Description:  This function translates the current              ;
;;;                  transformation values of an object by a supplied  ;
;;;                  vector list.  This vector list is a list of three ;
;;;                  numbers which determine the new values for the    ;
;;;                  existing transformation value.                    ;
;;;                  Translate-Object is similar to                    ;
;;;                  translate-vla-object except this function DOES    ;
;;;                  NOT perform error checking before passing the     ;
;;;                  information to make-translation-matrix.           ;
;;;                                                                    ;
;;;                  Note: This function DOES NOT performs             ;
;;;                        error checking.                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      translate-object                              ;
;;;                                                                    ;
;;;                  Example:  A line beginning is anchored at 0,0,0.  ;
;;;                  Its ending point is 1,0,0. The transformation     ;
;;;                  value is '(5 5 5). Hence add 5 to the X value, 5  ;
;;;                  to the Y value and 5 to the Z value. The result   ;
;;;                  will be:                                          ;
;;;                       The beginning point will have 5,5,5          ;
;;;                       The ending point will have 6,5,5             ;
;;;                                                                    ;
;;;                  The example above demonstrates a different method ;
;;;                  for moving an object.                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           vla-obj  =  a vla object that can contain                ;
;;;                       transformation vectors.                      ;
;;; translation-vector =  a valid vector list such as:                 ;
;;;                       '( 5 5 5) or '( 1.2 4.5 200.00)              ;
;;;                                                                    ;
;;;                                                                    ;
;;; Returned Value:  A vla object                                      ;
;;;                                                                    ;
;;;          Usage: (translate-object vla-Object '( 5 5 5))            ;
;;;--------------------------------------------------------------------;
(defun translate-object	(obj translation-vector)
  (vla-TransformBy
    obj
    (make-translation-matrix translation-vector)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-TRANSLATE-CURVE-REACTOR                    ;
;;;                                                                    ;
;;;    Description:  This function creates a curve reactor.            ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      save-center-reactor                           ;
;;;                      translate-center-reaction                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;   circles-list  = a list of valid vla circles.                     ;
;;;                   reactor.                                         ;
;;;          curve  = a list of valid vla objects which will           ;
;;;                   receive notification.                            ;
;;;                                                                    ;
;;; Returned Value:  A vlr reactor object.                             ;
;;;                                                                    ;
;;;          Usage:  Should not be used alone and is intended to be    ;
;;;                  be used within a reactor call back event.         ;
;;;                (save-center-reactor                                ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      PropertyString)                               ;
;;;--------------------------------------------------------------------;
(defun create-translate-curve-reactor (circles-list curve)
  (VLR-Object-reactor
    circles-list			;;owners
    (list curve)			;;recievers
    (list (cons :vlr-objectClosed (function save-center-reactor))
	  (cons :vlr-modified (function translate-center-reaction))
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-RADIUS                                        ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a radius from  ;
;;;                  a known point. User input is curtailed via a call ;
;;;                  to initget whose sum of the bit values determine  ;
;;;                  the behavior of this function.                    ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;                2           Prevents the user from responding       ;
;;;                            to the request by entering zero.        ;
;;;                                                                    ;
;;;                4           Prevents the user from responding       ;
;;;                            to the request by entering a            ;
;;;                            negative value.                         ;
;;;                                                                    ;
;;;                32          Uses dashed lines when drawing          ;
;;;                            rubber-band line or box. For those      ;
;;;                            functions with which the user can       ;
;;;                            specify a point by selecting a          ;
;;;                            location on the graphics screen,        ;
;;;                            this bit value causes the               ;
;;;                            rubber-band line or box to be           ;
;;;                            dashed instead of solid.                ;
;;;                            (Some display drivers use a             ;
;;;                            distinctive color instead of            ;
;;;                            dashed lines.)                          ;
;;;                            If the system variable POPUPS           ;
;;;                            is 0, AutoCAD ignores this bit.         ;
;;;                                                                    ;
;;;                64          Prohibits input of a Z                  ;
;;;                            coordinate to the getdist               ;
;;;                            function; lets an application           ;
;;;                            ensure that this function returns       ;
;;;                            a 2D distance.                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           point = a list of three reals that denotes where the     ;
;;;                   rubber-banding visual aid will commence.         ;
;;;                                                                    ;
;;; Returned Value:  a real number denoting a distance                 ;
;;;                                                                    ;
;;;          Usage: (get-radius '(0 0 0 ))                             ;
;;;--------------------------------------------------------------------;
(defun get-radius (point)
;| see above for the bit values used = (+ 1 2 4 32 64) |;

(if (eq (type point) 'VARIANT)
   (if (> (vlax-variant-type point) 8192)
    (setq point (vlax-safearray->list (vlax-variant-value point)))
  )
)
  (initget 103)
  (getdist point "\nSelect radius: ")
)

;;EOF
