;;;                                                                    ;
;;;  RSAME.LSP                                                         ;
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
;;; This file contains various reactor utilities to make objects share ;
;;; equal propertise. All modification will be made after              ;
;;; :vlr-modified notification has been received.                      ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-PROPERTIES                              ;
;;;                                                                    ;
;;;    Description:  This function is used to modify two               ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            obj1 = a valid vla object to be used as the source      ;
;;;                   object to get properties from.                   ;
;;;            obj2 = a valid vla object to be used as the target      ;
;;;                   objects to place the properties from obj1.       ;
;;;   property-list = a list of properties to be modified.             ;
;;;                                                                    ;
;;; Returned Value:  A vla object with updated properties.             ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (make-same-properties                               ;
;;;                      obj1                                          ;
;;;                      obj2                                          ;
;;;                      property-list)                                ;
;;;--------------------------------------------------------------------;
(defun make-same-properties
			    (obj1 obj2 property-list / new-value)
  (if (and
	obj2
	(eq 'VLA-OBJECT (type obj2))
	(vlax-write-enabled-p obj2)	; test if object can be modified
	(vlax-read-enabled-p obj1)	; test if object can be read
      )
    (foreach property property-list
      (if
	(and
	  ;;(vlax-property-available-p obj1 property)
	  ;;(vlax-property-available-p obj2 property)
	  (not				; don't modify if equal
	    (equal
	      (setq new-value (vlax-get obj1 property))
	      (vlax-get obj2 property)
	    )
	  )
	)
	 (vlax-put obj2 property new-value)
      )
    )
  )
)
;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-PROPERTIES-LIST                         ;
;;;                                                                    ;
;;;    Description:  This function is used to modify a collection of   ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      make-same-properties                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;         arg-list  = a list of arguments.                           ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (make-same-properties-list                          ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun make-same-properties-list (notifier obj-list property-list)
  (foreach obj obj-list
    (make-same-properties notifier obj property-list)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-RADIUS                                  ;
;;;                                                                    ;
;;;    Description:  This function is used as a call back function to  ;
;;;                  an event. It is responsible in modifying the      ;
;;;                  radius of a circle.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      make-same-properties-list                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the reactor     ;
;;;                   invoked.                                         ;
;;;        reactor  = a valid reactor that triggered the call back.    ;
;;;                   Filled in by the reactor invoked.                ;
;;;       arg-list  = a list of arguments.                             ;
;;;                   Filled in by the reactor invoked.                ;
;;;                                                                    ;
;;; Returned Value:  A vla object.                                     ;
;;;		                                                       ;
;;;          Usage:  Should not be used alone.                         ;
;;;                                                                    ;
;;;                (make-same-radius                                   ;
;;;                      Object-which-is-notifying                     ;
;;;                      Reactor-which-has-been-invoked                ;
;;;                      Some-list )                                   ;
;;;--------------------------------------------------------------------;
(defun make-same-radius	(notifier reactor arg-list)
  (make-same-properties-list
    notifier
    (VLR-Data reactor)
    '("Radius")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-SAME-REACTOR                               ;
;;;                                                                    ;
;;;    Description:  This creates a duplicate modified event for a     ;
;;;                  list of vla-objects.                              ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         obj-list  a valid list of vla objects.                     ;
;;;        reaction = a valid function to invoke as a call back.       ;
;;;                                                                    ;
;;; Returned Value:  A vlr object reactor.                             ;
;;;                  such as:                                          ;
;;;			#<VLR-Object-reactor>                          ;
;;;		                                                       ;
;;;          Usage:  Where ac1 and ac2 are valid vla-object and        ;
;;;                  reaction is a function call back.                 ;
;;;                 (setq r                                            ;
;;;                     (create-same-reactor (list ac1 ac2)            ;
;;;                       'reactor-save-center-color))                 ;
;;;--------------------------------------------------------------------;
;;!! redefined in RUTILS.LSP
(defun create-same-reactor (obj-list reaction)
  (VLR-Object-reactor
    obj-list	;; owners
    obj-list	;; user data - recivers
    (list (cons :vlr-modified reaction))
  )
)

;;EOF