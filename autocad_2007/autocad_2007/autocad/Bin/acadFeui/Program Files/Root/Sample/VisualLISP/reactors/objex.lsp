;;;                                                                    ;
;;;  OBJEX.LSP                                                         ;
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
;;; General Note:  THIS FILE IS A MEMBER OF THE REAC-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;;     This file contains a reactor test.                             ;
;;;	In this test three circles of the same radius                  ;
;;;	will be created and will have different colors                 ;
;;;	Try to change one circle and the others will                   ;
;;;	change their radius.                                           ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  PREPARE-FOR-OBJEX-TST                             ;
;;;                                                                    ;
;;;    Description:  This function makes 3 circles with equal radius   ;
;;;                  in model space.                                   ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (prepare-for-OBJEX-TST)                          ;
;;; Note: This function creates the global variables:                  ;
;;;       [ ac1 ac2 ac3 acadapp acaddoc acadmodel ]                    ;
;;;       They are allowed to be global for the                        ;
;;;       perusal of their values.                                     ;
;;;--------------------------------------------------------------------;
(defun prepare-for-OBJEX-TST ()
  (setq acadApp (vlax-get-acad-object))
  (setq acadDoc (vla-get-ActiveDocument acadApp))
  (setq acadModel (vla-get-ModelSpace acadDoc))

;;; Create 3 cirles
  (setq ac1 (vla-AddCircle acadModel (vlax-3d-point '(10.0 10.0 0.0)) 10.0))
  (vla-put-Color ac1 1)
  (setq ac2 (vla-AddCircle acadModel (vlax-3d-point '(5.0 5.0 0.0)) 5.0))
  (vla-put-Color ac2 2)
  (setq ac3 (vla-AddCircle acadModel (vlax-3d-point '(3.0 3.0 0.0)) 3.0))
  (vla-put-Color ac3 3)

  (command "_.ZOOM" "_EXTENTS")
  (command "_regen")
)

;;; define some helpers
;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-THE-SAME-RADIUS                              ;
;;;                                                                    ;
;;;    Description:  This function makes the 3 circles have the same   ;
;;;                  radius.                                           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            obj1 = a valid vla circle object.                       ;
;;;            obj2 = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:   a valid vla circle object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-the-same-radius                            ;
;;;                           vla-circle-object1                       ;
;;;                           vla-circle-object2                       ;
;;;                  )                                                 ;
;;;--------------------------------------------------------------------;
(defun make-the-same-radius (obj1 obj2)
  (if (and
	obj2
	(vlax-write-enabled-p obj2)	;; test if object can be modified
	(vlax-read-enabled-p obj1)	;; test if object can be read
      )
    (VLA-PUT-RADIUS obj2 (vla-get-radius obj1))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SAME-RADIUS-REACTION                         ;
;;;                                                                    ;
;;;    Description:  This function is a call back function invoked     ;
;;;                  from a reactor.                                   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     make-the-same-radius                           ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-same-radius-reaction                       ;
;;;                           notifier                                 ;
;;;                           reactor                                  ;
;;;                           arg-list                                 ;
;;;                  )                                                 ;
;;;--------------------------------------------------------------------;
(defun make-same-radius-reaction (notifier reactor arg-list)
  (make-the-same-radius notifier (VLR-Data reactor))
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:OBJEX-TST                                       ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object which will contain reactors for:           ;
;;;                  COPY, MIRROR or ARRAY                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-same-copy-reactor                        ;
;;;                    add-circle                                      ;
;;;                    reactor-make-same-radius-color                  ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:OBJEX-TST) or OBJEX-TST from                    ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:OBJEX-TST ()
  (prepare-for-OBJEX-TST)
  (setq	r1
	 (VLR-Object-reactor
	   (list ac1)
	   ac2
	   '((:vlr-modified . make-same-radius-reaction))
	 )
  )
  (setq	r2
	 (VLR-Object-reactor
	   (list ac2)
	   ac3
	   '((:vlr-modified . make-same-radius-reaction))
	 )
  )
  (setq	r3
	 (VLR-Object-reactor
	   (list ac3)
	   ac1
	   '((:vlr-modified . make-same-radius-reaction))
	 )
  )
  (vla-put-Color ac1 acRed)  ;touch of the circles to fire reactors the first time
)


;;;;;;; now take a look at ACAD console
;;;;;;; change radius of the ac1 (red one) 
;;;; both ac2 and ac3 will change there radii


;;To remove reactor
;;(vlr-remove r1)
;;(vlr-remove r2)
;;(vlr-remove r3)

;;;;; try ACAD again
;;; Now All circles radii independent again
;;; You can restore contraints by adding reactors back
;;(vlr-add r1)
;;(vlr-add r2)
;;(vlr-add r3)



;;;;;;;;;;;;;;;;;;;;; make them persistent!
;;(vlr-pers r1)
;;(vlr-pers r2)
;;(vlr-pers r3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; to trace all events coming to circles
;|
(setq rr
       (VLR-Object-reactor
	 (list ac1 ac2 ac3)
	 nil
	 nil
	 ))

(defun vlr-full-trace (reactor)
  (foreach name (VLR-Reaction-Names reactor)
    (VLR-Reaction-Set reactor name 'VLR-trace-reaction)))

(vlr-full-trace rr)
|;

;;;--------------------------------------------------------------------;
;;;       Function:  C:OBJEX-INFO                                      ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:OBJEX-INFO) or OBJEX-INFO from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:OBJEX-INFO ()
  (textscr)
  (princ "\nIn this test three circles of the same radius")
  (princ "\nwill be created and will have different colors ")
  (princ "\nTry to change one circle and the others will ")
  (princ "\nchange their radius.")

)



;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "OBJEX-TST" "OBJEX-INFO")
	     *REACT-TEST-COMMANDS-INFO*))
;;EOF