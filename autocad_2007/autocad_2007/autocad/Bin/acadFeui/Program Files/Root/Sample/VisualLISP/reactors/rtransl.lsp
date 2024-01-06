;;;                                                                    ;
;;;  RTRANSL.LSP                                                       ;
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
;;; This file contains various reactor utilities                       ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-TRANSLATE-CENTER                          ;
;;;                                                                    ;
;;;    Description:  This function is used within a :vlr-modified      ;
;;;                  reactor event.                                    ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     subtract-vector                                ;
;;;                     translate-vla-circle                           ;
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
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;     (reactor-translate-center notifier reactor arg-list)           ;
;;;--------------------------------------------------------------------;
(defun reactor-translate-center
				(notifier   reactor    arg-list
				 /	    property   from
				 to	    tr-vect    transl-obj
				)
  (if (vlax-read-enabled-p notifier)
    (progn
      (setq from    (vlax-ldata-get notifier "Center")
	    to	    (vlax-get notifier "Center")
	    tr-vect (subtract-vector to from)
      )
      (if
	(and from
	     (not (equal from to))
	)
	 (foreach obj (vlr-data reactor)
	   (translate-vla-circle obj tr-vect)
	 )
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  TRANSLATE-VLA-CIRCLE                              ;
;;;                                                                    ;
;;;    Description:  This function is used as a move method for an     ;
;;;                  object.                                           ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     add-vector                                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             obj = a valid vla object.                              ;
;;;     translation = a valid translation list as returned from        ;
;;;                   add-vector. Such as (7 7 7)                      ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object.                               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (translate-vla-circle obj '( 7 7 7 )             ;
;;;--------------------------------------------------------------------;
(defun translate-vla-circle (obj translation / old-center new-center)
  (if (and
	obj
	(eq 'VLA-OBJECT (type obj))
	(vlax-write-enabled-p obj) ;; test if object can be modified
      )
    (progn
      (setq old-center (vla-get-center obj)
	    new-center (add-vector old-center translation)
      )
      (vlax-ldata-put obj "Center" new-center)
      ;; It is important to store new-center before the object is moved.
      ;; Because after moving, this object will fire notifications to   
      ;; its associated objects. Note: updating the new center position 
      ;; property will not move other circles. Only the translation of  
      ;; the first object will cause translations of all other objects. 
      (vla-move obj old-center (vlax-3d-point new-center))
    )
  )
)


;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-TRANSLATE-REACTOR                          ;
;;;                                                                    ;
;;;    Description:  This function is used as a reactor constructor.   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     reactor-save-center                            ;
;;;                     reactor-translate-center                       ;
;;;                     save-object-properties                         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         cla-lst = a list of vla circle objects.                    ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (create-translate-reactor cla-lst )              ;
;;;--------------------------------------------------------------------;
(defun create-translate-reactor	(cla-lst / reactor)
  (function reactor-save-center)
  (function reactor-translate-center)
  (foreach obj cla-lst
    (save-object-properties obj '("Center"))
  )
  (setq	reactor
	 (VLR-Object-reactor
	   cla-lst		;; owners
	   cla-lst		;; recievers
	   (list
	     (cons :vlr-objectClosed 'reactor-save-center)
	     (cons :vlr-modified 'reactor-translate-center)
	   )
	 )
  )
  reactor
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-SQUARE-CL                                    ;
;;;                                                                    ;
;;;    Description:  This function creates 4 circles.                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;     start-point = a valid 3d point   (list of three reals)         ;
;;;             rad = a real number.                                   ;
;;;                                                                    ;
;;; Returned Value:  A valid list of vla circle objects.               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-square-cl start-point rad)                 ;
;;;--------------------------------------------------------------------;
(defun make-square-cl
       (start-point rad / acadApp acadDoc acadModel ac-lst pi2 i)
  (setq acadApp   (vlax-get-acad-object)
        acadDoc   (vla-get-ActiveDocument acadApp)
        acadModel (vla-get-ModelSpace acadDoc)
        pi2       (/ pi 2.0)
        i         0
  )
  (while (< i 4)
    (setq ac-lst      (cons (vla-AddCircle acadModel (vlax-3d-point start-point) rad) ac-lst)
          i           (1+ i)
          start-point (polar start-point (* pi2 i) rad)
    )
  )
  ac-lst
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:RTRANSL-TST                                     ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for the            ;
;;;                  position of the first circle and creates          ;
;;;                  3 additional circles. The circle centers          ;
;;;                  will form a square.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-translate-reactor                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:RTRANSL-TST) or RTRANSL-TST from                ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:RTRANSL-TST (/ pnt rad)
  (initget 1)
  (setq pnt (getpoint "\nSelect center of the first circle: "))
  (initget 103)	  ;; (+ 1 2 4 32 64)
  (setq rad (getdist pnt "\n Select radius: "))
  (create-translate-reactor (make-square-cl pnt rad))
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:RTRANSL-INFO                                    ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:RTRANSL-INFO) or RTRANSL-INFO from              ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:RTRANSL-INFO ()
  (princ
    "\nFour circles will be created. These circles will be moved together"
  )
  (princ "\nif you move one of them.")
  (princ
    "\n(You will be asked to select center and radius of the circle.)"
  )
  (princ "\nFor test call RTRANSL-TST command")
  (terpri)
  (princ)
)


;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "RTRANSL-TST" "RTRANSL-INFO")
	     *REACT-TEST-COMMANDS-INFO*
       )
)

;;EOF
