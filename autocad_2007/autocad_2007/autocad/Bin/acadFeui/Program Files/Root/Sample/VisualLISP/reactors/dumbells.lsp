;;;                                                                    ;
;;;  DUMBELLS.LSP                                                      ;
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
;;;     This file contains an example that enables the                 ;
;;;	creation of two red circles with the same radius               ;
;;;	with a red line connecting their centers. If you               ;
;;;	change the radius of a circle the adjacent circle              ;
;;;	will match the radius of the modified circle. If you           ;
;;;	move the position of a line end or the line itself,            ;
;;;	the circles will adjust to the end points of the line.         ;
;;;	Run the DUMBELLS-TST command to see this test.                 ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  _REACTOR-MAKE-ASSOCIATION-PROPERTIES              ;
;;;                                                                    ;
;;;    Description:  This function makes an association between        ;
;;;                  object1 and object2 with the supplied             ;
;;;                  association data.                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             obj1 = a valid vla object.                             ;
;;;             obj2 = a valid vla object.                             ;
;;; association-list = an association list.                            ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object.                               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (_reactor-make-association-properties                          ;
;;;           vla-Object1                                              ;
;;;           vla-Object1                                              ;
;;;           properties-List )                                        ;
;;;--------------------------------------------------------------------;
(defun _reactor-make-association-properties
					    (obj1
					     obj2
					     association-list
					     /
					     new-value
					     property-from
					     property-to
					    )
  (if (and
	(eq 'VLA-OBJECT (type obj2))
	(vlax-write-enabled-p obj2)	;; test if object can be modified
	(vlax-read-enabled-p obj1)	;; test if object can be read
      )
    (foreach association association-list
      (if
	(and
	  (setq property-from (vlax-ldata-get obj1 association))
	  (setq property-to (vlax-ldata-get obj2 association))
	  (vlax-property-available-p obj1 property-from)
	  (vlax-property-available-p obj2 property-to)
	  (not				;; don't modify if equal
	    (equal
	      (setq new-value (vlax-get obj1 property-from))
	      (vlax-get obj2 property-to)
	    )
	  )
	)
	 (vlax-put obj2 property-to new-value)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  _REACTOR-MAKE-ASSOCIATION-PROPERTIES-LIST         ;
;;;                                                                    ;
;;;    Description:  This function makes an association list using     ;
;;;                  _reactor-make-association-properties              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-association-properties          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         notifier = a valid vla object.                             ;
;;;         obj-list = a list of vla object.                           ;
;;;    property-list = an association list.                            ;
;;;                                                                    ;
;;; Returned Value:  The last modified vla object.                     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (_reactor-make-association-properties-list                     ;
;;;           notifier                                                 ;
;;;           obj-list                                                 ;
;;;            '("start-association"                                   ;
;;;              "end-association"                                     ;
;;;               "Radius-association") )                              ;
;;;--------------------------------------------------------------------;
(defun _reactor-make-association-properties-list
       (notifier obj-list property-list)
  (foreach obj obj-list
    (_reactor-make-association-properties
      notifier
      obj
      property-list
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-CIRCLE-LINE-POINT                         ;
;;;                                                                    ;
;;;    Description:  This function is a call back function to an       ;
;;;                  editor reactor.                                   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      _reactor-make-association-properties-list     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         notifier = a valid vla object filled by the reactor        ;
;;;         obj-list = a list of vla object filled by the reactor      ;
;;;           arg-list = filled by the reactor.                        ;
;;;                                                                    ;
;;; Returned Value:  The last modified vla object.                     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (_reactor-make-association-properties-list                     ;
;;;           notifier                                                 ;
;;;           obj-list                                                 ;
;;;           properties-List )                                        ;
;;;--------------------------------------------------------------------;
(defun reactor-circle-line-point (notifier reactor arg-list)
  (_reactor-make-association-properties-list
    notifier
    (VLR-Data reactor)
    '("start-association" "end-association" "Radius-association")
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:DUMBELLS-TST                                    ;
;;;                                                                    ;
;;;    Description:  This function creates a line and two circles.     ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    reactor-circle-line-point                       ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:DUMBELLS-TST) or DUMBELLS-TST from              ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:DUMBELLS-TST
       (/ circle1 circle2 line obj-lst reactor acadModel)
  (setq
    acadModel (vla-get-ModelSpace
		(vla-get-ActiveDocument (vlax-get-acad-object))
	      )
    line1     (vla-addLine acadModel (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point'(0.0 50.0 0.0)))
    circle1   (vla-addCircle acadModel (vlax-3d-point '(0.0 0.0 0.0)) 20)
    circle2   (vla-addCircle acadModel (vlax-3d-point '(0.0 50.0 0.0)) 20)
    obj-lst   (list circle1 circle2 line1)
  )
  (foreach vla-obj obj-lst
    (vla-put-Color vla-obj acRed)
  )
  (vlax-ldata-put line1 "start-association" "StartPoint")
  (vlax-ldata-put line1 "end-association" "EndPoint")
  (vlax-ldata-put circle1 "start-association" "Center")
  (vlax-ldata-put circle2 "end-association" "Center")
  (vlax-ldata-put circle1 "Radius-association" "Radius")
  (vlax-ldata-put circle2 "Radius-association" "Radius")
  (setq	reactor
	 (VLR-Object-reactor
	   obj-lst
	   obj-lst
	   '((:vlr-modified . reactor-circle-line-point))
	 )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:DUMBELLS-INFO                                   ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:DUMBELLS-INFO) or DUMBELLS-INFO from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:DUMBELLS-INFO ()
  (terpri)
  (princ
    "\nThis example creates two red circles with the same "
  )
  (princ
    "\nradius and a red line connecting their centers. If you "
  )
  (princ
    "\nchange the radius of a circle the adjacent circle "
  )
  (princ
    "\nwill match the radius of the modified circle. If you"
  )
  (princ
    "\nmove the position of a line end or the line itself, "
  )
  (princ
    "\nthe circles will adjust to the end points of the line. "
  )
  (princ "\nRun the DUMBELLS-TST command to see this test.")
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "DUMBELLS-TST" "DUMBELLS-INFO")
	     *REACT-TEST-COMMANDS-INFO*
       )
)
