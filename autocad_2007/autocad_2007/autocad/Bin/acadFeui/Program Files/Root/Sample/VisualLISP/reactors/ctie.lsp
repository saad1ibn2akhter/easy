;;;                                                                    ;
;;;  CTIE.LSP                                                          ;
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
;;; This file contains various reactor and functions.                  ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;       Function:  GET-MODEL-SPACE                                   ;
;;;                                                                    ;
;;;    Description:  This function test if the global variable         ;
;;;                  *current-model-space* is set. If it is the        ;
;;;                  current value of *current-model-space* is         ;
;;;                  returned. Otherwise the value of the global       ;
;;;                  variable *current-model-space* is created.        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A vla model space object                          ;
;;;                  is returned such as:                              ;
;;;                  #<VLA-OBJECT IAcadModelSpace 027a34c0>            ;
;;;                                                                    ;
;;;          Usage: (get-model-space)                                  ;
;;;--------------------------------------------------------------------;
(defun get-model-space (/ tmp)
  (cond (*current-model-space* *current-model-space*)
        ((and (setq tmp (vlax-get-acad-object))
              (setq tmp (vla-get-activedocument tmp))
              (setq tmp (vla-get-modelspace tmp))
         )
         (setq *current-model-space* tmp)
        )
        (t nil)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-POINT-AT-PROPORTION-ON-CURVE                  ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;      proportion = a valid integer                                  ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A list of points.                                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(get-point-at-proportion-on-Curve                      ;
;;;                       vla-Curve-Object                             ;
;;;                             proportion )                           ;
;;;--------------------------------------------------------------------;
(defun get-point-at-proportion-on-Curve
       (aCurve proportion / str-par end-par param)
  (setq	str-par	(vlax-curve-getStartParam aCurve)
	end-par	(vlax-curve-getEndParam aCurve)
	param	(+ (* str-par (- 1 proportion)) (* end-par proportion))
  )
  (vlax-curve-getPointAtParam aCurve param)
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-CIRCLES-ON-CIRCLE                            ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;       n-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of points.                                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(make-circles-on-circle                                ;
;;;                       vla-Curve-Object                             ;
;;;                       radiusOfCircles                              ;
;;;                       numberOfCircles                              ;
;;;              )                                                     ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun make-circles-on-circle (aCurve	     radius
			       n-circles     /
			       res-circles   proportion
			       posn	     index
			       num-of-Intervals
			      )
  (setq	index 0
	num-of-Intervals n-circles
	n-circles (1- n-circles)
  )
  (if (= 0 num-of-Intervals)
    (setq num-of-Intervals 1)
  )
  (while (<= index n-circles)
    (setq
      proportion  (/ (float index) num-of-Intervals)
      posn	  (get-point-at-proportion-on-Curve aCurve proportion)
      res-circles (cons
		    (make-a-circle posn radius proportion)
		    res-circles
		  )
      index	  (1+ index)
    )
  )
  res-circles
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-A-CIRCLE                                     ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla arc object.                          ;
;;;          radius = a valid real number                              ;
;;;       n-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of vla circle objects                      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(make-a-circle                                         ;
;;;                       vla-Curve-Object                             ;
;;;                       radiusOfCircles                              ;
;;;                       numberOfCircles                              ;
;;;              )                                                     ;
;;;              or                                                    ;
;;;		(make-a-circle pt1 1.0 5)                              ;
;;;--------------------------------------------------------------------;
(defun make-a-circle (posn radius proportion / new-circle)
    (setq new-circle
	 (vla-AddCircle (get-model-space) (vlax-3d-point posn) radius)
  )
  (vlax-ldata-put new-circle "proportion" proportion)
  new-circle
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-POSITION-REACTION                          ;
;;;                                                                    ;
;;;    Description:  This function updates the position of each        ;
;;;                  circle associated with the reactor.               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         update-position                            ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;         reactor = a valid real number                              ;
;;;        arg-list = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of vla circle objects                      ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;		(update-position-reaction                              ;
;;;                       aCurve                                       ;
;;;                       reactor                                      ;
;;;                       arg-list                                     ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun update-position-reaction	(aCurve reactor arg-list)
  (foreach circle (vlax-ldata-get aCurve (vlr-data reactor))
    (update-position aCurve circle)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-POSITION                                   ;
;;;                                                                    ;
;;;    Description:  This function updates the position of a circle    ;
;;;                  according its proportion property and the         ;
;;;                  the curve object.                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;         aCircle = a valid vla circle object                        ;
;;;                                                                    ;
;;; Returned Value:  A list of vla circle object.                      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(update-position                                       ;
;;;                       aCurve                                       ;
;;;                       aCircle                                      ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun update-position (aCurve aCircle / old-center new-center)
  (if
    (and aCircle
	 (vlax-write-enabled-p aCircle)
	 (not
	   (equal (setq old-center (vla-get-center aCircle))
		  (setq	new-center
			 (get-point-at-proportion-on-Curve
			   aCurve
			   (vlax-ldata-get aCircle "proportion")
			 )
		  )
	   )
	 )
    )
     (vla-put-center aCircle (vlax-3d-point new-center))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CIRCLES-TIED-TO-CURVE                             ;
;;;                                                                    ;
;;;    Description:  Calculate positiom of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                         make-circles-on-circle                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;       n-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr object reactor object.                ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(circles-tied-to-curve                                 ;
;;;                       aCurve                                       ;
;;;                       radius                                       ;
;;;                       n-circles                                    ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun circles-tied-to-curve (aCurve radius circl-number)
  (setq circles-lst (make-circles-on-circle aCurve radius circl-number))
  (vlax-ldata-put aCurve "circles" circles-lst)
  (VLR-Object-reactor
    (list aCurve)
    "circles"
    (list
      (cons :vlr-modified (function update-position-reaction))
    )
  )
)

;;;;;; To use test this file
;;;	1. draw a circle on the screen
;;;	2. Cut and paste the code below
;|
(setq e (circles-tied-to-curve
          (vlax-ename->vla-object (entlast))
          (getdist "\nRadius Distance:")
          (getint "\nNumber of Circles:")
        )
)
|;
;;EOF
