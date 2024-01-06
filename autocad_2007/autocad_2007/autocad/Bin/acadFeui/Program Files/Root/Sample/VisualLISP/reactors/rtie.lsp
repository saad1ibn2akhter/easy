;;;                                                                    ;
;;;  RTIE.LSP                                                          ;
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
;;; This file contains various utilities                               ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  POINT-AT-DISTANCE-ON-CURVE                        ;
;;;                                                                    ;
;;;    Description:  This function is used to modify a collection of   ;
;;;                  vla objects to share the same properties.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;      proportion = a valid real number                              ;
;;;                                                                    ;
;;; Returned Value:  A point.                                          ;
;;;		                                                       ;
;;;          Usage:                                                    ;
;;;                (point-at-distance-on-curve                         ;
;;;                      vla-curve-object                              ;
;;;                      proportion)                                   ;
;;;--------------------------------------------------------------------;
(defun point-at-distance-on-curve (aCurve proportion)
  (vlax-curve-getPointAtDist
    aCurve
    (+
      (* (- 1 proportion)
	 (vlax-curve-getDistAtParam
	   aCurve
	   (vlax-curve-getStartParam aCurve)
	 )
      )
      (* proportion
	 (vlax-curve-getDistAtParam
	   aCurve
	   (vlax-curve-getEndParam aCurve)
	 )
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-CIRCLES-ON-CURVE                             ;
;;;                                                                    ;
;;;    Description:  Calculate position of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                        point-at-distance-on-curve                  ;
;;;                        make-a-circle                               ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
;;;          radius = a valid real number                              ;
;;;  num-of-circles = an integer representing number of circles.       ;
;;;                                                                    ;
;;; Returned Value:  A list of points.                                 ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(make-circles-on-curve                                 ;
;;;                       aCurve                                       ;
;;;                       radius                                       ;
;;;                       num-of-circles                               ;
;;;              )                                                     ;
;;;              or                                                    ;
;;;            (make-circles-on-curve vla-curve 1.0 5)                 ;
;;;--------------------------------------------------------------------;
(defun make-circles-on-curve (aCurve	     radius
			      num-of-circles /
			      res-circles    proportion
			      ctrpt	     i
			      num-of-Intervals
			     )
  (setq	i 0
	num-of-Intervals
	 (if (vlax-curve-isClosed aCurve)
	   num-of-circles
	   (1- num-of-circles)
	 )
	num-of-circles (1- num-of-circles)   ;we starts from 0
  )
  (if (= 0 num-of-Intervals)
    (setq num-of-Intervals 1))
  (while (<= i num-of-circles)
    (setq
      proportion  (/ (float i) num-of-Intervals)
      ctrpt	  (point-at-distance-on-curve aCurve proportion)
      res-circles (cons
		    (make-a-circle ctrpt radius proportion)
		    res-circles
		  )
      i		  (1+ i)
    )
  )
  res-circles
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-A-CIRCLE                                     ;
;;;                                                                    ;
;;;    Description:  Calculate position of circles on a curve          ;
;;;                  with equal steps proportional to line length      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;          aCurve = a valid vla object arc Object.                   ;
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
;;; redefined in CTIE.LSP!!!
(defun make-a-circle (ctrpt radius proportion / new-circle)
  (setq	new-circle
	 (vla-AddCircle acadModel (vlax-3d-point ctrpt) radius)
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
;;; redefined in CTIE.LSP!!!
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
;;; redefined in CTIE.LSP!!!
(defun update-position (aCurve aCircle / old-center new-center)
  (if
    (and aCircle
	 (vlax-write-enabled-p aCircle)
	 (not
	   (equal (setq old-center (vla-get-center aCircle))
		  (setq	new-center
			 (point-at-distance-on-curve
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
;;;    Description:  Calculate position of circles on a curve          ;
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
;;; redefined in CTIE.LSP!!!
(defun circles-tied-to-curve (aCurve radius circl-number)
  (function update-position-reaction)	; prevent name drop
  (setq circles-lst (make-circles-on-curve aCurve radius circl-number))
  (vlax-ldata-put aCurve "circles" circles-lst)
  (VLR-Object-reactor
    (list aCurve)
    "circles"
    '((:vlr-modified . update-position-reaction))
  )
)

;;EOF