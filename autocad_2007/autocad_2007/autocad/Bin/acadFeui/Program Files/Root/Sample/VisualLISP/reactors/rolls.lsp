;;;                                                                    ;
;;;  ROLLS.LSP                                                         ;
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
;;; This file contains an example of object and editor reactors.       ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  NUM-OF-CIRCLES                                    ;
;;;                                                                    ;
;;;    Description:  This function calculates how many circles         ;
;;;                  can be contained within a circle.                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      big-radius = a number denoting a radius.                      ;
;;;    small-radius = a number denoting a radius.                      ;
;;;                                                                    ;
;;; Returned Value:  A number representing the number of circles.      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;               (num-of-circles 12 5)                                ;
;;;--------------------------------------------------------------------;
(defun num-of-circles (big-radius small-radius)
  ;;  (fix (/ (* pi big-radius) small-radius))  ;; Voided
  (fix
    (/ pi
       (atan
	 (/ small-radius
	    (sqrt
	      (- (* big-radius big-radius) (* small-radius small-radius))
	    )
	 )
       )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-M-CIRCLE-RADIUS                           ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event. It is responsible for        ;
;;;                  recalculating the quantity of circles.            ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    num-of-circles                                  ;
;;;                    make-circles-on-circle                          ;
;;;                    get-model-space                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vla circle object.                        ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;     (reactor-m-circle-radius notifier reactor arg-list)            ;
;;;--------------------------------------------------------------------;
(defun reactor-m-circle-radius (notifier     reactor	  arg-list
				/	     midle-circle circle1
				circle2	     m-new-rad	  c-new-rad
				circle-list  len	  num-circles
			       )
  (if (and
	(setq midle-circle (vlr-data reactor))
	(vlax-write-enabled-p midle-circle)
	(setq circle1 (vlax-ldata-get midle-circle "circle1"))
	(setq circle2 (vlax-ldata-get midle-circle "circle2"))
	(vlax-read-enabled-p circle1)
	(vlax-read-enabled-p circle2)
      )
    (progn
      (setq rad1	(vla-get-radius circle1)
	    rad2	(vla-get-radius circle2)
	    m-new-rad	(/ (+ rad1 rad2) 2) ;midle circle radius
	    circle-list	(vlax-ldata-get midle-circle "circles")
	    c-new-rad	(/ (abs (- rad1 rad2)) 2) ;roll  radius
	    len		(length circle-list) ;current number of rolls
	    num-circles	(num-of-circles m-new-rad c-new-rad)
					;new number of rolls
      )

      (if (not (= (vla-get-radius midle-circle) m-new-rad))
	(vla-put-radius midle-circle m-new-rad)
      )
      (if (not (= len num-circles))	;change number of rolls
	(progn
	  (foreach cl circle-list
	    (if	(not (vlax-erased-p cl))
	      (vla-Erase cl)
	    )
	  )
	  (setq	acadModel   (get-model-space)
		circle-list
			    (make-circles-on-circle
			      midle-circle
			      c-new-rad
			      num-circles
			    )
	  )
	  (vlax-ldata-put midle-circle "circles" circle-list)
	)
      )
      (if (and circle-list
	       (not (= (vla-get-radius (car circle-list)) c-new-rad))
	  )
	(foreach circle	circle-list
	  (if (vlax-write-enabled-p circle)
	    (vla-put-radius circle c-new-rad)
	  )
	)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-ROLLS                                      ;
;;;                                                                    ;
;;;    Description:  This function is responsible for passing          ;
;;;                  information to other procedures that display      ;
;;;                  the circles on the screen. Furthermore, this      ;
;;;                  function creates the neccesary reactor.           ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                        num-of-circles                              ;
;;;                        circles-tied-to-curve                       ;
;;;                        create-same-reactor                         ;
;;;                        reactor-make-same-center                    ;
;;;                        reactor-m-circle-radius                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         circle1 = a valid vla circle object.                       ;
;;;         circle2 = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr object reactor                        ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;               (create-rolls vla-circle1 vla-circle2)               ;
;;;--------------------------------------------------------------------;
(defun create-rolls (circle1	   circle2	 /
		     m-circle	   circles-list	 circle-reactor
		     rad	   num-circles	 acadModel
		    )
  (setq	acadModel      (get-model-space)
	m-circle       (make-middle-circle circle1 circle2)
	rad	       (/ (abs (- (vla-get-radius circle1) (vla-get-radius circle2)))
			  2
		       )
	num-circles    (num-of-circles (vla-get-radius m-circle) rad)
	circle-reactor (circles-tied-to-curve m-circle rad num-circles)
	circles-list   (vlax-ldata-get m-circle "circles")
  )
  (list
    (create-same-reactor
      (list circle1 circle2 m-circle)
      (function reactor-make-same-center)
    )
    (VLR-Object-reactor
      (list circle1 circle2)
      m-circle
      (list
	(cons :vlr-modified (function reactor-m-circle-radius))
      )
    )
    circle-reactor
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-MIDDLE-CIRCLE                                ;
;;;                                                                    ;
;;;    Description:  This function is responsible for creating         ;
;;;                  a small circle between the two vla                ;
;;;                  circle objects.                                   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                        load-line-types                             ;
;;;                        get-model-space                             ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         circle1 = a valid vla circle object.                       ;
;;;         circle2 = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A valid vla circle object.                        ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;               (make-middle-circle vla-circle1 vla-circle2)         ;
;;;--------------------------------------------------------------------;
(defun make-middle-circle (circle1 circle2 / middle-circle)
  (setq	middle-circle
	 (vla-AddCircle
	   (get-model-space)
	   (vla-get-center circle1)
	   (/ (+ (vla-get-radius circle1) (vla-get-radius circle2)) 2)
	 )
  )
  (if (load-line-types "Dashdot" "acad.lin")
    (vla-put-Linetype middle-circle "Dashdot"))
  (vlax-ldata-put middle-circle "circle1" circle1)
  (vlax-ldata-put middle-circle "circle2" circle2)
  (vla-put-Color circle1 acRed)
  (vla-put-Color circle2 acRed)
  middle-circle
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:ROLLS-TST                                       ;
;;;                                                                    ;
;;;    Description:  This function will create 4 circles. One outer    ;
;;;                  circle and three inner circles.                   ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    get-model-space                                 ;
;;;                    create-rolls                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object.                           ;
;;;                                                                    ;
;;;          Usage: (C:ROLLS-TST) or ROLLS-TST from                    ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:ROLLS-TST (/ center circle1 circle2 radius1 radius2 acadModel)
  (initget 1)				
  (setq center (GETPOINT "\nSelect center"))
  (initget 103)				;(+ 1 2 4 32 64)
  (setq rad1 (GETDIST center "\nSelect first raduis"))
  (initget 103)				;(+ 1 2 4 32 64)
  (setq rad2 (GETDIST center "\nSelect second raduis"))
  (terpri)
  (setq	acadModel (get-model-space)
	circle1	  (vla-AddCircle acadModel (vlax-3d-point center) rad1)
	circle2	  (vla-AddCircle acadModel (vlax-3d-point center) rad2)
  )
  (create-rolls circle1 circle2)
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:ROLLS-INFO                                      ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:ROLLS-INFO) or ROLLS-INFO from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:ROLLS-INFO ()
  (princ "\nTo run test call ROLLS-TST")
  (princ "\nYou will be asked to select a center,")
  (princ "\na radius of the first circle and the radius of the second circle.")
  (princ "\nTwo circles (colored red) will be drawn.")
  (princ "\nThe space between them will be filled with rolls (circles)")
  (princ "\nEach time you change position of one of the red circles the")
  (princ "\nscheme will also change.")
  (princ "\nEach time you change radius of one of the red circles ")
  (princ "\nthe optimal number of rolls will be recalculated and if needed")
  (princ "\nchanged.")
  (princ "\n")
  (princ)
)


;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "ROLLS-TST" "ROLLS-INFO")
	     *REACT-TEST-COMMANDS-INFO*))
	     
;; EOF
