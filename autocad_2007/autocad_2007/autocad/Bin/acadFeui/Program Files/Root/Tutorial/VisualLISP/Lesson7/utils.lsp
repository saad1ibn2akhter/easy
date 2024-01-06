;;;                                                                    ;
;;;  UTILS.LSP                                                         ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998          ;
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
;;;  This file is from the Garden Path tutorial, and represents the    ;
;;;  final state of the application at the end of Lesson 7. Use this   ;
;;;  file to check your work.                                          ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  First step is to load ActiveX functionality.  If ActiveX support  ;
;;;  already exists in document (can occur when Bonus tools have been  ;
;;;  loaded into AutoCAD), nothing happens.                            ;
;;;--------------------------------------------------------------------;

(progn
    (vl-load-com)
    (vl-load-reactors)
)

;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       ) ;_ end of vla-get-ModelSpace
) ;_ end of setq



;;;--------------------------------------------------------------------;
;;;     Function: Degrees->Radians                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function converts a number representing an      ;
;;;               angular measurement in degrees, into its radian      ;
;;;               equivalent.  There is no error checking done on the  ;
;;;               numberOfDegrees parameter -- it is always expected   ;
;;;               to be a valid number.                                ;
;;;--------------------------------------------------------------------;
(defun Degrees->Radians	(numberOfDegrees)
  (* pi (/ numberOfDegrees 180.0))
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: 3dPoint->2dPoint                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function takes one parameter representing a     ;
;;;               3D point (a list of three integers or reals), and    ;
;;;               converts it into a 2D point (a list of two reals).   ;
;;;               There is no error checking done on the 3dpt          ;
;;;               parameter --  it is always expected to be a valid    ;
;;;               point.                                               ;
;;;--------------------------------------------------------------------;
;;;   Work to do: Add some kind of parameter checking so that this     ;
;;;               function won't crash a program if it is passed a     ;
;;;               null value, or some other kind of data type than a   ;
;;;               3D point.                                            ;
;;;--------------------------------------------------------------------;
(defun 3dPoint->2dPoint	(3dpt)
  (list (float(car 3dpt)) (float(cadr 3dpt)))
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;     Function: list->variantArray                                   ;
;;;--------------------------------------------------------------------;
;;;  Description: This function takes one parameter representing a     ;
;;;               list of double values, e.g. a list of 2D points:     ;
;;;               '(p1.X p1.Y p2.X p2.Y p3.X p3.Y p4.X p4.Y).          ;
;;;		  The list is converted into an ActiveX                ;
;;;		  variant based on a safearray.                        ;
;;;               No error checking is performed on the parameter --   ;
;;;               it is assumed to consist of a list of doubles.       ;
;;;------------------------------------------------------------------- ;
(defun gp:list->variantArray (ptsList / arraySpace sArray)
					; allocate space for an array of 2d points stored as doubles
  (setq	arraySpace
	 (vlax-make-safearray
	   vlax-vbdouble		; element type
	   (cons 0
		 (- (length ptsList) 1)
	   )				; array dimension
	 )

  )
  (setq sArray (vlax-safearray-fill arraySpace ptsList))
					; return array variant
  (vlax-make-variant sArray)
)


;;;--------------------------------------------------------------------;
;;;     Function: xyzList->ListOfPoints                                ;
;;;--------------------------------------------------------------------;
;;;  Description: This function extracts and formats 3D point lists    ;
;;;               from one big list of reals, in the form:             ;
;;;                   (x y z x y z x y z ...)                          ;
;;;               This is the format of the data returned by the       ;
;;;               vla-get-coordinates function when applied to a       ;
;;;               standard polyline object.                            ;
;;;--------------------------------------------------------------------;
;;;               The return value will be a list in the format:       ;
;;;                    ((x y z) (x y z) (x y z) ... )                  ;
;;;--------------------------------------------------------------------;
(defun xyzList->ListOfPoints (coordList / ptlist)
  (while coordList
    (setq ptlist (append ptlist
			 (list (list (car coordList) (cadr coordList) (caddr coordList)))
		 ) ;_ end of append
	  coordList	 (cdddr coordList)
    ) ;_ end of setq
  ) ;_ end of while
  ptlist
  ;;; (setq ptlist ptlist)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: xyList->ListOfPoints                                 ;
;;;--------------------------------------------------------------------;
;;;  Description: This function extracts and formats 2D point lists    ;
;;;               from one big list of reals, in the form:             ;
;;;                   (x y x y x y ...)                                ;
;;;               This is the format of the data returned by the       ;
;;;               vla-get-coordinates function when applied to a       ;
;;;               lightweight polyline object.                         ;
;;;--------------------------------------------------------------------;
;;;               The return value will be a list in the format:       ;
;;;                    ((x y) (x y) (x y) ... )                        ;
;;;--------------------------------------------------------------------;
(defun xyList->ListOfPoints (coordList / ptlist)
  (while coordList
    (setq ptlist (append ptlist
			 (list (list (car coordList) (cadr coordList)))
		 ) ;_ end of append
	  coordList	 (cddr coordList)
    ) ;_ end of setq
  ) ;_ end of while
    ptlist
  ;;; (setq ptlist ptlist)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: CleanReactors                                        ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a general utility function used for cleaning ;
;;;               up reactors. It can be used during debugging, as     ;
;;;               well as cleaning up any open reactors before a       ;
;;;               drawing is closed.                                   ;
;;;--------------------------------------------------------------------;
(defun CleanReactors ()
  (setq	*commandReactor* nil		; clear the variable
	*DrawingReactor* nil		; clear the variable
	)

  (mapcar 'vlr-remove-all
	  '(:VLR-AcDb-reactor		 :VLR-Editor-reactor
	    :VLR-Linker-reactor		 :VLR-Object-reactor
	    :VLR-Command-Reactor	 :VLR-DeepClone-Reactor
	    :VLR-DocManager-Reactor	 :VLR-DWG-Reactor
	    :VLR-DXF-Reactor		 :VLR-Editor-reactor
	    :VLR-Insert-Reactor		 :VLR-Linker-Reactor
	    :VLR-Lisp-Reactor		 :VLR-Miscellaneous-Reactor
	    :VLR-Mouse-Reactor		 :VLR-Object-Reactor
	    :VLR-SysVar-Reactor		 :VLR-Toolbar-Reactor
	    :VLR-Undo-Reactor		 :VLR-Wblock-Reactor
	    :VLR-Window-Reactor		 :VLR-XREF-Reactor
	    )
	  ) ;_ end of mapcar
  ) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: square                                               ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns the square of a number         ;
;;;               example:  (square 7) returns 49                      ;
;;;--------------------------------------------------------------------;
(defun square (aNumber)
  (* aNumber aNumber)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: getPerp-Distance-and-Angle                           ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns a list with the distance and   ;
;;;               perpendicular angle to user pt3, and is determined   ;
;;;               by supplied points pt1 pt2.  Pt3 is "user input"     ;
;;;               and need not be at right angles.  This allows us to  ;
;;;               solve for cases where ortho mode is off.             ;
;;;  Example usage:                                                    ;
;;;        (setq Data  (getPerp-Distance-and-Angle pt1 pt2 pt3) )      ;
;;;--------------------------------------------------------------------;
;;;      Arguments:                                                    ;
;;;          pt1  seed point                                           ;
;;;          pt2  seed point                                           ;
;;;      Note:  pt1 and pt2 denote a "line" segment                    ;
;;;          pt3  "user point" (point to solve for)                    ;
;;;--------------------------------------------------------------------;
(defun getPerp-Distance-and-Angle (linept1 linept2 userpt3 / dist:pt1->pt2
				   dist:pt1->pt3 dist:pt2->pt3
				   dist:pt2->ToPerpPt)
  (setq	dist:pt1->pt2    (distance linept1 linept2)
	dist:pt1->pt3    (distance linept1 userpt3)
	dist:pt2->pt3    (distance linept2 userpt3)
	dist:pt2->ToPerpPt  (/ (- (+ (SQUARE dist:pt2->pt3)
				  (SQUARE dist:pt1->pt2))
			       (SQUARE dist:pt1->pt3))
			    (* 2.0 dist:pt1->pt2))  
  ) ;_ end of setq

  ;; return a list of the point perpendicular from userpt3
  ;; on line segment between linept1 and linept2, as well
  ;; as the angle & distance between userpt3 and perpPt
  (list
    (setq perpPt(polar linept2 (angle linept2 linept1) dist:pt2->ToPerpPt))
    (distance userpt3 perpPt)
    (angle userpt3 perpPt)
  ) ;_ end of list
) ;_end of defun


;;;--------------------------------------------------------------------;
;;;     Function: midPoint                                             ;
;;;--------------------------------------------------------------------;
;;;  Description: This function returns the point at the middle        ;
;;;               between two give points                              ;
;;;--------------------------------------------------------------------;
(defun midPoint	(pt1 pt2)
  (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.00))
) ;_ end of defun
;|«Visual LISP© Format Options»
(72 2 40 1 nil "end of " 60 9 0 0 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
