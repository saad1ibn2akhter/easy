;;;                                                                    ;
;;;  GPMAIN.LSP                                                        ;
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
;;;  state of the application at the end of Lesson 3.  Use this file   ;
;;;  to check your work, or to start off Lesson 4 with the code as it  ;
;;;  appears in the tutorial.                                          ;
;;;--------------------------------------------------------------------;

;;;  In lesson 4, the following comment and code is moved to utils.lsp ;
;;;--------------------------------------------------------------------;
;;;  First step is to load ActiveX functionality.  If ActiveX support  ;
;;;  already exists in document (can occur when Bonus tools have been  ;
;;;  loaded into AutoCAD), nothing happens. 
;;;--------------------------------------------------------------------;

(vl-load-com)

;;;  In lesson 4, the following comment and code is moved to utils.lsp ;
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

;;;  In lesson 4, the following function is moved to utils.lsp         ;
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


;;;  In lesson 4, the following function is moved to utils.lsp         ;
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
  (list (float (car 3dpt)) (float (cadr 3dpt)))
) ;_ end of defun

;;;  In lesson 4, the following function is moved to utils.lsp         ;
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


;;;  In lesson 4, the following function is moved to gp-io.lsp         ;
;;;--------------------------------------------------------------------;
;;;     Function: gp:getPointInput                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will ask the user to select three      ;
;;;               points in the drawing, which will determine the      ;
;;;               path location, direction, and size.                  ;
;;;--------------------------------------------------------------------;
;;;  If the user responds to the get functions with valid data,        ;
;;;  use startPt and endPt to determine the position, length, and      ;
;;;  angle at which the path is drawn.                                 ;
;;;--------------------------------------------------------------------;
;;;  The return value of this function will be a list consisting of:   ;
;;;   (10 . Starting Point) ;; A list of 3 reals (a point) denotes     ;
;;;                         ;; the starting point of the garden path.  ;
;;;   (11 . Ending Point)   ;; A list of 3 reals (a point) denotes     ;
;;;                         ;; the ending point of the garden path.    ;
;;;   (40 . Width)          ;; A real number denoting boundary width   ;
;;;   (41 . Length)         ;; A real number denoting boundary length  ;
;;;   (50 . Path Angle)     ;; A real number denoting the angle of the ;
;;;                         ;; path, in radians                        ;
;;;--------------------------------------------------------------------;
(defun gp:getPointInput	(/ StartPt EndPt HalfWidth)
  (if (setq StartPt (getpoint "\nStart point of path: "))
    (if	(setq EndPt (getpoint StartPt "\nEndpoint of path: "))
      (if (setq HalfWidth (getdist EndPt "\nHalf width of path: "))
	;; if you've made it this far, build the association list
	;; as documented above.  This will be the return value
	;; from the function.
	(list
	  (cons 10 StartPt)
	  (cons 11 EndPt)
	  (cons 40 (* HalfWidth 2.0))
	  (cons 50 (angle StartPt EndPt))
	  (cons 41 (distance StartPt EndPt))
	) ;_ end of list
      ) ;_ end of if
    ) ;_ end of if
  ) ;_ end of if
) ;_ end of defun

;;;  In lesson 4, the following function is moved to gp-io.lsp         ;
;;;--------------------------------------------------------------------;
;;;     Function: gp:getDialogInput                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will ask the user to determine the     ;
;;;               following path parameters:                           ;
;;;                   Tile size, Tile spacing                          ;
;;;                   Boundary polyline type                           ;
;;;                   Entity creation method                           ;
;;;--------------------------------------------------------------------;
(defun gp:getDialogInput ()
  (alert
    "Function gp:getDialogInput will get user choices via a dialog"
  ) ;_ end of alert
  ;; For now, return T, as if every task in the function worked correctly
  T
) ;_ end of defun



;;;  In lesson 4, the following function is moved to gpdraw.lsp        ;
;;;--------------------------------------------------------------------;
;;;     Function: gp:drawOutline                                       ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will draw the outline of the garden    ;
;;;               path.                                                ;
;;;--------------------------------------------------------------------;
;;;  Note: no error checking or validation is performed on the         ;
;;;  BoundaryData parameter.  The sequence of items within this        ;
;;;  parameter do not matter, but it is assumed that all sublists      ;
;;;  are present, and contain valid data.                              ;
;;;--------------------------------------------------------------------;
;;;  Note: This function uses Activex as a means to produce the garden ;
;;;  path boundary.  The reason for this will become apparent during   ;
;;;  future lessons.  But here is a hint:  certain entity creation     ;
;;;  methods will not work from within a reactor-triggered function    ;
;;;--------------------------------------------------------------------;
(defun gp:drawOutline (BoundaryData /	VLADataPts PathAngle
		       Width	  HalfWidth  StartPt	PathLength
		       angm90	  angp90     p1		p2
		       p3	  p4	     polypoints	pline
		      )
  ;; extract the values from the list BoundaryData
  (setq	PathAngle  (cdr (assoc 50 BoundaryData))
	Width	   (cdr (assoc 40 BoundaryData))
	HalfWidth  (/ Width 2.00)
	StartPt	   (cdr (assoc 10 BoundaryData))
	PathLength (cdr (assoc 41 BoundaryData))
	angp90	   (+ PathAngle (Degrees->Radians 90))
	angm90	   (- PathAngle (Degrees->Radians 90))
	p1	   (polar StartPt angm90 HalfWidth)
	p2	   (polar p1 PathAngle PathLength)
	p3	   (polar p2 angp90 Width)
	p4	   (polar p3 (+ PathAngle (Degrees->Radians 180)) PathLength)
	polypoints (apply 'append
			  (mapcar '3dPoint->2dPoint (list p1 p2 p3 p4))
		   )
  )


  ;; ***** data conversion *****
  ;; Notice, polypoints is in AutoLISP format, consisting of a list of the
  ;; 4 corner points for the garden path.
  ;; The variable needs to be converted to a form of input parameter
  ;; acceptable to ActiveX calls.
  (setq VLADataPts (gp:list->variantArray polypoints))

  ;; Add polyline to the model space using ActiveX automation.
  (setq	pline (vla-addLightweightPolyline
		*ModelSpace*		; Global Definition for Model Space
		VLADataPts
	      ) ;_ end of vla-addLightweightPolyline

  ) ;_ end of setq
  (vla-put-closed pline T)
  ;; Return the ActiveX object name for the outline polyline
  ;; The return value should look something like this:
  ;; #<VLA-OBJECT IAcadLWPolyline 02351a34> 
  pline
) ;_ end of defun


;;;********************************************************************;
;;;     Function: C:GPath        The Main Garden Path Function         ;
;;;--------------------------------------------------------------------;
;;;  Description: This is the main garden path function.  It is a C:   ;
;;;               function, meaning that it is turned into an AutoCAD  ;
;;;               command called GPATH.  This function determines the  ;
;;;               overall flow of the Garden Path program              ;
;;;********************************************************************;
;;;  The gp_PathData variable is an association list of the form:      ;
;;;   (10 . Starting Point) -- A list of 3 reals (a point) denotes     ;
;;;                              the starting point of the garden path ;
;;;   (11 . Ending Point)   -- A list of 3 reals (a point) denotes     ;
;;;                              the ending point of the garden path   ;
;;;   (40 . Width)          -- A real number denoting boundary width   ;
;;;   (41 . Length)         -- A real number denoting boundary length  ;
;;;   (50 . Path Angle)     -- A real number denoting the angle of the ;
;;;                              path, in radians                      ;
;;;   (42 . Tile Size)      -- A real number denoting the size         ;
;;;                              (radius) of the garden path tiles     ;
;;;   (43 . Tile Offset)    -- Spacing of tiles, border to border      ;
;;;   ( 3 . Object Creation Style)                                     ;
;;;                         -- The object creation style indicates how ;
;;;                               the tiles are to be drawn.  The      ;
;;;                               expected value is a string and one   ;
;;;                               one of three values (string case is  :
;;;                               unimportant):                        ;
;;;                                "ActiveX"                           ;
;;;                                "Entmake"                           ;
;;;                                "Command"                           ;
;;;   ( 4 . Polyline Border Style)                                     ;
;;;                          -- The polyline border style determines   ;
;;;                               the polyline type to be used for the ;
;;;                               path boundary.  The expected value   ;
;;;                               one of two values (string case is    :
;;;                               unimportant):                        ;
;;;                                "Pline"                             ;
;;;                                "Light"                             ;
;;;********************************************************************;
(defun C:GPath (/ gp_PathData PolylineName)
  ;; Ask the user for input: first for path location and
  ;; direction, then for path parameters.  Continue only if you have
  ;; valid input.  Store the data in gp_PathData
  (if (setq gp_PathData (gp:getPointInput))
    (if	(gp:getDialogInput)
      (progn
	;; At this point, you have valid input from the user.
	;; Draw the outline, storing the resulting polyline "pointer"
	;; in the variable called PolylineName
	(setq PolylineName (gp:drawOutline gp_PathData))
	(princ "\nThe gp:drawOutline function returned <")
	(princ PolylineName)
	(princ ">")
	(Alert "Congratulations - your program is complete!")
      ) ;_ end of progn
      (princ "\nFunction cancelled.")
    ) ;_ end of if
    (princ "\nIncomplete information to draw a boundary.")
  ) ;_ end of if
  (princ)				; exit quietly
) ;_ end of defun

;;; Display a message to let the user know the command name
(princ "\nType GPATH to draw a garden path.")
(princ)