;;;--------------------------------------------------------------------;
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
;;;  final state of the application at the end of Lesson 7. Use this   ;
;;;  file to check your work.                                          ;
;;;--------------------------------------------------------------------;


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
;;;--------------------------------------------------------------------;
;;; In lesson 6, the following field was added to the list             ;
;;;    (100 . tileList  ) --  the tiles in the path                    ;
;;;--------------------------------------------------------------------;
;;; In lesson 7, the following fields were added to the list           ;
;;; StartingPoint                                                      ;
;;;	(12 . BottomStartingPoint)      15------------------------14   ;
;;;	(15 . TopStartingPoint)         |                          |   ;
;;; EndingPoint                         10    ----pathAngle--->   11   ;
;;;	(13 .  BottomEndingPoint)       |                          |   ;
;;;	(14 . TopEndingPoint)           12------------------------13   ;
;;;                                                                    ;
;;;********************************************************************;
(defun C:GPath ( / gp_PathData gp_dialogResults 
		PolylineName gp_PathData tileList PolylineList
		)
  (setvar "OSMODE" 0)
  ;; Ask the user for input: first for path location and
  ;; direction, then for path parameters.  Continue only if you have
  ;; valid input.  Store the data in gp_PathData
  (if (setq gp_PathData (gp:getPointInput))
    (if	(setq gp_dialogResults
	       (gp:getDialogInput
		 (cdr (assoc 40 gp_PathData))
	       ) ;_ end of gp:getDialogInput
	) ;_ end of setq
      (progn

;;;	(princ "\nReceived gp_dialogResults: ") (princ gp_dialogResults)

	;; Now take the results of gp:getPointInput and append this to
	;; the added information supplied by gp:getDialogInput
	(setq gp_PathData (append gp_PathData gp_DialogResults))

	;; At this point, you have all the input from the user.
	;; In lesson 7, the gp:drawOutline function was modified to
	;; return a list of the pointer to the polyline as well as
	;; the list of boundary points (the 12, 13, 14, 15 lists)
	;; Draw the outline, storing the resulting polyline "pointer"
	;; in the variable called PolylineName, and the boundary 
	;; points into gp_pathData
	; (trace gp:drawOutline)
	(setq PolylineList     (gp:drawOutline gp_PathData)
	      PolylineName     (car PolylineList)
	      gp_pathData      (append gp_pathData (cadr PolylineList))
	) ;_ end of setq


	;; Next, it is time to draw the tiles within the boundary.
	;; The tileList contains a list of the object pointers for
	;; the tiles.  By counting up the number of points (using the
	;; length function), we can print out the results of how many
	;; tiles were drawn.
	(princ "\nThe path required ")
	(princ
	  (length
	    (setq tileList
		   (gp:Calculate-and-Draw-Tiles
		     ;; path data list
		     gp_PathData
		     ;; object creation style to use - should be nil
		     ;; when drawing initial path.  Subsequent calls
		     ;; from reactor will provide the function to use.
		     nil
		   ) ;_ end of gp:Calculate-and-Draw-Tiles
	    ) ;_ end of setq
	  ) ;_ end of length
	) ;_ end of princ
	(princ " tiles.")

	;; Add the list of pointers to the tiles (returned by
	;; gp:Calculate-and-Draw-Tiles) to the gp_PathData variable.
	;; This is stored in the reactor data for the reactor attached
	;; to the boundary polyline.  With this data, the polyline
	;; "knows" what tiles (circles) belong to it.
	(setq gp_PathData   (append (list (cons 100 tileList))
					; all the tiles
				    gp_PathData
			    ) ;_ end of append
	) ;_ end of setq

	;; Before we attach reactor data to an object let's look at
	;; the function vlr-object-reactor.
	;; vlr-object-reactor has the following arguments:
	;;	(vlr-object-reactor owners data callbacks)
	;;      The callbacks Argument is a list comprising of
	;; 		owner , Reactor_Object list
	;; 		For further explanation see Help system
	;; For this exercise we will use all arguments
	;; associated with vlr-object-reactor

	;; These reactor functions will excecute only if
	;; the polyline in  PolylineName is modified or erased
 
	(vlr-object-reactor

	  ;; The first argument for vlr-object-reactor is
	  ;; the "Owners List" argument.  This is where to
	  ;; place the object to be associated with the
	  ;; reactor.  In this case it is the vlaObject
	  ;; stored in PolylineName

	  (list PolylineName)

	  ;; The second argument contains the data for the path

	  gp_PathData

	  ;; The third argument is the list of specific reactor
	  ;; types that we are interested in dealing with

	  '(
	    ;; reactor that is called upon modification of the object
	    (:vlr-modified . gp:outline-changed)
	    ;; reactor that is called upon erasure of the object
	    (:vlr-erased . gp:outline-erased)
	   )
	) ;_ end of vlr-object-reactor



	;; Next, register a command reactor to adjust the polyline
	;; when the changing command is finished.
	(if (not *commandReactor*)
	  (setq	*commandReactor*
		 (VLR-Command-Reactor

		   nil				; No data is associated with the editor reactor
		   ;; call backs
		   '
		    (
		     (:vlr-commandWillStart . gp:command-will-start)
		     (:vlr-commandEnded . gp:command-ended)
		     )
		   ) ;_ end of vlr-editor-reactor
		)
	  )

	(if (not *DrawingReactor*)
	  (setq *DrawingReactor*
		 (VLR-DWG-Reactor

		   nil				; No data is associated with the editor reactor
		   ;; call backs
		   '
		    (
		     ;; This is extremely important!!!!!!!!!
		     ;; Without this notification, AutoCAD will 
		     ;; crash upon exiting.
		     (:vlr-beginClose . gp:clean-all-reactors)
		     )
		   ) ;_ end of vlr-editor-reactor
		)
	  )



  
	
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
;|«Visual LISP© Format Options»
(72 2 40 1 nil "end of " 60 9 0 0 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
