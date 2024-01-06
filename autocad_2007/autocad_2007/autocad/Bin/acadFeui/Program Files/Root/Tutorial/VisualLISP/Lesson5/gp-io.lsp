;;;                                                                    ;
;;;  GP-IO.LSP                                                         ;
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
;;;  state of the application at the end of Lesson 5.  Use this file   ;
;;;  to check your work, or to start off Lesson 6 with the code as it  ;
;;;  appears in the tutorial.                                          ;
;;;--------------------------------------------------------------------;


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


;;;--------------------------------------------------------------------;
;;;     Function: gp:getDialogInput                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This function will ask the user to determine the     ;
;;;               following path parameters:                           ;
;;;                   Tile size, Tile spacing                          ;
;;;                   Boundary polyline type                           ;
;;;                   Entity creation method                           ;
;;;--------------------------------------------------------------------;
;;;  The function calls the gp_mainDialog dialog (which is defined     ;
;;;  in the file gpdialog.dcl)  This function is responsible for       ;
;;;  retrieving the following association list fields and returning    ;
;;;  this information:                                                 ;
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
;;;  * * * * * * * * * * * * * * * NOTE * * * * * * * * * * * * * * *  ;
;;;  The tile size and spacing values are set based on the pathWidth.  ;
;;;  The tile radius is set to 1/15th of the path width, and the tile  ;
;;;  spacing is set to 1/5th of the tile radius.  Adjust these values  ;
;;;  as required to draw a path with initial values to your liking.    ;
;;;--------------------------------------------------------------------;
(defun gp:getDialogInput (pathWidth / dcl_id objectCreateMethod
			  plineStyle tilerad tilespace result userclick
			  dialogLoaded dialogShow
			 )
  ;; prepopulate some values - entity creation styles are hardcoded
  ;; defaults, while the tile size and spacing is set from the path
  ;; size
  (setq	objectCreateMethod
	 "ACTIVEX"
	plineStyle
	 "LIGHT"
	tilerad	(/ pathWidth 15.0)
	tilespace (/ tilerad 5.0)
	dialogLoaded
	 T
	dialogShow
	 T
  ) ;_ end of setq

  ;;------------------------------------------------------------;;
  ;; **** NOTE: THE FOLLOWING LOAD_DIALOG FUNCTION ASSUMES THAT ;;
  ;; THE DCL FILE IS LOCATED IN THE CURRENT DIRECTORY OR IN ONE ;;
  ;; THE AUTOCAD SUPPORT DIRECTORIES!! ************************ ;;
  ;;------------------------------------------------------------;;
  ;; Load the dialog box.  Set up error checking to make sure   ;;
  ;; that the dialog file is loaded before continuing           ;;
  (if (= -1 (setq dcl_id (load_dialog "gpdialog.dcl")))
    (progn
      ;; There's a problem - display a message and set the
      ;; dialogLoaded flag to nil
      (princ "\nERROR: Cannot load gpdialog.dcl")
      (setq dialogLoaded nil)
    ) ;_ end of progn
  ) ;_ end of if

  ;; Load the gp_mainDialog.  If this operation fails, set the
  ;; dialogShow flag to nil.  Note: if there was a problem with
  ;; the load_dialog function above, this will be trapped here
  ;; by including the test for the dialogLoaded flag.
  (if (and dialogLoaded
	   (not (new_dialog "gp_mainDialog" dcl_id))
      ) ;_ end of and
    (progn
      ;; There's a problem...
      (princ "\nERROR: Cannot show dialog gp_mainDialog")
      (setq dialogShow nil)
    ) ;_ end of progn
  ) ;_ end of if

  (if (and dialogLoaded dialogShow)
    (progn
      ;; Set the initial state of the tiles
      (set_tile "gp_trad" (rtos tileRad 2 2))
      (set_tile "gp_spac" (rtos tileSpace 2 2))
      ;; Assign the actions (the functions to be invoked) to the dialog buttons
      (action_tile
	"gp_lw"
	"(setq plineStyle \"Light\")"
      ) ;_ end of action_tile
      (action_tile
	"gp_hw"
	"(setq plineStyle \"Pline\")"
      ) ;_ end of action_tile
      (action_tile
	"gp_actx"
	"(setq objectCreateMethod \"ActiveX\")"
      ) ;_ end of action_tile
      (action_tile
	"gp_emake"
	"(setq objectCreateMethod \"Entmake\")"
      ) ;_ end of action_tile
      (action_tile
	"gp_cmd"
	"(setq objectCreateMethod \"Command\")"
      ) ;_ end of action_tile
      (action_tile "cancel" "(done_dialog) (setq UserClick nil)")
      (action_tile
	"accept"
	(strcat	"(progn (setq tileRad (atof (get_tile \"gp_trad\")))"
		"(setq tileSpace (atof (get_tile \"gp_spac\")))"
		"(done_dialog) (setq UserClick T))"
	) ;_ end of strcat
      ) ;_ end of action_tile

      ;; Now that everything is set and ready to go, invoke the dialog.
      ;; Once it is on-screen, it controls the program flow, until the
      ;; user hits OK or cancel
      (start_dialog)

      ;; OK or cancel has been hit, you're out of the dialog.  Unload it
      (unload_dialog dcl_id)

      ;; Check for the value of the variable userClick.  This determines if
      ;; the user selected OK or cancel, and is represented by a value
      ;; of T or nil
      (if UserClick			; User clicked Ok
	;; Build the resulting data

	(progn
	  (setq	Result (list
			 (cons 42 tileRad)
			 (cons 43 TileSpace)
			 (cons 3 objectCreateMethod)
			 (cons 4 plineStyle)
		       ) ;_ end of list
	  ) ;_ end of setq
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if

  ;; If everything worked (the dialog loaded properly and the user
  ;; did not cancel the function) the result should look something
  ;; like this:
  ;;     ((42 . 0.5) (43 . 0.1) (3 . "ACTIVEX") (4 . "LIGHT"))
  ;; If the user cancelled, or the dialog load or show failed,
  ;; result will be nil
  Result
) ;_ end of defun

