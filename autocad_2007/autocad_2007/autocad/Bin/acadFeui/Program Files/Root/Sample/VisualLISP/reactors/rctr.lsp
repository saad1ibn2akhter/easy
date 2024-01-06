;;;                                                                    ;
;;;  RCTR.LSP                                                          ;
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
(vl-load-com)

;;;--------------------------------------------------------------------;
;;; General Note:  THIS FILE IS A MEMBER OF THE RCTR-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;; This file contains various examples that will enable you to:       ;
;;;                                                                    ;
;;; 1. Create circles with the same radii.                             ;
;;; 2. Are placed on a curve with equal spacing                        ;
;;; 3. And a reactor is attached to the arc (or more precise a curve), ;
;;;    that will notify an event to re-space the circles               ;
;;;    according to the new shape of the curve.                        ;
;;;--------------------------------------------------------------------;
;;; Globals defined:                                                   ;
;;;                                                                    ;

	(setq *use-persistent-reactor* nil)
	(setq *use-dialog* nil)

	(setq *previous-radius* 1.0)
	(setq *previous-circle-number* 2)
	(setq *previous-color* 1)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-DIST                                          ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a distance     ;
;;;                  from known point. User input is curtailed via a   ;
;;;                  call to initget whose sum of the bit values       ;
;;;                  determine the behavior of this function.          ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;                2           Prevents the user from responding       ;
;;;                            to the request by entering zero.        ;
;;;                                                                    ;
;;;                4           Prevents the user from responding       ;
;;;                            to the request by entering a            ;
;;;                            negative value.                         ;
;;;                                                                    ;
;;;                32          Uses dashed lines when drawing          ;
;;;                            rubber-band line or box. For those      ;
;;;                            functions with which the user can       ;
;;;                            specify a point by selecting a          ;
;;;                            location on the graphics screen,        ;
;;;                            this bit value causes the               ;
;;;                            rubber-band line or box to be           ;
;;;                            dashed instead of solid.                ;
;;;                            (Some display drivers use a             ;
;;;                            distinctive color instead of            ;
;;;                            dashed lines.)                          ;
;;;                            If the system variable POPUPS           ;
;;;                            is 0, AutoCAD ignores this bit.         ;
;;;                                                                    ;
;;;                64          Prohibits input of a Z                  ;
;;;                            coordinate to the getdist               ;
;;;                            function; lets an application           ;
;;;                            ensure that this function returns       ;
;;;                            a 2D distance.                          ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           point = a list of three reals that denotes where the     ;
;;;                   rubber-banding visual aid will commence.         ;
;;;           msg   = a string value to print on the Command: prompt.  ;
;;;                                                                    ;
;;; Returned Value:  a real number denoting a distance                 ;
;;;                                                                    ;
;;;          Usage: (get-dist '(0 0 0 ) "\nSelect a Point:")           ;
;;;--------------------------------------------------------------------;
(defun get-dist	(point msg)
  (if (null msg) (setq msg ""))
  (initget 103)				;(+ 1 2 4 32 64)
  (if point
    	(getdist point msg)
    	(getdist msg)
    )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-INTEGER                                       ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for an integer     ;
;;;                  value.                                            ;
;;;                  User input is curtailed via a call to initget     ;
;;;                  whose sum of the bit values determine the         ;
;;;                  behavior of this function.                        ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;                2           Prevents the user from responding       ;
;;;                            to the request by entering zero.        ;
;;;                                                                    ;
;;;                4           Prevents the user from responding       ;
;;;                            to the request by entering a            ;
;;;                            negative value.                         ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           msg   = a string value to print on the Command: prompt.  ;
;;;                                                                    ;
;;; Returned Value:  an integer.                                       ;
;;;                                                                    ;
;;;          Usage: (get-integer "\nEnter a Number:")                  ;
;;;--------------------------------------------------------------------;
(defun get-integer (msg / circl-number)
  (initget 7)	;;(+ 1 2 4)
  (setq circl-number (GETINT msg))
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-YES/NO                                        ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a response.    ;
;;;                  Yes is default.                                   ;
;;;                  User input is curtailed via a call to initget     ;
;;;                  whose sum of the bit values determine the         ;
;;;                  behavior of this function.                        ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;                2           Prevents the user from responding       ;
;;;                            to the request by entering zero.        ;
;;;                                                                    ;
;;;                4           Prevents the user from responding       ;
;;;                            to the request by entering a            ;
;;;                            negative value.                         ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           msg   = a string value to print on the Command: prompt.  ;
;;;                                                                    ;
;;; Returned Value:  T if Yes yes or a enter is selected.              ;
;;;                  Nil otherwise.                                    ;
;;;                                                                    ;
;;;          Usage: (get-Yes/No "\nDo you want to play a game?: ")     ;
;;;--------------------------------------------------------------------;
(defun get-Yes/No (msg)
  (initget "Yes No")
  (setq ans (getkword (strcat msg " <[Yes]/No> ")))
  (setq	ans (or	(null ans)
		(= (ascii ans) 89 ;|Y|;)
		(= (ascii ans) 121 ;|y|;)
	    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-NO/YES                                        ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a response.    ;
;;;                  No is default.                                    ;
;;;                  User input is curtailed via a call to initget     ;
;;;                  whose sum of the bit values determine the         ;
;;;                  behavior of this function.                        ;
;;;                                                                    ;
;;;                Bit value	Description                            ;
;;;                                                                    ;
;;;                1           Prevents the user from responding       ;
;;;                            to the request by entering              ;
;;;                            only ENTER.                             ;
;;;                                                                    ;
;;;                2           Prevents the user from responding       ;
;;;                            to the request by entering zero.        ;
;;;                                                                    ;
;;;                4           Prevents the user from responding       ;
;;;                            to the request by entering a            ;
;;;                            negative value.                         ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           msg   = a string value to print on the Command: prompt.  ;
;;;                                                                    ;
;;; Returned Value:  T if No no or a enter is selected.                ;
;;;                  Nil otherwise.                                    ;
;;;                                                                    ;
;;;          Usage: (get-No/Yes "\nDo you want to play a game?: ")     ;
;;;--------------------------------------------------------------------;
(defun get-No/Yes (msg)
  (initget "Yes No")
  (setq ans (getkword (strcat msg " <Yes/[No]> ")))
  (setq	ans (or	(null ans)
		(= (ascii ans) 78 ;|N|;)
		(= (ascii ans) 110 ;|n|;)
	    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-RADIUS-FROM-POINT                             ;
;;;                                                                    ;
;;;    Description:  This function prompts the user for a radius from  ;
;;;                  a known point.                                    ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    get-dist                                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           point = a list of three reals that denotes where the     ;
;;;                   rubber-banding visual aid will commence.         ;
;;;                                                                    ;
;;; Returned Value:  a real number denoting a distance                 ;
;;;                                                                    ;
;;;          Usage: (get-radius-from-point '(0 0 0 ))                  ;
;;;--------------------------------------------------------------------;
(defun get-radius-from-point (point)
	 (if point
	   (get-dist point "\nRadius: ")
	   (get-dist nil   "\nRadius: ")
	 )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-RUN-RCTR-TST-PARAMETERS                       ;
;;;                                                                    ;
;;;    Description:  This function sets a value of the global:         ;
;;;                  *use-dialog*, calls the dialog for parameters if  ;
;;;                  the user selected yes for "Use dialog? "          ;
;;;                  If the user selected no, a command line           ;
;;;                  interaction with the user commences to retreive   ;
;;;                  neccesary information.                            ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    call-GetParams-dlg                              ;
;;;                    prompt-run-rctr-tst-parameters                  ;
;;;                    get-model-space                                 ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  T if user pressed Ok.                             ;
;;;                  Nil if the user pressed Cancel.                   ;
;;;                                                                    ;
;;;          Usage: (get-run-rctr-tst-parameters)                      ;
;;;--------------------------------------------------------------------;
(defun get-run-rctr-tst-parameters (/ ans)
  (if (setq *use-dialog*
	     (if *use-dialog*
	       (get-Yes/No "Use dialog? ")
	       (not (get-No/Yes "Use dialog? "))
	     )
      )
    (call-GetParams-dlg)
    (prompt-run-rctr-tst-parameters)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CALL-GETPARAMS-DLG                                ;
;;;                                                                    ;
;;;    Description:  This function seeds temporary global varibles     ;
;;;                  from the global variables define previously.      ;
;;;                  Then invokes the dialog function and restores     ;
;;;                  the values from the user interaction to the       ;
;;;                  main global variables.                            ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    run-GetParams-dlg                               ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  T if user pressed Ok.                             ;
;;;                  Nil if the user pressed Cancel.                   ;
;;;                                                                    ;
;;;          Usage: (call-GetParams-dlg)                               ;
;;;--------------------------------------------------------------------;
(defun call-GetParams-dlg (/ ans)
  (setq	radius	      *previous-radius*
	circle-number *previous-circle-number*
	color	      *previous-color*
	ans	      (run-GetParams-dlg)
  )
  (cond
    ((= ans 0) nil)   ;OK button was pressed
    (t
     ;; remember new values as new defaults
     (setq *previous-radius*	    radius
	   *previous-color*	    color
	   *previous-circle-number* circle-number
     )
     ans
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  PROMPT-RUN-RCTR-TST-PARAMETERS                    ;
;;;                                                                    ;
;;;    Description:  This function invokes a command line              ;
;;;                  interaction with the user commences to retreive   ;
;;;                  neccesary information.                            ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    select-a-curve                                  ;
;;;                    get-radius-from-point                           ;
;;;                    get-integer                                     ;
;;;                    get-Yes/No                                      ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  T if the user input was retreived.                ;
;;;                  Nil if the user did not select a curve object,    ;
;;;                                                                    ;
;;;          Usage: (prompt-run-rctr-tst-parameters)                   ;
;;;--------------------------------------------------------------------;
(defun prompt-run-rctr-tst-parameters ()
  (if (setq aCurve (select-a-curve))
    (progn
      (setq radius (get-radius-from-point (vlax-curve-getStartPoint aCurve)))
      (setq circle-number (get-integer "\nNumber of circles: "))
      (setq color (get-integer "\nColor index: "))
      (setq *use-persistent-reactor*
	     (if *use-persistent-reactor*
	       (get-Yes/No "Make reactor persistent? ")
	       (not (get-No/Yes "Make reactor persistent? "))
	     )
      )
      t
    )
    nil
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-MODEL-SPACE                                ;
;;;                                                                    ;
;;;    Description:  This function creates an ACAD model space object. ;
;;;                  Note: acadModel is global.                        ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a global vla model space object.                  ;
;;;                                                                    ;
;;;          Usage: (create-model-space)                               ;
;;;--------------------------------------------------------------------;
(defun create-model-space (/ acadApp acadDoc)
  (and
    (setq acadApp (vlax-get-acad-object))
    (setq acadDoc (vla-get-ActiveDocument acadApp))
    (setq acadModel(vla-get-ModelSpace acadDoc))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-A-CURVE                                    ;
;;;                                                                    ;
;;;    Description:  This function prompts the user to select a        ;
;;;                  curve object.                                     ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a val curve object.                               ;
;;;                                                                    ;
;;;          Usage: (select-a-curve)                                   ;
;;;--------------------------------------------------------------------;
(defun select-a-curve (/ curve sel)
  (if
    (and
      (setq sel (entsel "Please choose a curve: "))
      (setq curve (vlax-ename->vla-object (car sel)))
      (vlax-curve-getStartPoint curve)	;test on curve
    )
     curve
     nil
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  RCTR-TST                                          ;
;;;                                                                    ;
;;;    Description:  This function aids the user in:                   ;
;;;                  1. Selecting a curve object.                      ;
;;;                  2. Gather information for circle radius, color    ;
;;;                     and quantity.
;
;;;                  3. Asks for persistency for the curve reactor.    ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    get-run-rctr-tst-parameters                     ;
;;;                    create-model-space                              ;
;;;                    circles-tied-to-curve                           ;
;;;                    create-same-reactor                             ;
;;;                    make-same-radius                                ;
;;;                    save-property                                   ;
;;;                    create-translate-curve-reactor                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a vlr object reactor.                             ;
;;;                                                                    ;
;;;          Usage: (rctr-tst)                                         ;
;;;--------------------------------------------------------------------;

(defun C:rctr-tst (/		  AcadModel    radius
		     circle-number	       start
		     aCurve	  color	       reactors
		     circles-list
		    )
;;; Get parameters from user
;;; and prepare model space
  (if (and (get-run-rctr-tst-parameters) (create-model-space))
    (progn
;;; setup reactor to tie circles on line
      (setq
	reactors (cons
		   (circles-tied-to-curve aCurve radius circle-number)
		   reactors
		 )
      )
;;; setup reactor to make circles have eq, radius
      (setq circles-list (vlax-ldata-get aCurve "circles"))
      (setq reactors (cons
		       (create-same-reactor
			 circles-list
			 (function make-same-radius)
					; prevent name drop
		       )
		       reactors
		     )
      )
;;; put color to circles and curve
      (foreach circle circles-list
	(vla-put-Color circle color)
        (vla-Update circle)
	(save-property circle "Center")	;prepare for the next step
      )
      (vla-put-Color aCurve color)
;;; setup reactor to make aCurve follow circles moves
      (setq reactors
	     (cons (create-translate-curve-reactor circles-list aCurve)
		   reactors
	     )
      )
;;; if needed make all reactors persistent
      (if *use-persistent-reactor*
	(foreach react reactors
	  (vlr-pers react)
	)
      ); if pers
    )
  )
 (vla-Update (vlax-get-acad-object))
 (princ "\nRctr-Tst Finished.")
 (princ)
)


;;; EOF
