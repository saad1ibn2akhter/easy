;;;                                                                    ;
;;;  GETPARM.LSP                                                       ;
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
;;; This file contains various DCL functions which are called from     ;
;;; the external file RCTR.LSP                                         ;
;;;--------------------------------------------------------------------;
;;; Globals defined:
;;;  *GetParams-dlg-position*
;;;  *dcl-file-name*

;;;--------------------------------------------------------------------;
;;;       Function:  LOAD TIME EVALUATED                               ;
;;;                                                                    ;
;;;    Description:  Make the global variable that contains the dialog ;
;;;                  startup screen position.                          ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a string.                                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;--------------------------------------------------------------------;
(SETQ *GetParams-dlg-position* '(-1 -1)) ;;default startup position


;;;--------------------------------------------------------------------;
;;;       Function:  LOAD TIME EVALUATED                               ;
;;;                                                                    ;
;;;    Description:  Make the global variable that contains the dialog ;
;;;                  name to load.                                     ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a string.                                         ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;--------------------------------------------------------------------;
(if (= 3 (VL-STRING-MISMATCH "RTS" (_VLISP-VERSION) nil nil t))
  ;; in IDE, assume standard Visual Lisp directory structure
  (setq	*dcl-file-name*
	 (findfile
	   (strcat (VL-FILENAME-DIRECTORY (strcase (findfile "vl.arx")))
		   "/sample/VisualLisp/reactors/getparm.dcl"
	   )
	 )
  )
  nil
)
;; not in IDE or file not found, the default path will be used.	
(if (= nil *dcl-file-name*)
  (setq *dcl-file-name* "getparm.dcl")
  nil
)


;;;--------------------------------------------------------------------;
;;;       Function:  GETPARAMS-DLG-CALLBACK                            ;
;;;                                                                    ;
;;;    Description:  This function returns the values selected by the  ;
;;;                  user from the main dialog run-GetParams-dlg.      ;
;;;                                                                    ;
;;;                  Note: This function is not used.                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            bool = a value T or nil. This argument if T will save   ;
;;;                   the current dialog position upon exiting. If     ;
;;;                   nil the function will make a list from the       ;
;;;                   current value of $x and $y.                      ;
;;;            code = an integer that denotes what information needs   ;
;;;                   to be returned.                                  ;
;;;                                                                    ;
;;;                   If code value is:                                ;
;;;                    12 = Return the present value of color          ;
;;;                    20 = Return the present value of circle-number  ;
;;;                    30 = Return the present value of radius         ;
;;;                    50 = Return the present value of                ;
;;;                         *use-persistent-reactor*                   ;
;;;                                                                    ;
;;; Returned Value:   the value of the code argument.                  ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (GetParams-dlg-callback T 20)                                  ;
;;;--------------------------------------------------------------------;
(DEFUN GetParams-dlg-callback (bool code)
  (COND
	((= code 12) (SETQ color (ATOI $value)))
	((= code 20) (SETQ circle-number (ATOI $value)))
	((= code 30) (SETQ radius (ATOF $value)))
	((= code 50) (SETQ *use-persistent-reactor* 
	             (not *use-persistent-reactor*)))
  )
  (IF bool
    (SETQ *GetParams-dlg-position* (DONE_DIALOG code))
    (SETQ *GetParams-dlg-position* (LIST $x $y))
  ) ;_ end of if
  code
)

;;;--------------------------------------------------------------------;
;;;       Function:  GETPARAMS-DLG-POPUPS                              ;
;;;                                                                    ;
;;;    Description:  This function is responsible in placing default   ;
;;;                  values in various edit fields within the dialog.  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:   nothing of importance.                           ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (GetParams-dlg-popups)                           ;
;;;--------------------------------------------------------------------;
(DEFUN GetParams-dlg-popups ( / db)
  (SET_TILE "key-Color" (ITOA color))
  (SET_TILE "key-Number" (ITOA circle-number))
  (SET_TILE "key-Radius" (RTOS radius))
  (SET_TILE "key-Persistent-reactors" 
            (if *use-persistent-reactor* "1" "0"))
  (mode_tile "key-Get-radius" (if aCurve 0 1))
  (mode_tile "accept" (if (and radius aCurve) 0 1))
)

;;;--------------------------------------------------------------------;
;;;       Function:  RUN-GETPARAMS-DLG                                 ;
;;;                                                                    ;
;;;    Description:  This function returns the values selected by the  ;
;;;                  user from the main dialog run-GetParams-dlg.      ;
;;;                                                                    ;
;;;                  Required Functions: none                          ;
;;;                                                                    ;
;;;                  Required variables with values:                   ;
;;;                                                                    ;
;;;                            color         = an integer value        ;
;;;                            circle-number = an integer value        ;
;;;                            radius        = a real number           ;
;;;                                                                    ;
;;;                                                                    ;
;;;                    Note: The variables noted above                 ;
;;;                    are globalized within function                  ;
;;;                    the calling function call-GetParams-dlg         ;
;;;                    and is located in RCTR.LSP.                     ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:   none                                             ;
;;;                                                                    ;
;;; Returned Value:   0 if the user pressed the cancel button.         ;
;;;                   1 if the user pressed the ok button.             ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                  (run-GetParams-dlg)                               ;
;;;--------------------------------------------------------------------;
(DEFUN run-GetParams-dlg ( /   dcl_id	tmp	what_next )
  (IF (NULL
	(SETQ dcl_id (load_dialog *dcl-file-name*))
      )
    (alert "Unable to load DCL file")
  )
  (SETQ	what_next 1000)
  (WHILE (> what_next 1)
    (IF	(NOT (NEW_DIALOG
	       "DLG_GetParams"
	       dcl_id
	       ""
	       *GetParams-dlg-position*
	     )
	)
      (alert "Unable to create new dialog")
    )
    (GetParams-dlg-popups)
    (SETQ what_next (START_DIALOG))
    (COND
      ((= what_next 10)
       (setq aCurve (select-a-curve))
      )
      ((= what_next 40)
       (setq radius (get-radius (vlax-curve-getStartPoint aCurve)))
      )
    )
  )
  (unload_dialog dcl_id)
  what_next
)


;;; EOF
