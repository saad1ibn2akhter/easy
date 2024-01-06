;;;                                                                    ;
;;;  DRAWLINE.LSP                                                      ;
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
;;;  This file is referenced from the Visual LISP documentation        ;
;;;--------------------------------------------------------------------;



;;;--------------------------------------------------------------------;
;;;       Function:  drawline                                          ;
;;;                                                                    ;
;;;    Description:  The (DRAWLINE) function prompts the user to       ;
;;;                  select two points, then draws a line using the    ;
;;;                  (command "_.line" ...) function. The function     ;
;;;                  performs very simple input validation -- in this  ;
;;;                  case,  simply checking to see if the two point    ;
;;;                  variables exist before drawing the line. The      ;
;;;                  function can be invoked by entering (DRAWLINE)    ;
;;;                  at either the Visual LISP Console window prompt,  ; 
;;;                  or at the AutoCAD Command: prompt.                ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (drawline)                                         ;
;;;--------------------------------------------------------------------;
(defun drawline	(/ pt1 pt2) ;; local variables pt1 and pt2 are declared
  ;; get two points from the user
  (setq	pt1 (getpoint "\nEnter the start point for the line: ")
	pt2 (getpoint pt1 "\nEnter the end point for the line: ")
  )
  ;; check to see that the two points exist
  (if (and pt1 pt2)
    (command "_.line" pt1 pt2 "")
    (princ "\nInvalid or missing points!")
  )
  ;; suppress any return value from the function - exit 'quietly'
  (princ)
)