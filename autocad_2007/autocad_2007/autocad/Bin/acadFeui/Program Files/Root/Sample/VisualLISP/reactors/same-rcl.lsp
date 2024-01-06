;;;                                                                    ;
;;;  SAME-RCL.LSP                                                      ;
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
;;;  For a description of the entire project and a listing of the      ;
;;;  AutoCAD commands defined within it, see the source code file      ;
;;;  REAC-TST.PRJ.                                                     ;
;;;--------------------------------------------------------------------;



;;;--------------------------------------------------------------------;
;;;       Function:  C:SAME-RCL-TST                                    ;
;;;                                                                    ;
;;;    Description:  This command demonstrates the ability to          ;
;;;                  associate a reactor to 3 circles and once         ;
;;;                  modified all share the same proprties.            ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    reactor-make-same-radius-color                  ;
;;;                    create-same-reactor                             ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:SAME-RCL-TST) or C:SAME-RCL-TST from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:SAME-RCL-TST (/ cla-lst reactor)
  (function reactor-make-same-radius-color)
  (setq cla-lst (add-n-circles 3))
  (if (null cla-lst)
    (princ "No circles added")
    (progn
      (foreach cla cla-lst
	(vla-put-Color cla acRed)
      )
      (setq reactor (create-same-reactor
		      cla-lst
		      'reactor-make-same-radius-color
		    )
      )
    )
  )
  reactor
(princ)
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:SAME-RCL-INFO                                   ;
;;;                                                                    ;
;;;    Description:  This function displays a simple help file         ;
;;;                  in the ACAD Command: prompt.                      ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:SAME-RCL-INFO) or C:SAME-RCL-INFO from          ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;

(defun C:SAME-RCL-INFO ()
  (terpri)
  (princ
    "\nExample of reactor for making circles with the same"
  )
  (princ "\nradius and color.")
  (princ "\nThree circles will be created")
  (princ
    "\n(You will be asked to select center and radius of the circle.)"
  )
  (princ
    "\nTry to change radius or color of one of them."
  )
  (princ
    "\nYou will see that radius and color of all others changes too."
  )
  (princ "\nRun SAME-RCL-TST for test")
  (terpri)
  (princ)
)


;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "SAME-RCL-TST" "SAME-RCL-INFO")
	     *REACT-TEST-COMMANDS-INFO*
       )
)

;;EOF
