;;;                                                                    ;
;;;  LISPDATA2.LSP                                                     ;
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

;;; Load the COM object model functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;;       Function:  C:LDT                                             ;
;;;                                                                    ;
;;;    Description:  This function demonstrates how to change          ;
;;;                  LispDATA value attached to an AutoCAD entity      ;
;;;                  so that it can be UNDOne correctly.               ;
;;;                                                                    ;
;;;    Note: Attaching xdictionaries to objects will increase the      ;
;;;          size of the drawing.                                      ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  (C:LDT) Or LDT from the ACAD command line.        ;
;;;--------------------------------------------------------------------;

(defun C:LDT (/ entname curData newData cmdecho)
  (and
    (setq entname (car (entsel "\nEntity to change LispDATA ")))
    (if	(setq curData (vlax-ldata-get entname "LDT"))
      (princ (strcat "\nExisting LispDATA value: " curData))
      (progn
	(princ "\nNo LDATA attached")
	(setq curData "")
      )
    )
    (setq newData
	   (getstring t
		      (strcat "\nEnter LDATA string <" curData ">: ")
	   )
    )
    (/= newData "")
    (progn
      (setq cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.UNDO" "_m")
      (setvar "CMDECHO" cmdecho)
      (vlax-ldata-put entname "LDT" newData)
    )
  )
  (princ)
)
