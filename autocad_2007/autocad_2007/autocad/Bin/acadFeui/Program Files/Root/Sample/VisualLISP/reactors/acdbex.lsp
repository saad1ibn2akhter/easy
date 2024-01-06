;;;                                                                    ;
;;;  ACDBEX.LSP                                                        ;
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
;;; This file contains various reactor AcDbReactors test               ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  ACDB-CALLBACK                                     ;
;;;                                                                    ;
;;;    Description:  This is a stub function. Its duties are to report ;
;;;                  the values of the arguments passed to it.         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;                  (AcDb-callback reactor arg-list)                  ;
;;;--------------------------------------------------------------------;
(defun AcDb-callback (reactor arg-list / ent)
  (princ "\nAcDb reaction ")
  (princ (VLR-current-reaction-name))
  (if (car arg-list)
    (princ "\nevent in the current graphical data base")
    (princ "\nevent in the other graphical data base")
    )
  (princ "\nAcDb callback second argument: ")
  (print (cadr arg-list))
  (terpri)
)

(setq ar (VLR-AcDb-reactor))    ;; create an object reactor 
                                ;; without data or callbacks

;;;--------------------------------------------------------------------;
;;;       Function:  VLR-FULL-TRACE                                    ;
;;;                                                                    ;
;;;    Description:  This function traces a reactor.                   ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;           reactor =  a valid vlr object reactor. Filled in by the  ;
;;;                      calling reactor.                              ;
;;; callback-function =  a function to be invoked                      ;
;;;                      during a reactor event                        ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                  (vlr-full-trace ar 'AcDb-callback)                ;
;;;--------------------------------------------------------------------;
(defun vlr-full-trace (reactor callback-function)
  (foreach name	(VLR-Reaction-Names reactor)
    (VLR-Reaction-Set reactor name callback-function)
  )
  reactor
)


;;examples
;;(vlr-full-trace ar 'AcDb-callback)
;;(vlr-full-trace ar 'VLR-trace-reaction)

;;(vlr-remove ar)

;;;--------------------------------------------------------------------;
;;;       Function:  C:COPYSELF-TEST                                   ;
;;;                                                                    ;
;;;    Description:  This function traces the reactor named ar.        ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-same-copy-reactor                        ;
;;;                    add-circle                                      ;
;;;                    reactor-make-same-radius-color                  ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value: a valid vlr reactor object.                        ;
;;;                                                                    ;
;;;          Usage: (C:ACDBEX-TST) or ACDBEX-TST from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:ACDBEX-TST ()
  (if ar
    (vlr-full-trace ar (function AcDb-callback))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:STOP-ACDBEX-TST                                 ;
;;;                                                                    ;
;;;    Description:  This function removes the reactor named ar.       ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value: a valid vlr reactor object.                        ;
;;;                                                                    ;
;;;          Usage: (C:STOP-ACDBEX-TST) or STOP-ACDBEX-TST from        ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:STOP-ACDBEX-TST ()
  (if ar
    (vlr-remove ar)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:ACDBEX-INFO                                     ;
;;;                                                                    ;
;;;    Description:  This function displays a simple help on the       ;
;;;                  ACAD Command: prompt.                             ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:ACDBEX-INFO) or ACDBEX-INFO from                ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:ACDBEX-INFO ()
  (terpri)
  (princ "This test shows how to trace")
  (princ "\nall modification of ACAD graphical data base.")
  (princ "\nYou will see all ACDB reactors calls and their arguments")
  (princ "\nin the Visual Lisp trace window and ACAD console.")
  (princ "\nFor test run ACDBEX-TST command.")
  (princ
    "\nTo stop test run  STOP-ACDBEX-TST command."
  )
  (terpri)
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "ACDBEX-TST" "ACDBEX-INFO" "STOP-ACDBEX-TST")
	     *REACT-TEST-COMMANDS-INFO*))
