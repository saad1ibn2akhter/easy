;;;                                                                    ;
;;;  EDITEX.LSP                                                        ;
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
;;; This file shows examples of adding and removing reactors from the  ;
;;; editor.                                                            ;
;;;--------------------------------------------------------------------;


;;;;;; first look for existing reactors types
(vlr-types)

;;;;;;; then look available reactions 
(VLR-Reaction-Names :VLR-Editor-Reactor)

;;;--------------------------------------------------------------------;
;;;       Function:  C:EDITEX-TST                                      ;
;;;                                                                    ;
;;;    Description:  This function creates a reactor to test removal   ;
;;;                  from and re-activation of the reactor created.    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:EDITEX-TST) or EDITEX-TST from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:EDITEX-TST () 
(setq er
       (VLR-Editor-Reactor
	 nil
	 '((:vlr-commandWillStart . VLR-trace-reaction)
	   (:vlr-commandEnded  . VLR-trace-reaction)
	   (:vlr-lispWillStart . VLR-trace-reaction)
	   (:vlr-lispEnded . VLR-trace-reaction)
	   (:vlr-sysVarWillChange	. VLR-trace-reaction)
	   (:vlr-sysVarChanged .	VLR-trace-reaction)
	   )
	 ))
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:STOP-EDITEX-TST                                 ;
;;;                                                                    ;
;;;    Description:  This function stops or de-activates the           ;
;;;                  created reactor.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:EDITEX-TST) or EDITEX-TST from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:STOP-EDITEX-TST () 
  (vlr-remove er))

;;;;; now try some commands in ACAD console

;;;;;;;;; to stop reactor's work use
;;(vlr-remove er)

;;;;;;;;; to restore reactor's work use
;;(vlr-add er) 

;;;;;;;;; to make reactor persistent
;;(vlr-pers er)


;;;;; to make full trace of editor reactor
;;;;; for use select text inside commented region and use "LOAD selection"
;;;;; You will have a lot of trace info.
;|
(setq er1
       (VLR-Editor-reactor nil nil))

(defun vlr-full-trace (reactor)
  (foreach name (VLR-Reaction-Names reactor)
    (VLR-Reaction-Set reactor name 'VLR-trace-reaction)))

(vlr-full-trace er1)
|;
;;to stop ir 
;;(vlr-remove er1)

;;;--------------------------------------------------------------------;
;;;       Function:  C:EDITEX-INFO                                     ;
;;;                                                                    ;
;;;    Description:  This function displays the a simple help          ;
;;;                  associated with this file.                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:EDITEX-INFO) or EDITEX-INFO from                ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:EDITEX-INFO ()
  (princ "\nExample of Editor reactors.")
  (princ "\nEDITEX-TST command starts tracing some editors events.")
  (princ "\nDo something in ACAD console and then look at Visual Lisp Trace window.")
  (princ "\nTo test run EDITEX-TST command.")
  (princ "\nto stop test run STOP-EDITEX-TST command.")
  (princ)
)


;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "EDITEX-TST" "EDITEX-INFO" "STOP-EDITEX-TST")
	     *REACT-TEST-COMMANDS-INFO*))
