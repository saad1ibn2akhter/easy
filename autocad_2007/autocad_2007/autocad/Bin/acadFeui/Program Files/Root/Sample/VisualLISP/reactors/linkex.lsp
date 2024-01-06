;;;                                                                    ;
;;;  LINKEX.LSP                                                        ;
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
;;; This file contains reactor tets.                                   ;
;;;--------------------------------------------------------------------;
;;; Linker-reactor test

;;;;;; first look for existing reactors types

(vlr-types)

;;;;;;; then look available reactions 

(VLR-Reaction-Names :VLR-Linker-Reactor)


;;;--------------------------------------------------------------------;
;;;       Function:  STANDARD-UN/LOAD-REACTION                         ;
;;;                                                                    ;
;;;    Description:  This function copies a reaction to all objects    ;
;;;                  contained in the argument obj-list.               ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            self = a valid linker object.                           ;
;;;            lstr = a call back function to the call back function.  ;
;;;                                                                    ;
;;; Returned Value:  prints the current event and reaction names       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(standard-un/load-reaction                             ;
;;;		                self                                   ;
;;;                     lstr )                                         ;
;;;--------------------------------------------------------------------;
(defun standard-un/load-reaction (self lstr)
  (terpri)
  (prin1 (VLR-current-reaction-name))
  (print (car lstr))
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:LINKEX-TST                                      ;
;;;                                                                    ;
;;;    Description:  This function creates linking reactors.           ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    standard-un/load-reaction                       ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  a valid vla linker reactor object                 ;
;;;                                                                    ;
;;;          Usage: (C:LINKEX-TST) or LINKEX-TST from                  ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
;;; create and add reactor to ACAD
(defun C:LINKEX-TST ()
  (function standard-un/load-reaction)
  (setq	lr
	 (VLR-Linker-reactor
	   nil				;users' data
	   '((:vlr-rxAppLoaded . VLR-trace-reaction)
	     (:vlr-rxAppUnLoaded . VLR-beep-reaction)
	    )				;reactions
	 )
  )

  (setq	lr1
	 (VLR-Linker-reactor
	   nil				;users' data
	   '((:vlr-rxAppLoaded . standard-un/load-reaction)
	     (:vlr-rxAppUnLoaded . standard-un/load-reaction)
	    )				;reactions
	 )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:STOP-LINKEX-TST                                 ;
;;;                                                                    ;
;;;    Description:  This function stops all link messages.            ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:STOP-LINKEX-TST) or STOP-LINKEX-TST from        ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:STOP-LINKEX-tst ()
  (vlr-remove lr)
  (vlr-remove lr1)
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:LINKEX-INFO                                     ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:LINKEX-INFO) or LINKEX-INFO from                ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:LINKEX-INFO ()
  (princ "\nFor test run LINKEX-tst command.")
  (princ
    "\nThen load an arx application and see results at Visual Lisp console"
  )
  (princ "\nand Trace windows.")
  (princ
    "\nTo stop test run STOP-LINKEX-TST command."
  )
  (terpri)
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "LINKEX-TST" "LINKEX-INFO" "STOP-LINKEX-TST")
	     *REACT-TEST-COMMANDS-INFO*))
;;EOF

