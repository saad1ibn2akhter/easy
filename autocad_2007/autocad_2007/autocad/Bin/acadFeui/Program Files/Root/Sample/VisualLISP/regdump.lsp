;;;                                                                    ;
;;;  REGDUMP.LSP                                                       ;
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
;;; General Note:                                                      ;
;;;         Functions defined:                                         ;
;;;	                registry-tree-dump                             ;
;;;	                dump-registered-apps                           ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
;;;  This file demonstrates several registry-xxxx functions and how    ;
;;;  they can be used to create user defined registry functions.       ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here
(if (car (atoms-family 1 '("vl-load-com"))) (vl-load-com))

;;;--------------------------------------------------------------------;
;;;       Function:  REGISTRY-TREE-DUMP                                ;
;;;                                                                    ;
;;;    Description:  This function dumps the registry contents for     ;
;;;                  a specific key.                                   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             rkey    = Registry Key Name                            ;
;;;             indent  = indent by a string value.                    ;
;;;                       This value can be nil. If nil it             ;
;;;                       defaults to "" other this indent value is    ;
;;;                       incremented internally as required.          ;
;;;                                                                    ;
;;; Returned Value:  A consed list denoting:                           ;
;;;                  (Number-of-registry-decendants                    ;
;;;                     . Number-of-Keys)                              ;
;;;                  Note: This is used internally.                    ;
;;;                                                                    ;
;;;          Usage: (registry-tree-dump                                ;
;;;                     "HKEY_CURRENT_USER\\Software" ""               ;
;;;                     )                                              ;
;;;--------------------------------------------------------------------;
;;;
;;;
(defun registry-tree-dump (rkey indent / vs ks)
  (if (equal "\\" (substr rkey (strlen rkey)))
    (setq rkey (substr rkey 1 (1- (strlen rkey))))
    )
  (or indent (setq indent ""))

  (princ (strcat indent "Key: " rkey "\n"))
  (if (setq vs (vl-registry-descendents rkey t))	; value names
    (progn
      (princ indent)
      (princ "- values:\n")
      (foreach v (vl-sort vs '<)
	(princ indent)
	(princ (strcat
		 "  "
		 (if (equal v "") "@" v)
		 ": " ) )
	(prin1 (vl-registry-read rkey v))
	(terpri)
	) )
    (progn
      (princ indent)
      (princ "- no values\n")
      )
    )
  (if (setq ks (vl-registry-descendents rkey))		; subkey names
    (progn
      (princ indent)
      (princ "- subkeys:\n")
      (setq rkey (strcat rkey "\\")
	    indent (strcat indent "  ") )
      (foreach k (vl-sort ks '<)
	(registry-tree-dump (strcat rkey k) indent)
	) )
    (progn
      (princ indent)
      (princ "- no subkeys\n")
      )
    )
(cons (length ks) (length vs))
 )

;;;--------------------------------------------------------------------;
;;;       Function:  DUMP-REGISTERED-APPS                              ;
;;;                                                                    ;
;;;    Description:  This function dumps the registry database         ;
;;;                  subtree for every application that is registered  ;
;;;                  to the current ACAD .                             ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          registry-tree-dump                        ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (dump-registered-apps)                             ;
;;;--------------------------------------------------------------------;
(defun dump-registered-apps (/ AcadApp AppNames)
  ;; find registry key for current AutoCAD version
  (setq AcadApp "HKEY_LOCAL_MACHINE\\Software\\Autodesk\\AutoCAD"
	AcadApp (strcat AcadApp "\\"
			(vl-registry-read AcadApp "CurVer") )
	AcadApp (strcat AcadApp "\\"
			(vl-registry-read AcadApp "CurVer")
			"\\Applications" ) )
  ;; get list of registered applications
  (setq AppNames (VL-SORT (vl-registry-descendents AcadApp) '<))
  
  ;; dump registry subtree for every application
  (foreach app AppNames
    (princ (strcat "\n=== " app " registry subtree dump\n"))
    (setq app (vl-registry-read (strcat AcadApp "\\" app) "REGPATH"))
    ;; app starts with "\\\\" - Visual LISP does not like this
    (setq app (substr app 3)) ;; So we remove some \\.
    (registry-tree-dump app nil)
    )
  (length AppNames)
  )

;;;--------------------------------------------------------------------;
;;;                                                                    ;
;;; Note:                                                              ;
;;;  To start this function cut and paste the function call below.     ;
;;;     (dump-registered-apps)                                         ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;

;;; EOF
