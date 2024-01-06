;;;                                                                    ;
;;;  LISPDATA1.LSP                                                     ;
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
;;;  This file demonstrates the capability of Visual LISP to store     ;
;;;  AutoLISP data to a drawing.                                       ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here.
(if (car (atoms-family 1 '("vl-load-com"))) (vl-load-com))

;;;--------------------------------------------------------------------;
;;;       Function:  C:LISPDATA-TEST                                   ;
;;;                                                                    ;
;;;    Description:  Command which demonstrates the capability of      ;
;;;                  Visual LISP to store AutoLISP data to a drawing.  ;
;;;                  This is done in two ways:  First, by              ;
;;;                  storing data in a global dictionary               ;
;;;                  named "MY-DICT". Second, by storing data in an    ;
;;;                  AutoCAD circle entity.                            ;
;;;                                                                    ;
;;;              The ActiveX methods demonstrated are:                 ;
;;;                  (vlax-ldata-put <str/entity> <key> <data>)        ;
;;;                  (vlax-ldata-get <str/entity> <key> [<default>])   ;
;;;                  (vlax-ldata-delete <str/entity> <key>)            ;
;;;                  (vlax-ldata-list <str/entity>)                    ;
;;;                  (vlax-ldata-test <ldata>)                         ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:LISPDATA-TEST) or LISPDATA-TEST from the ACAD   ;
;;;                 Command: prompt.                                    ;
;;;--------------------------------------------------------------------;
(defun C:LISPDATA-TEST (/	 aApp	  aDoc	   key1	    val1
			modelSpace	  circleObj	    x
			handle	 entname  key2	   val2
		       )
  (princ "Visual LISP LispDATA demonstration")
  (setq	aApp (vlax-get-acad-object)
	aDoc (vla-get-ActiveDocument aApp)
  )

  ;; Add LDATA in global dictionary
  (princ "\nStep 1. LDATA in global named dictionary:\n")
  (setq	key1 "Key1"
	val1 '("Key1 value" 1 1.1 k1)
  )

  ;; Check value to fit in LDATA
  (or (vlax-ldata-test val1)
      (exit)
  )

  (vlax-ldata-put "MY-DICT" key1 val1)
  (princ "Dictionary: adding   -> ")
  (prin1 val1)
  (terpri)

  (setq x (vlax-ldata-get "MY-DICT" key1 "None"))
  (princ "Dictionary: getting  <- ")
  (prin1 x)
  (terpri)

  (setq x (vlax-ldata-list "MY-DICT"))
  (princ "Dictionary: listing  == ")
  (prin1 x)
  (terpri)

  ;; Get default value for no LDATA:
  (vlax-ldata-delete "MY-DICT" key1)
  (setq x (vlax-ldata-get "MY-DICT" key1 "NO VALUE"))
  (princ "Dictionary: deleted  -- ")
  (prin1 x)
  (terpri)


  ;; Add LDATA in Entity's xdictionary
  (princ "\nStep 2. LDATA in Entity's xdictionary:\n")
  (setq	modelSpace (vla-get-ModelSpace aDoc)
	circleObj  (vla-AddCircle modelSpace (vlax-3d-point '(10.0 10.0 0.0)) 10.0)
	handle	   (vla-get-Handle circleObj)
	entname	   (entlast)
  )
  (setq	key2 "Key2"
	val2 (list "Key2 value" 2 2.2 'k2 handle circleObj entname)
  )

  ;; Check value to fit in LDATA
  (or (vlax-ldata-test val2)
      (exit)
  )

  (vlax-ldata-put circleObj key2 val2)
  (princ "Entity: adding   -> ")
  (prin1 val2)
  (terpri)

  (setq x (vlax-ldata-get circleObj key2 "None"))
  (princ "Entity: getting  <- ")
  (prin1 x)
  (terpri)

  (setq x (vlax-ldata-list circleObj))
  (princ "Entity: listing  == ")
  (prin1 x)
  (terpri)

  ;; Get default value for no LDATA:
  (vlax-ldata-delete entname key2)
  (setq x (vlax-ldata-get entname key2 "NO VALUE"))
  (princ "Entity: deleted  -- ")
  (prin1 x)
  (terpri)

  ;; Clean up DWG
  (vla-erase circleObj)

  (princ "\nLDATA demonstration complete")
  (princ)
)

(princ "\nTo test: C:LISPDATA-TEST")
(princ)
