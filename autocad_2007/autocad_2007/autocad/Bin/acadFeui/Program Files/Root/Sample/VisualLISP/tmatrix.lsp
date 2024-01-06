;;;                                                                    ;
;;;  TMATRIX.LSP                                                       ;
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
;;;  This file demonstrates the use of vla-TransformBy to modify       ;
;;;  AutoCAD drawing entities                                          ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;;       Function:  none                                              ;
;;;                                                                    ;
;;;    Description:  a sample load and execute upon loading.           ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  Vla Object                                        ;
;;;                                                                    ;
;;;          Usage: Load and evaluate                                  ;
;;;--------------------------------------------------------------------;
;|
vla-TransformBy
Moves, scales, or rotates an object given a 4 x 4 transformation matrix.
The following table demonstrates the transformation matrix 
configuration, where R = Rotation, and T = Translation:
'(
  (R00 R01 R02 T0)
  (R10 R11 R12 T1)
  (R20 R21 R22 T2)
  (0.0 0.0 0.0 1.0)
 )
|;

(setq IAcadApplication (vlax-get-acad-object)
      ActiveDocument (vla-get-ActiveDocument IAcadApplication)
      ModelSpace (vla-get-ModelSpace ActiveDocument)
)

;Rotation Matrix: 90 Degrees about point 0,0,0
(setq ma1 '((0.000000  -1.000000  0.000000  0.000000)
	    (1.000000  0.000000  0.000000  0.000000)
	    (0.000000  0.000000  1.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Scaling Matrix: scale by 1.707107 about point 5,5,0
(setq ma2 '((1.707107  0.000000  0.000000  5.000000)
	    (0.000000  1.707107  0.000000  5.000000)
	    (0.000000  0.000000  1.707107  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Translation Matrix: move an entity by 10,10,0
(setq ma3 '((1.000000  0.000000  0.000000  10.000000)
	    (0.000000  1.000000  0.000000  10.000000)
	    (0.000000  0.000000  1.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Scaling Matrix: scale by 10 at point 0,0,0
(setq ma4 '((10.000000  0.000000  0.000000  0.000000)
	    (0.000000  10.000000  0.000000  0.000000)
	    (0.000000  0.000000  10.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))
;Scaling Matrix: scale by 10 at point 2,2
(setq ma5 '((10.000000  0.000000  0.000000  -18.000000)
	    (0.000000  10.000000  0.000000  -18.000000)
	    (0.000000  0.000000  10.000000  0.000000)
	    (0.000000  0.000000  0.000000  1.000000)))

;; New for AutoCAD 2000 - Create and Fill A Variant Array for the Transforms
(setq clr 0)
(foreach x (list 'ma1 'ma2 'ma3 'ma4 'ma5)
  (progn
    (setq translen (length (eval x)))
    (setq TransDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- translen)) (cons 0 (1- translen))))
    (vlax-safearray-fill TransDataA (eval x))
    (setq TransData (vlax-make-variant TransDataA (logior vlax-vbarray vlax-vbDouble)))
    (setq line (vla-AddLine ModelSpace (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point '(10.0 10.0 0.0))))
    (vla-TransformBy line TransData)
    (vla-Put-Color line (setq clr (1+ clr)))
    (vla-Update line)
   )
)
