;;;                                                                    ;
;;;  IMAGE.LSP                                                         ;
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

;;; Load the AutoCAD 2000 COM object model functions here.
(vl-load-com)

;;;--------------------------------------------------------------------;
;;;  This file demonstrates adding an Image using Visual LISP's        ;
;;;  ActiveX functions.                                                ;
;;;--------------------------------------------------------------------;
;;;--------------------------------------------------------------------;
;;;     Function:  INSERT-IMAGE                                        ;
;;;--------------------------------------------------------------------;
;;;                                                                    ;
;;;    Description:  Places an image into ACAD.                        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            fnm  = File name of image.                              ;
;;;             pt  = Insertion point for image.                       ;
;;;           scale = Scale insertion for the imag.                    ;
;;;       rot-angle = Rotation for the image.                          ;
;;;                          #<Vla Objec IAcadLine 022f2814>           ;
;;;                                                                    ;
;;; Returned Value:  A Vla Object such as:                             ;
;;;                     #<VLA-OBJECT IAcadRaster 027a1c4c>             ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(insert-image                                          ;
;;;		  (getfiled "Choose Image file"  ""                    ;
;;;			    "BMP"                                      ;
;;;			    4                                          ;
;;;		  )                                                    ;
;;;		  '(0 0)                                               ;
;;;		  1                                                    ;
;;;		  0                                                    ;
;;;		)                                                      ;
;;;--------------------------------------------------------------------;
(defun insert-image (fnm	pt	   scale      rot-angle
		     /		AcadApp	   ActiveDoc  ModelSpace
		    )
  (if fnm  ;; if the file name is present and not nil
    (progn
      (setq AcadApp    (vlax-get-acad-object)
	    ActiveDoc  (vla-get-ActiveDocument AcadApp)
	    ModelSpace (vla-get-ModelSpace ActiveDoc)
      )
      (vla-AddRaster
	ModelSpace
	fnm
	(vlax-3d-point pt)
	scale
	rot-angle
      )
    )
  )
)
