;;
;;  Flatten.lsp - Converts 3d geometry to 2d geometry.
;;
;;  Copyright © 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:flatten ( / ss ans )
 (acet-error-init (list nil 1))
 
 (princ "\nSelect objects to convert to 2d...")
 (if (not acet:flatn-hide)
     (setq acet:flatn-hide "No")
 );if
 
 (if (and (setq ss (ssget "_:l" '((-4 . "<NOT") (0 . "VIEWPORT") (-4 . "NOT>"))));setq
          (setq ss (car (acet-ss-filter (list ss nil T))))
     );and
     (progn
      (initget "Yes No")
      (setq ans (getkword 
                 (acet-str-format "\nRemove hidden lines? <%1>: "
                                  acet:flatn-hide
                 )
                );getkword
      );setq
      (if (not ans)
          (setq ans acet:flatn-hide)
          (setq acet:flatn-hide ans) 
      );if
      (if (equal ans "No")
          (acet-flatn ss nil)
          (acet-flatn ss T)
      );if
     );progn then
 );if
 (acet-error-restore)
);defun c:flatten


(acet-autoload2	'("FLATTENSUP.LSP"	(acet-flatn ss hide)))
(princ)