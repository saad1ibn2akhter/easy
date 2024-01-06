;;
;;  saveall.lsp - SAVEALL command
;;
;;  Copyright © 2000 by Autodesk, Inc.
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
;;  Use, duplication, or disclosure by the U.S. Government is subject to
;;  restrictions set forth in FAR 52.227-19 (Commercial Computer
;;  Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;  (Rights in Technical Data and Computer Software), as applicable.
;;  
;;-------------------------------------------------------------------------
;;
;;  DESCRIPTION
;;    Implements the SAVEALL command.
;;
;;-------------------------------------------------------------------------
 
 
;;
;;  save all open drawings
;;
(defun C:SAVEALL (/ dwg saveDwg)
  ;;  save drawing
  (defun saveDwg (dwg / titled writeable name)
    (setq titled (= 1 (vlax-variant-value (vla-getvariable dwg "DWGTITLED")))
          writeable (= 1 (vlax-variant-value (vla-getvariable dwg "WRITESTAT")))
          name (if titled
                 (vlax-get dwg "fullname")
                 (vlax-variant-value (vla-getvariable dwg "DWGNAME")) )
    )
    (cond
      ;;  REFEDIT active ??
      ((/= "" (vlax-variant-value (vla-getvariable dwg "REFEDITNAME")))
        (acet-ui-message
          (acet-str-format "%1\nCannot Save while REFEDIT active." name)
          "AutoCAD - SAVEALL"
          Acet:ICONSTOP )
      )
      ;;  simple save if titled and writeable
      ((and titled writeable)
        (vla-save dwg)
      )
      ;;  otherwise ask for name first
      (T
        (if (setq name (ACET-FILE-WRITEDIALOG "%1\nCannot Save while REFEDIT active." name "dwg" "Acet:SaveAll" 1665))
          (vla-saveas dwg (vlax-make-variant name)) )
      )
    )
  )

  ;;  quietly
  (acet-error-init '(("CMDECHO" 0)))

  ;;  for each drawing
  (vlax-for dwg (vla-get-documents (vlax-get-acad-object))
    ;;  save if modified
    (if (/= 0 (vlax-variant-value (vla-getvariable dwg "DBMOD")))
      (saveDwg dwg) )
  )

  (acet-error-restore)
  (princ)
)


(princ)