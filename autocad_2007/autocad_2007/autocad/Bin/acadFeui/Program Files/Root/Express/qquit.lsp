;;
;;  qquit.lsp - QQUIT command
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
;;    Implements the QQUIT command.
;;
;;-------------------------------------------------------------------------
 
 
;;
;;  quick quit
;;
(defun C:QQUIT (/ item cur saveQuery)
  ;;  save if requested
  (defun saveQuery (item / titled writeable name reply)
    (setq titled (= 1 (vlax-variant-value (vla-getvariable item "DWGTITLED")))
          writeable (= 1 (vlax-variant-value (vla-getvariable item "WRITESTAT")))
          name (if titled
                 (vlax-get item "fullname")
                 (vlax-variant-value (vla-getvariable item "DWGNAME")) )
          reply (acet-ui-message
                    (acet-str-format "Save changes to %1?" name)
                                     "AutoCAD"
                                     (+ Acet:YESNOCANCEL Acet:ICONWARNING) ) )
    (cond
      ((= Acet:IDYES reply)
        (cond
          ;;  REFEDIT active ??
          ((/= "" (vlax-variant-value (vla-getvariable item "REFEDITNAME")))
            (acet-ui-message "Cannot Save while REFEDIT active."
                             "AutoCAD - QQUIT"
                             Acet:ICONSTOP )
            (exit)
          )
          ((and titled writeable)
            (vla-save item)
          )
          (T
            (if (setq name (ACET-FILE-WRITEDIALOG "Save Drawing As" name "dwg" "Acet:SaveAll" 1665))
              (vla-saveas item (vlax-make-variant name))
              (exit)
            )
          )
        )
      )
      ((= Acet:IDCANCEL reply)
        (exit) )
    )
  )
 
  ;;  only valid in MDI
  (if (= 0 (getvar "SDI"))
    (progn
      ;;  quiet
      (acet-error-init '(("CMDECHO" 0)))
      ;;  locate current doc
      (setq cur (vla-get-activedocument (vlax-get-acad-object)))
      ;;  for each doc
      (vlax-for item (vla-get-documents (vlax-get-acad-object))
        ;;  skip current doc
        (if (not (equal cur item))
          (progn
            ;;  save if modified ??
            (if (/= 0 (vlax-variant-value (vla-getvariable item "DBMOD")))
              (saveQuery item) )
            ;;  close without saving
            (vla-close item (vlax-make-variant :vlax-false))
          )
        )
      )
      ;;  close current
      (vla-sendcommand cur "_.QUIT ")
    )
    (command "_.QUIT")
  )
  (princ)
)


(vl-load-com)
(princ)
