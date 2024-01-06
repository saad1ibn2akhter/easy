;;
;;  revert.lsp - REVERT command
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
;;  Use, duplication, or disclosure by the U.S. Government is subject to
;;  restrictions set forth in FAR 52.227-19 (Commercial Computer
;;  Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) 
;;  (Rights in Technical Data and Computer Software), as applicable.
;;  
;;-------------------------------------------------------------------------
;;
;;  DESCRIPTION
;;    Implements the REVERT command.
;;
;;-------------------------------------------------------------------------
 
 
;;
;;  revert to previous saved version of this drawing
;;
(defun C:REVERT (/ dwg reply sdiOpen)
  ;;  special open for SDI mode
  (defun sdiOpen (name force)
    (vla-sendcommand
        (vla-get-activedocument
            (vlax-get-acad-object))
                (acet-str-format "(command \"_.OPEN\"%1)\n%2\n"
                                 (if force " \"_Y\"" "")
                                 name ) )
  )
 
  ;;  quiet
  (acet-error-init '(("CMDECHO" 0)))
  ;;  check for the necessary support code
  (if (acet-reg-get (strcat "HKEY_LOCAL_MACHINE\\"
                            (acet-reg-prodkey)
                            "\\Applications\\acadvba\\commands" )
                    "VBASTMT" )
    ;;  worth trying ??
    (if (= 1 (getvar "DWGTITLED"))
      (progn
        ;;  pick file name
        (setq dwg (strcat (getvar "DWGPREFIX") (getvar "DWGNAME")))
        ;;  drawing modified ??
        (if (and (/= 0 (getvar "DBMOD"))
                 (= Acet:IDCANCEL
                    (acet-ui-message
                        (acet-str-format "Abandon changes to %1?" dwg)
                                         "AutoCAD - REVERT"
                                         (+ Acet:OKCANCEL
                                            Acet:ICONWARNING
                                            Acet:DEFBUTTON2 ) ) ) )
          (exit)
        )
        ;; SDI ??
        (if (/= 0 (getvar "SDI"))
          (sdiOpen dwg (/= 0 (getvar "DBMOD")))
          (command "vbastmt" (acet-str-format
              "ThisDrawing.Close(FALSE):AcadApplication.Documents.Open \"%1\""
              dwg ) )
        )
      )
      (acet-ui-message "Drawing has never been saved."
                       "AutoCAD - REVERT"
                       Acet:ICONSTOP )
    )
    (acet-ui-message "VBA support not installed."
                     "AutoCAD - REVERT"
                     Acet:ICONSTOP )
  )
 
  (acet-error-restore)
  (princ)
)


(princ)