;;
;;
;;  movebak.lsp - MOVEBAK command
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
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  AUTODESK
;;  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR
;;  FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.  DOES NOT WARRANT THAT
;;  THE OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
;;
;;  Use, duplication, or disclosure by the U.S. Government is subject to
;;  restrictions set forth in FAR 52.227-19 (Commercial Computer Software -
;;  Restricted Rights) and DFAR 252.227-7013(c)(1)(ii) (Rights in Technical
;;  Data and Computer Software), as applicable.
;;
;;----------------------------------------------------------------
;;
;;  DESCRIPTION
;;  Provides an interface to the MOVEBAK ObjectARX app.
;;
;;----------------------------------------------------------------
 
 
(defun C:MOVEBAK (/ cur new)
  (acet-error-init nil)
  (if (member nil (mapcar
        '(lambda (x) (if (member x (arx)) T (arxload x nil)))
           '("acetutil.arx" "movebak.arx") ) )
    (alert "Could not load required ObjectARX modules.")
    (progn
      (setq cur (getenv "AcetMovebak")
            cur (if cur cur "")
            new (getstring T
                    (acet-str-format
                        "New value for MOVEBAK, or . for none <%1>: " cur ) )
            new (cond
                  ((= "" new) cur)
                  ((= "~" new) (acet-ui-pickdir
                                 "Select MOVEBAK directory" cur ) )
                  ((= "." new) "")
                  (T new)
                )
      )
      (if new
        (setq new (acet-str-replace "/" "\\" new)) )
      (if (and new
               (/= "" new)
               (/= ":\\" (substr new 2))
               (/= Acet:DIRECTORY (acet-file-attr new)) )
        (progn
          (princ (acet-str-format "Not a directory: %1" new))
          (setq new nil)
        )
      )
 
      (if new
        (setenv "AcetMoveBak" new) )
    )
  )
  (acet-error-restore)
  (princ)
)
 
;;  verify 'acetutil.fas'
(if (and (not acet-error-init) (not (load "acetutil" nil)))
  (progn
    (princ "Missing 'acetutil.fas', MOVEBAK not loaded.")
    (setq C:MOVEBAK nil)
  )
  (princ "MOVEBAK command loaded.")
)


(princ)
