;;;
;;;    DWGLOG.LSP
;;;    Copyright © 1999 by Autodesk, Inc.
;;;
;;;    Your use of this software is governed by the terms and conditions of the
;;;    License Agreement you accepted prior to installation of this software.
;;;    Please note that pursuant to the License Agreement for this software,
;;;    "[c]opying of this computer program or its documentation except as
;;;    permitted by this License is copyright infringement under the laws of
;;;    your country.  If you copy this computer program without permission of
;;;    Autodesk, you are violating the law."
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;  ----------------------------------------------------------------
 
 
(defun c:dwglog (/ ANS)
  (acet-error-init nil)
 
  (if (member "dwglog.arx" (arx))
    (progn
      (prompt "\nYou are about to turn off logging for drawing files.")
      (initget "Yes No")
      (setq ANS (getkword "\nDo you wish to continue? [Yes/No] <No>:"))
      (setq ANS (if (null ANS) "No" ANS))
      (if (= ANS "Yes")
        (progn
          (arxunload "dwglog.arx")
          (prompt "\nLogging for drawing files has been turned off.")
          (prompt "\nType 'dwglog' to to turn logging back on.")
        )
      )
    )
    (progn
      (if (findfile "dwglog.arx")
        (progn
          (arxload "dwglog.arx")
          (princ "\nLogging for drawing files has been turned on.")
        )
        (prompt "\nDWGLOG.ARX not found. Check your installation directories.")
      )
    )
  )
 
  (acet-error-restore)
  (princ)
)


(princ)