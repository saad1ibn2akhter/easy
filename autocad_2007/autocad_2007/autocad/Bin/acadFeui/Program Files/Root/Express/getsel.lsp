;;
;;;
;;;    GETSEL.LSP
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
 
(defun c:GETSEL (/ LAY    ;; Layer of the selected entity
                   ENT    ;; Entity type of seleted entity
                   SS     ;; Selection set
                   SSLST  ;; Filter list
                   cspace ;; current space
                )
 
  (acet-error-init
         (list
           (list "cmdecho" 0
                 "expert"  0
           )
           T     ;flag. True means use undo for error clean up.
         )       ;list
  );acet-error-init
  (sssetfirst nil nil)
 
  ;;(setq LAY (car(entsel "\nSelect Object on layer to Select from <*>: ")))
  (setq LAY (car (entsel "\nSelect an object on the Source layer <*>: ")))
 
  (if LAY
    (setq LAY  (cdr(assoc 8 (entget LAY)))
          SSLST (list (cons 8 LAY ))
    )
  )
 
  ;;(setq ENT (car(entsel "\nSelect type of entity you want <*>: ")))
  (setq ENT (car(entsel "\nSelect an object of the Type you want <*>: ")))
 
  (if ENT
    (progn
      (setq ENT  (cdr(assoc 0 (entget ENT))))
      (if SSLST
         (setq SSLST (append (list (cons 0 ENT )) SSLST))
         (setq SSLST (list (cons 0 ENT )))
      )
    )
  )
  (if SSLST
    (progn
      (cond
        ((and LAY ENT)
          (prompt (acet-str-format "\nCollecting all %1 objects on layer %2..." ENT LAY))
        )
        (LAY
          (prompt (acet-str-format "\nCollecting ALL objects on layer %1..."  LAY ))
        )
        (ENT
          (prompt (acet-str-format "\nCollecting all %1 objects in the drawing..."  ENT ))
        )
        (T
          (prompt "\nCollecting all objects in the drawing...")
        )
      )
      (setq SS (ssget "_X" SSLST))
    )
    (progn
      (setq SS (ssget "_X"))
    ) ;progn
  )
  (if SS
    (progn
      (setq SS (sslength SS))
      (if (> SS 0)
        (if (= SS 1)
          (prompt
            (acet-str-format "\n%1 object has been placed in the active selection set." (itoa SS))
          )
          (prompt
            (acet-str-format "\n%1 objects have been placed in the active selection set." (itoa SS))
          )
        ) ;if
        (prompt"\nNothing selected.")
      )
    ) ;progn
  ) ;if
 
  (acet-error-restore)
 
  (princ)
 
);end defun


(princ)