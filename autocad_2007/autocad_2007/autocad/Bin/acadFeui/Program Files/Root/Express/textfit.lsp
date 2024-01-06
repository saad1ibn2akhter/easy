;;
;;;
;;;    TEXTFIT.LSP
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
 
 
(defun c:textfit (/ ename textent newend tmp start newpt val ltc_% ss txtsz)
 
  (acet-error-init
    (list
        (list  "cmdecho"    0
               "snapang"    0
              "limcheck"    0
              "orthomode"  1
        )
        T     ;flag. True means use undo for error clean up.
     ) ;list
  );acet-error-init
 
;;;End Error control
 
  (if (not (and
              (setq ss (ssget "_i"))
              (= (sslength ss) 1)
              (setq ename (ssname ss 0)
              )
           )
      )
    (setq ename  (car (entsel "\nSelect Text to stretch or shrink:" )))
  )
 
 
  (cond
    ((not (setq textent (if ename (entget ename))))
      (princ "\nNo object selected!")
    )
    ((/= (acet-dxf 0 textent) "TEXT")
      (princ "\nSelected object is not Text!")
    )
    ((acet-layer-locked (acet-dxf 8 textent))
      (princ "\nSelected object is on a locked layer!")
    )
    (t
      (setq txtsz (textbox textent))
      (setq newend (distance
                      (list
                        (caadr txtsz) ;upper right x coord
                        (cadar txtsz) ;lower left y coord
                      )
                      (car txtsz) ;; ll xyz
                   );distance
      );setq
      ;set snap along text entity
      (setvar "snapang"
        (angtof (angtos (acet-dxf 50 textent) 0 8) 0 )
      )
      (initget 0 "Start _Start")
      (setq
        tmp (getpoint (acet-dxf 10 textent) "\nSpecify end point or [Start point]: ")
      )
      (setvar "snapang" 0)
      (cond
        ((= (type tmp) 'STR) ;;new starting point to be selected
          (setq start (getpoint "\nSpecify new starting point: "))
          (if start
            (progn
              (acet-ucs-cmd (list "_E" (acet-dxf -1 textent)))
              (setvar "orthomode" 1)
              (setq newpt
                (if start
                  (getpoint (trans start 0 1) " ending point: ")
                  nil
                ) ;if
              ) ;setq
              (if newpt
                (setq newpt (trans newpt 1 0))
              )
              (setvar "orthomode" 0)
              (acet-ucs-cmd (list "_p"))
            ) ;progn
          ) if
        )
        ((not (null tmp))    ;;new ending point selected
          (setq start (acet-dxf 10 textent)
                newpt tmp)
        )
        (t  (setq start nil
                  newpt nil)
        )
      ) ;cond
      (if (and start newpt)
        (progn
          (setq val (assoc 41 textent) ;;current width factor
                val (if val (cdr val) 1.0)
                ltc_% (* (/ (distance start newpt) newend) val)
                textent (subst (cons 41 ltc_%)
                               (assoc 41 textent)
                               textent)
                textent (subst (cons 10 start)
                               (assoc 10 textent)
                               textent)
                textent (subst (cons 11 newpt)
                               (assoc 11 textent)
                               textent)
          ) ;setq
          (entmod textent)
          (entupd (acet-dxf -1 textent))
        )
      )  ;;end of points check
    )
  ) ;cond
  (acet-error-restore)
  (princ)
) ;end defun
 
 
 
 
(defun c:TFHELP (/)
 
(prompt " TEXTFIT will change the width factor of the selected text, \n")
(prompt " to fit within the user specified points.\n")
(prompt "\n")
(prompt " TEXTFIT will prompt:  Select Text to stretch/shrink:\n")
(prompt " The user is expected to select the text.\n")
(prompt "\n")
(prompt " TEXTFIT will then prompt:  Specify starting Point/<select new ending point>: \n")
(prompt " At which time the user can specify a new ending point \n")
(prompt "                          or\n")
(prompt " The user can provide the letter \"S\" for a new starting point\n")
(prompt " to which TEXTFIT will prompt:  Specify new starting point:  \n")
(prompt " and:  ending point: \n")
(textscr)
(princ)
)


(princ)
