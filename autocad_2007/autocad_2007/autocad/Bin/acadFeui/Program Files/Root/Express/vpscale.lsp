;;;
;;;    VPSCALE.LSP
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
 
;    find the scale of a viewport relative to paper space
;    Carl Bethea  11 April 91
;
;     Paul Vine   20 April 1999   Ported to 2000.
;
;--- paper -------------------------------------------------
; returns T if in paper space
(defun paper ()
   (> 2 (getvar "cvport")(getvar "tilemode")) ; port=1 & tile=0
)
;
;--- getx --------------------------------------------------
; return <nth> dotted pair of the extended entity data
; from an entity association list <data>
;
(defun getx (n data)
(nth n (cdadr (assoc -3 data)))
)
;
;
;
;--- c:vpscale ----------------------------------------------
; get the xp scale factor of a pspace viewport
;
(defun c:vpscale (/ ent data cvsize cvhgt units vpna flag)
 (cond
  ((not (equal 0 (getvar "tilemode")))
   (princ "\n  Command not allowed unless TILEMODE is set to 0  ") 
  )
  ((and (/= 1 (getvar "cvport"))
        (setq vpna (acet-currentviewport-ename))
        (equal 1 (logand 1 (cdr (assoc 90 (entget vpna)))))
   )
   (princ "\n  Command not allowed in perspective view  ") 
  )
  (T
 
      (acet-error-init
        (list
          (list "cmdecho" 0
                "luprec" (getvar "luprec")
                "dimzin" 8
          )
          T     ;flag. True means use undo for error clean up.
 
        );list
      );acet-error-init
 
 
      (if (paper)
        ;(setq ent (car (entsel "\nSelect edge of viewport: ")))
 
       ;;Added the following code to replace the above line.  Irregularly shaped floating viewports actuall
       ;;consist fo two entities (a pline and a viewport) with reactors on each other to point to each other
       ;;so a simple (entsel) returned a pline instead of a viewport. Had to uise the built-in filtering
       ;;capability of 'acet-ui-single-select' to get around this problem.
       (progn
          (while (not flag)
           (princ "\nSelect edge of viewport.")
           (setq ent (acet-ui-single-select '((0 . "viewport")) T )) ;setq
           (if (and ent
                    (= 1 (logand 1 (cdr (assoc 90 (entget ent)))))
               )
               (progn
                 (princ "\nViewports with perspective view on are not allowed.")
                 (setq flag nil)
               );progn
               (setq flag T)
           );if
          );while
        );progn
        (setq ent (acet-currentviewport-ename))
      )
      (cond
        ((and
            ent
            (setq data (entget ent '("ACAD")))
            (= "VIEWPORT" (acet-dxf 0 DATA))
         );and
          (setq cvhgt  (acet-dxf 41 DATA)  ; viewport height
                cvsize (cdr (getx 6 data))    ; viewsize from extended data
          )
          (prompt "\nPS:MS == ")
          (cond
            ((< cvsize cvhgt)
              (princ (rtos (/ cvhgt cvsize) 2))
              (princ ":1")
            )
            (T (princ "1:")
              (princ (rtos (/ cvsize cvhgt) 2))
            )
          );cond
          (setq units (getvar "lunits"))
          (setvar "luprec" 8)
          (cond
            ((= units 4)
              (prompt (strcat "\nViewport Scale: " (rtos (/ 12 (/ cvsize cvhgt))) " = 1'-0\""))
            )
            ((= units 3)
              (prompt (strcat "\nViewport Scale: 1\" = " (rtos (/ cvsize cvhgt))))
            )
          )
        )
        (T (prompt " no viewport found."))
      );cond
      (acet-error-restore)                                  ; Retsore values
  )
 );cond close 
  (princ)
);c:vpscale


(princ)