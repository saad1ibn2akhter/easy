;;
;;;
;;;    VPSYNC.LSP
;;;    Created 10/16/97 by Dominic Panholzer
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
 
 
(defun c:vpsync (/ is_perspective align_vp FLAG IN_MSPACE ANS VPM CNT DCNT VP_ELST VP_HLST EN LOOP SS)
 
 
; --------------- PERSPECTIVE CHECKING FUNCTION ------------------
; This function checks if a viewport is oin perspective view.
; It returns T if the vp is in perspective view else nil.
; ----------------------------------------------------------------
 
  (defun is_perspective (VP)
    (setq VP (entget VP '("ACAD"))                     ; Get viewport edata
          VP (cdr (car (cdr (assoc -3 VP))))           ; and its xdata
          VP (cdr (member (assoc 1070 VP) VP))         ; Strip out Extended data version #
          VP (cdr (assoc 1070 VP))                     ; and get the view mode
    )
    (equal 1 (logand 1 VP))                            ; Is the 1 bit set (perspective)?
  )
 
; ------------------- ALIGN VPORTS FUNCTION ----------------------
; This function takes a Master viewport entity name MVP and a
; list of viewport entities to be aligned to the master VP.
; ----------------------------------------------------------------
  (defun align_vp (VPM VP_ELST / PT1 PT2 VPM# VDIR PMS1 PMS2 CNT vplock na vp )
 
    (setq PT1 '(0 0)                                   ; set abitrary PS points for alignment
          PT2 '(1 1)
          VPM# (cdr (assoc 69 (entget VPM)))           ; get the master VP's number
    )
    (command "_.mspace")                               ; Get into MS
    (setvar "cvport" VPM#)                             ; Set the master VP current
    (setq VDIR (getvar "viewdir"))                     ; get the view direction
    (setq PMS1 (trans PT1 3 2)                         ; translate PT1 from PS to Display
          PMS1 (trans PMS1 2 0)                        ; and then from display to current ucs
          PMS2 (trans PT2 3 2)                         ; Do the same for PT2
          PMS2 (trans PMS2 2 0)
          CNT  0
    )
    (foreach na VP_ELST                                ; Go through the list of slave VPs
      (setq VP (cdr (assoc 69 (entget na))))           ; get its number
      (setvar "cvport" VP)                             ; set it current
      (setq vplock (acet-viewport-lock-set na nil))    ; unlock the viewport if needed
      (if (not (equal vdir
                      (getvar "viewdir")
                      0.000001
          )    );not equal
          (command "_.vpoint" (trans VDIR 0 1 T))      ; change the view direction if needed
      );if
      (alignspace PMS1 PMS2 PT1 PT2)                   ; align the points
      (if vplock
          (acet-viewport-lock-set na T)
      );if
    );foreach
  );defun
 
; ----------------------------------------------------------------
;                          MAIN PROGRAM
; ----------------------------------------------------------------
 
  (acet-error-init
    (list (list   "cmdecho" 0
                "regenmode" 1
          );list
          T
    );list
  );acet-error-init
 
  (acet-autoload (list "aspace.lsp" "(alignspace PT1 PT2 PT3 PT4)")) ; Autoload alignspace
 
  (cond
    ((not (equal 0 (getvar "tilemode")))
       (princ "\n  Command not allowed unless TILEMODE is set to 0  ")
    );cond #1
 
    ((and (setq FLAG (acet-viewport-next-pickable))                 ; If there is at least one
          (car FLAG)                                   ; non-perspective VP
     );and
 
     (if (> (getvar "cvport") 1)                       ; If MS is active
       (progn
         (setq IN_MSPACE (getvar "cvport"))            ; save the current VP
         (command "._pspace")                          ; and switch to PS
       )
     )
 
     ;(setq ANS  (car (entsel "\nSelect master viewport: "))
     ;;Added the following code to replace the above line.  Irregularly shaped floating viewports actuall
       ;;consist fo two entities (a pline and a viewport) with reactors on each other to point to each other
       ;;so a simple (entsel) returned a pline instead of a viewport. Had to uise the built-in filtering
       ;;capability of 'acet-ui-single-select' to get around this problem.
 
     (setq ANS (acet-ui-single-select '((0 . "viewport")) T ) ;setq
           LOOP T
     )
 
     (while LOOP
       (cond
         ((not ANS)
           (setq LOOP nil)
         )
         ((not (= "VIEWPORT" (cdr (assoc 0 (entget ANS))))) ; If selection is not a VP
           (prompt "\nInvalid selection. Try again.")       ; re-prompt the user
         )
         ((is_perspective ANS)
           (prompt "\n  Command may not be invoked on a viewport with perspective view  ")
         )
         ((= 0 (cdr (assoc 68 (entget ANS))))
           (prompt "\n  Command may not be invoked on a viewport which is turned off  ")
         )
         (T
           (setq VPM  ANS
                 LOOP nil
           )
           (redraw VPM 3)                              ; Highlight the Master VP
         )
       )
 
       (if LOOP
         (setq ANS  (car (entsel "\nSelect master viewport: ")))
       )
     )
 
     (if VPM
       (progn
         (prompt "\nSelect viewports to be aligned to master viewport.")
 
         (setq SS (ssget (list '(-4 . "<AND")
                               '(0 . "VIEWPORT")                                   ; Must be a viewport
                               (cons 410 (getvar "ctab"))
                               '(-4 . "<NOT") '(68 . 0)   '(-4 . "NOT>")           ;and not be turned off
                               '(-4 . "<NOT") '(-4 . "&") '(90 . 1) '(-4 . "NOT>") ;and not perspective
                               '(-4 . "AND>")
                          )
                  )
         )
 
         (if SS                                        ; If a valid selection was made
           (progn
             (setq CNT    0
                   DCNT   0
             )
 
             (while (setq EN (ssname SS CNT))          ; step through each viewport
               (if (is_perspective EN)                 ; check to see if it is in perspective
                 (setq DCNT (1+ DCNT))                 ; if so, increment the removed counter
                 (setq VP_ELST (append (list EN) VP_ELST)                 ; else add the VP to the list
                       VP_HLST (cons (cdr (assoc 5 (entget EN))) VP_HLST) ; and save its handle for storage
                 )
               )
               (setq CNT (1+ CNT))
             )
 
             (if (< 0 DCNT)
               (progn
                 (prompt (strcat "\n" (itoa (sslength SS)) " viewport(s) selected."))
                 (prompt (strcat "\n" (itoa DCNT) " perspective view viewport(s) removed from selection."))
               )
             )
           )
         )
         (redraw VPM 4)                                ; If no valid selection, Unhighlight Master VP
       )
     )
 
     (if (and VPM VP_ELST)                             ; If both Masetr VP and 'slave' VP(s) exist
       (progn
         (align_vp VPM VP_ELST)                        ; align them
         (setq VP_ELST (list VPM VP_ELST))
       )
     )
 
     (if IN_MSPACE                                     ; If MS was originally active
       (setvar "cvport" IN_MSPACE)                     ; return back to the original VP
       (command "_.pspace")                            ; else goto PS
     )
 
    );cond #2
    ((cadr flag)                                       ; If no no-perspective VP available
     (princ (cadr flag))                               ; display error message
    );cond #3
  );cond close
 
  (acet-error-restore)
  (princ)
)


(acet-autoload2	'("aspace.lsp"	(alignspace p1 p2 p3 p4)))
(princ)