;;
;;;
;;;    SHP2BLK.LSP
;;;    Created 10/31/97 by Dominic Panholzer
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
 
 
(defun c:shp2blk (/ grplst getgname acet-wmfin-loc ANS THAW0 DOIT FLTR GLST GDICT SHP ED NAM INS SCL ROT SPC
                    LAY CLR LTP CLAY LOOP PTS BLK ZM TMPFIL CUR_LOCKED SS CNT ENT ENT_LOCKED 0_LOCKED VEC)
 
 
 
; --------------------- GROUP LIST FUNCTION ----------------------
;   This function will return a list of all the group names in the
;   drawing and their entity names in the form:
;   ((<ename1> . <name1>) ... (<enamex> . <namex>))
; ----------------------------------------------------------------
 
  (defun grplst (/ GRP ITM NAM ENT GLST)
 
    (setq GRP  (dictsearch (namedobjdict) "ACAD_GROUP"))
    (while (setq ITM (car GRP))                        ; While edata item is available
      (if (= (car ITM) 3)                              ; if the item is a group name
        (setq NAM (cdr ITM)                            ; get the name
              GRP (cdr GRP)                            ; shorten the edata
              ITM (car GRP)                            ; get the next item
              ENT (cdr ITM)                            ; which is the ename
              GRP (cdr GRP)                            ; shorten the edata
              GLST                                     ; store the ename and name
                  (if GLST
                    (append GLST (list (cons ENT NAM)))
                    (list (cons ENT NAM))
                  )
        )
        (setq GRP (cdr GRP))                           ; else shorten the edata
      )
    )
    GLST                                               ; return the list
  )
 
; ------------------- GET GROUP NAME FUNCTION --------------------
;   This function returns a list of all the group names in GLST
;   where ENT is a member. The list has the same form as GLST
; ----------------------------------------------------------------
 
  (defun getgname (ENT GLST / GDATA NAM NLST)
    (if (and GLST (listp GLST))
      (progn
        (foreach GRP GLST
          (setq GDATA (entget (car GRP)))
          (foreach ITM GDATA                           ; step through the edata
            (if (and
                  (= (car ITM) 340)                    ; if the item is a entity name
                  (eq (setq NAM (cdr ITM)) ENT)        ; and the ename being looked for
                )
              (setq NLST                               ; store the ename and name
                      (if NLST
                        (append NLST (list (cons (car GRP) (cdr GRP))))
                        (list (cons (car GRP) (cdr GRP)))
                      )
              )
            )
          )
        )
      )
    )
    NLST
  )
 
 
; -------------------- BONUS WMFIN FUNCTION ---------------------
;   This function runs WMFIN on the file FIL, scales the resulting
;   vectors, explodes the insert and removes the outside frame.
;   It returns a selection set of the entities brought in. If the
;   file cannot be found it returns nil
;
;   Note that this routine assumes the current view size is the
;   same as the one used while WMFOUT was invoked. If it is not
;   the resulting entities will have a scale that is different
;   than the original
;
;   External Functions:
;
;   VIEWPNTS       --> AC_BONUS.LSP   Returns upper left and lower right of screen
;   PIXEL_UNIT     --> AC_BONUS.LSP   Size of pixel in drawing units
;   MIDPNT         --> AC_BONUS.LSP   Returns midpoint between two points
;   B_LAYER_LOCKED --> AC_BONUS.LSP   Checks to see if layer is locked
; ----------------------------------------------------------------
 
  (defun acet-wmfin-loc (FIL / VIEW UPLFT 0_LOCKED CUR_LOCKED BLK RETURN)
    (if (/= (xstrcase (substr FIL (- (strlen FIL) 3) 4)) ".WMF")
      (setq FIL (strcat FIL ".WMF"))
    )
    (if (findfile FIL)
      (progn
        (setq VIEW     (acet-geom-view-points)
              UPLFT    (list (caar VIEW) (cadadr VIEW))
        )
 
        (if (acet-layer-locked "0")                       ; if layer 0 is locked
          (progn
            (command "_.layer" "_unl" "0" "")          ; unlock it
            (setq 0_LOCKED T)
          )
        )
 
        (if (acet-layer-locked (getvar "clayer"))         ; if current layer is locked
          (progn
            (command "_.layer" "_unl" (getvar "clayer") "")  ; unlock it
            (setq CUR_LOCKED T)
          )
        )
 
 
        (command "_.WMFIN" FIL UPLFT  "2" "" "")
 
        (setq BLK (cdr (assoc 2 (entget (entlast)))))  ; Get name of temp wmfin block
 
        (command "_.EXPLODE" (entlast))                ; Explode wmfin block
 
        (while (wcmatch (getvar "cmdnames") "*EXPLODE*")  ; Verify explode command is complete
          (command "")
        )
 
        (acet-table-purge "block" blk nil)
 
        (setq RETURN (ssget "_p"))                     ; Gather all ents created from explode
 
        (command "_.erase" RETURN "_R" "_W"
                 (polar (car VIEW) (* 0.25 Pi) (* 1.414213 (acet-geom-pixel-unit)))
                 (cadr VIEW)
                 ""
        )
 
        (if CUR_LOCKED
          (command "_.layer" "_lock" (getvar "clayer") "") ; relock current if needed
        )
        (if 0_LOCKED
          (command "_.layer" "_lock" "0" "")               ; relock 0 if needed
        )
      )
    )
    RETURN
  )
 
; ----------------------------------------------------------------
;                          MAIN PROGRAM
; ----------------------------------------------------------------
 
  (acet-error-init
        (list
         (list   "cmdecho"   0
                 "highlight" 1
                 "osmode"    0
                 "expert"    5
                 "limcheck"  0
                 "cecolor"   "bylayer"
         )
         T
        )
  )
 
  ;(acet-autoload (list "getext.arx" "(getgeomextents EN)"))
 
 
  (if (and
        (= (logand 1 (cdr (assoc 70 (tblsearch "layer" "0")))) 1) ; if layer 0 is frozen
        (not (= (logand 4 (getvar "cmdactive")) 4))               ; and no script is running,
      )
    (progn
      (initget "Yes No" 128)
      (setq ANS (getkword "\nSHP2BLK cannot run if Layer 0 is frozen. Would you like to thaw it? <Y>: "))
      (if (or (= ANS "Yes") (not ANS))
        (progn
          (command "_.layer" "_thaw" "0" "")
          (setq THAW0 T
                DOIT  T
          )
        )
      )
    )
    (setq DOIT T)
  )
 
  (if DOIT
    (progn
      (prompt "\nSelect shape entity to convert: ")
 
      (Setq FLTR    '((0 . "SHAPE"))                   ; Filter for shapes
            GLST     (grplst)                          ; Get all the groups in drawing
            GDICT    (if GLST
                       (dictsearch (namedobjdict) "ACAD_GROUP")
                     )
            SHP      (acet-ui-single-select FLTR T)
      )
 
 
      (if SHP
        (progn
 
          (setq ED   (entget SHP)                      ; Get shape's entity data
                NAM  (cdr (assoc 2 ED))                ; Get shape's name
                CLAY (getvar "clayer")                 ; Store current layer
                LOOP T
          )
 
          (while LOOP
            (setq BLK (getstring (strcat "\nEnter the name of the block to create <" NAM ">: ")))
            (if (= BLK "")
              (setq BLK NAM)
            )
            (setq BLK (xstrcase BLK))
            (cond
              ((not (snvalid BLK))
                (princ "\nInvalid block name.")
              )
              ((tblobjname "BLOCK" BLK)
                (princ (strcat "\nBlock " BLK " already exists."))
                (initget "Yes" 128)
                (if (= (getkword "\nRedefine it? <N>") "Yes")
                  (setq LOOP nil)
                )
              )
              (T
                (setq LOOP nil)
              )
            )
          )
 
          (command "_.ucs" "_view"
                   "_.shape" NAM (getvar "viewctr") 1 0)
 
          (setq SHP (entlast)
               ; PTS (getgeomextents SHP)
               ;replaced with new API call for 2000
                PTS (acet-ent-geomextents SHP)
                PTS (list (trans (car PTS) 0 1) (trans (cadr PTS) 0 1)) ; translate from world to current ucs
                INS (getvar "viewctr")
          )
 
          (if (setq ZM (acet-geom-zoom-for-select PTS)) ; If current view does not contain
            (progn                                                 ; shape inserted
              (setq ZM
                (list
                  (list (- (caar ZM) (acet-geom-pixel-unit))     ; increase zoom area by
                        (- (cadar ZM) (acet-geom-pixel-unit))    ; one pixel width to
                        (caddar ZM)                    ; sure nothing will be lost
                  )
                  (list (+ (caadr ZM) (acet-geom-pixel-unit))
                        (+ (cadadr ZM) (acet-geom-pixel-unit))
                        (caddr (cadr zm))
                  )
                )
              )
              (command "_.zoom" "_w" (car ZM) (cadr ZM))  ; zoom to include shape objects
            )
          )
 
          (setq TMPFIL   (strcat (getvar "tempprefix") "bnsshp.wmf"))
 
          (if (acet-layer-locked (getvar "clayer"))       ; if current layer is locked
            (progn
              (command "_.layer" "_unl" (getvar "clayer") "")  ; unlock it
              (setq CUR_LOCKED T)
            )
          )
 
          (if (acet-layer-locked "0")                      ; if layer 0 is locked
            (progn
              (command "_.layer" "_unl" "0" "")         ; unlock it
              (setq 0_LOCKED T)
            )
          )
 
          (command "_.chprop" SHP """_lt" "continuous" ""
                   "_.WMFOUT" TMPFIL SHP ""
                   "_.ERASE" SHP ""
          )
 
          (setq SHP (acet-wmfin-loc TMPFIL))
 
          (command "_.chprop" SHP ""
                     "_c" "_byblock"
                     "_lt" "_byblock"
                   ""
                   "_.block" BLK INS SHP ""
          )
 
          (if ZM (command "_.zoom" "_p"))              ; Restore original view if needed
 
; find all shapes in drawing and replace them with the block insert
 
          (setq FLTR (list (cons -4  "<AND")
                             (cons 0 "SHAPE")
                             (cons 2 NAM)
                           (cons -4 "AND>")
                     )
                SS   (ssget "_x" FLTR)
                CNT 0
          )
 
          (While (and
                   SS
                   (setq ENT (ssname SS CNT))          ; step through each object in set
                 )
 
            (setq ED  (entget ENT)
                  INS (assoc 10 ED)                    ; Get shape's insertion point
                  SCL (cdr (assoc 40 ED))              ; Get shape's size
                  ROT (assoc 50 ED)                    ; Get shape's rotation
                  LAY (assoc 8 ED)                     ; Get shape's layer
                  CLR (assoc 62 ED)                    ; Get shape's color
                  LTP (assoc 6 ED)                     ; Get shape's linetype
                  SPC (assoc 67 ED)                    ; Get shape's space
                  VEC (assoc 210 ED)                   ; Get shape's extrusion vector
            )
 
            (if (acet-layer-locked (cdr LAY))                   ; if shape's layer is locked
              (progn
                (command "_.layer" "_unl" (cdr LAY) "")      ; unlock it
                (setq ENT_LOCKED T)
              )
            )
 
            (entdel ENT)
 
            (entmake (list
                       (cons 0 "INSERT")
                       (cons 2 BLK)
                       INS
                       (cons 41 SCL)
                       (cons 42 SCL)
                       (cons 43 SCL)
                       ROT
                       LAY
                       (if CLR
                         CLR
                         (cons 62 256)
                       )
                       (if LTP
                         LTP
                         (cons 6 "BYLAYER")
                       )
                       (if SPC
                         SPC
                         (cons 67 0)
                       )
                       VEC
                     )
            )
 
            (if ENT_LOCKED
              (progn
                (command "_.layer" "_lock" (cdr LAY) "")     ; relock if needed
                (setq ENT_LOCKED nil)
              )
            )
 
            (setq CNT (1+ CNT))
 
          )
 
 
          (if CUR_LOCKED (command "_.layer" "_lock" (getvar "clayer") "")) ; relock current if needed
          (if 0_LOCKED (command "_.layer" "_lock" "0" ""))                 ; relock 0 if needed
 
          (setvar "clayer" CLAY)                       ; Return the original layer current
          (command "_.ucs" "_P")
 
          (prompt (strcat "\nThe shape " NAM " has been replaced with block " BLK "."))
        )
      )
      (if THAW0
        (progn
          (command "_.layer" "_freeze" "0" "")
          (prompt "\nLayer 0 has been refrozen.")
        )
      )
    )
  )
 
  (acet-error-restore)                                  ; Retsore values
  (princ)
)


(princ)