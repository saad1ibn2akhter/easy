;;
;;;
;;;    SSX.LSP
;;;    Copyright � 1999 by Autodesk, Inc.
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
 
;;; DESCRIPTION
;;;                              SSX.LSP
;;;
;;;   "(SSX)" -  Easy SSGET filter routine.
;;;
;;;   Creates a selection set.  Either type "SSX" at the "Command:" prompt
;;;   to create a "previous" selection set or type "(SSX)" in response to
;;;   any "Select objects:" prompt.  You may use the functions "(A)" to add
;;;   entities and "(R)" to remove entities from a selection set during
;;;   object selection.  More than one filter criteria can be used at a
;;;   time.
;;;
;;;   SSX returns a selection set either exactly like a selected
;;;   entity or, by adjusting the filter list, similar to it.
;;;
;;;   The initial prompt is this:
;;;
;;;     Command: ssx
;;;     Select object/<None>: (RETURN)
;;;     >>Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector:
;;;
;;;   Pressing RETURN at the initial prompt gives you a null selection
;;;   mechanism just as (ssx) did in Release 10, but you may select an
;;;   entity if you desire.  If you do so, then the list of valid types
;;;   allowed by (ssget "x") are presented on the command line.
;;;
;;;     Select object/<None>:  (a LINE selected)
;;;     Filter: ((0 . "LINE") (8 . "0") (39 . 2.0) (62 . 1) (210 0.0 0.0 1.0))
;;;     >>Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector:
;;;
;;;   At this point any of these filters may be removed by selecting the
;;;   option keyword, then pressing RETURN.
;;;
;;;     >>Layer name to add/<RETURN to remove>: (RETURN)
;;;
;;;     Filter: ((0 . "LINE") (39 . 2.0) (62 . 1) (210 0.0 0.0 1.0))
;;;     >>Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector:
;;;
;;;   If an item exists in the filter list and you elect to add a new item,
;;;   the old value is overwritten by the new value, as you can have only
;;;   one of each type in a single (ssget "x") call.
;;;
;;;--------------------------------------------------------------------------;
;;;
;;; Find the dotted pairs that are valid filters for ssget
;;; in entity named "ent".
;;;
;;; ssx_fe == SSX_Find_Entity
;;;
 
(defun ssx_fe (/ data fltr ent)
  (setq ent (car (entsel "\nSelect object <None>: ")))
  (if ent
    (progn
      (setq data (entget ent))
      (foreach x '(0 2 6 7 8 39 62 66 210) ; do not include 38
        (if (assoc x data)
          (setq fltr
            (cons (assoc x data) fltr)
          )
        )
      )
      (reverse fltr)
    )
  )
)
;;;
;;; Remove "element" from "alist".
;;;
;;; ssx_re == SSX_Remove_Element
;;;
(defun ssx_re (element alist)
  (append
    (reverse (cdr (member element (reverse alist))))
    (cdr (member element alist))
  )
)
;;;
;;; INTERNAL ERROR HANDLER
;;;
(defun ssx_er (s)                     ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (acet-str-format "\nError: %1" s))
  )
  (if olderr (setq *error* olderr))   ; Restore old *error* handler
  (princ)
)
;;;
;;; Get the filtered sel-set.
;;;
;;;
(defun ssx (/ olderr fltr)
  (gc)                                ; close any sel-sets
  (setq olderr *error*
        *error* ssx_er
  )
  (setq fltr (ssx_fe))
  (ssx_gf fltr)
)
;;;
;;; Build the filter list up by picking, selecting an item to add,
;;; or remove an item from the list by selecting it and pressing RETURN.
;;;
;;; ssx_gf == SSX_Get_Filters
;;;
(defun ssx_gf (f1 / t1 t2 t3 f1 f2)
  (while
    (progn
      (cond (f1 (prompt "\nCurrent filter: ") (prin1 f1)))
      (initget
        "Block Color Entity Flag LAyer LType Pick Style Thickness Vector")
      (setq t1 (getkword
        "\nEnter filter option [Block name/Color/Entity/Flag/LAyer/LType/Pick/Style/Thickness/Vector]: "))
    )
    (setq t2
      (cond
        ((eq t1 "Block")      2)   ((eq t1 "Color")     62)
        ((eq t1 "Entity")     0)   ((eq t1 "LAyer")      8)
        ((eq t1 "LType")      6)   ((eq t1 "Style")      7)
        ((eq t1 "Thickness") 39)   ((eq t1 "Flag" )     66)
        ((eq t1 "Vector")   210)
        (T t1)
      )
    )
    (setq t3
      (cond
        ((= t2  2)  (getstring "\n>>Enter block name to add <RETURN to remove>: "))
        ((= t2 62)  (initget 4 "?")
          (cond
            ((or (eq (setq t3 (getint
              "\n>>Enter color number to add [?] <RETURN to remove>: ")) "?")
              (> t3 256))
              (ssx_pc)                ; Print color values.
              nil
            )
            (T
              t3                      ; Return t3.
            )
          )
        )
        ((= t2  0) (getstring "\n>>Enter entity type to add <RETURN to remove>: "))
        ((= t2  8) (getstring "\n>>Enter layer name to add <RETURN to remove>: "))
        ((= t2  6) (getstring "\n>>Enter linetype name to add <RETURN to remove>: "))
        ((= t2  7)
          (getstring "\n>>Enter text style name to add <RETURN to remove>: ")
        )
        ((= t2 39)  (getreal   "\n>>Enter thickness to add <RETURN to remove>: "))
        ((= t2 66)  (if (assoc 66 f1) nil 1))
        ((= t2 210)
          (getpoint  "\n>>Specify extrusion Vector to add <RETURN to remove>: ")
        )
        (T          nil)
      )
    )
    (cond
      ((= t2 "Pick") (setq f1 (ssx_fe) t2 nil)) ; get entity
      ((and f1 (assoc t2 f1))         ; already in the list
        (if (and t3 (/= t3 ""))
          ;; Replace with a new value...
          (setq f1 (subst (cons t2 t3) (assoc t2 f1) f1))
          ;; Remove it from filter list...
          (setq f1 (ssx_re (assoc t2 f1) f1))
        )
      )
      ((and t3 (/= t3 ""))
        (setq f1 (cons (cons t2 t3) f1))
      )
      (T nil)
    )
  )
  (if f1 (setq f2 (ssget "_x" f1)))
  (setq *error* olderr)
  (if (and f1 f2)
    (progn
      (princ (acet-str-format "\n%1 found. "  (itoa (sslength f2))))
      f2
    )
    (progn (princ "\n0 found.") (prin1))
  )
)
;;;
;;; Print the standard color assignments.
;;;
;;;
(defun ssx_pc ()
  (if textpage (textpage) (textscr))
  (princ "\n                                                     ")
  (princ "\n                 Color number   |   Standard meaning ")
  (princ "\n                ________________|____________________")
  (princ "\n                                |                    ")
  (princ "\n                       0        |      <BYBLOCK>     ")
  (princ "\n                       1        |      Red           ")
  (princ "\n                       2        |      Yellow        ")
  (princ "\n                       3        |      Green         ")
  (princ "\n                       4        |      Cyan          ")
  (princ "\n                       5        |      Blue          ")
  (princ "\n                       6        |      Magenta       ")
  (princ "\n                       7        |      White         ")
  (princ "\n                    8...255     |      -Varies-      ")
  (princ "\n                      256       |      <BYLAYER>     ")
  (princ "\n                                               \n\n\n")
)
;;;
;;; C: function definition.
;;;
(defun c:ssx ()
 (ssx)
 (princ)
)
(princ "\n\tType \"ssx\" at a Command: prompt or ")
(princ "\n\t(ssx) at any object selection prompt. ")


(princ)
