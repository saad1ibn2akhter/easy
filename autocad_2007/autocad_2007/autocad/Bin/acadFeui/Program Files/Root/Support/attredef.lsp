; Next available MSG number is    13 
; MODULE_ID ATTREDEF_LSP_
;;;
;;;    attredef.lsp
;;;
;;;    Copyright 1988-2003 by Autodesk, Inc.
;;;
;;;    Permission to use, copy, modify, and distribute this software
;;;    for any purpose and without fee is hereby granted, provided
;;;    that the above copyright notice appears in all copies and
;;;    that both that copyright notice and the limited warranty and
;;;    restricted rights notice below appear in all supporting
;;;    documentation.
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
;;;.
;;;
;;; DESCRIPTION
;;;
;;;   This program allows you to redefine a Block and update the
;;;   Attributes associated with any previous insertions of that Block.
;;;   All new Attributes are added to the old Blocks and given their
;;;   default values. All old Attributes with equal tag values to the new
;;;   Attributes are redefined but retain their old value. And all old
;;;   Attributes not included in the new Block are deleted.
;;;
;;;   Note that if handles are enabled, new handles will be assigned to
;;;   each redefined block.
;;;
;;; --------------------------------------------------------------------------;
(vl-load-com)
;;;
;;; Oldatts sets "old_al" (OLD_Attribute_List) to the list of old Attributes
;;; for each Block.  The list does not include constant Attributes.
;;;
(defun oldatts (/ e_name e_list cont)
  (setq oa_ctr 0 
        cont   T
        e_name b1
  )
  (while cont
    (if (setq e_name (entnext e_name))
      (progn
        (setq e_list (entget e_name))
        (if (and (= (cdr (assoc 0 e_list)) "ATTRIB")
                 ;; NOT a constant attribute -- (cdr (assoc 70 e_list)) != 2)
                 (/= (logand (cdr (assoc 70 e_list)) 2) 2))
          (progn
            (if old_al
              (setq old_al (cons e_list old_al))
              (setq old_al (list e_list))
            )
            (setq oa_ctr (1+ oa_ctr))           ; count the number of old atts
          )
          ;; else, exit
          (setq cont nil)
        )
      )
      (setq cont nil)
    )
  )
)
;;;
;;; Newatts sets "new_al" to the list of new Attributes in the new Block.
;;; The list does not include constant Attributes.
;;;
(defun newatts (ssetn ssl / i e_name e_list)
  (setq i 0 na_ctr 0)
  (while (< i ssl)
    (if (setq e_name (ssname ssetn i))
      (progn
        (setq e_list (entget e_name))
        (if (and (= (cdr (assoc 0 e_list)) "ATTDEF")
                 ;; NOT a constant attribute -- (cdr (assoc 70 e_list)) != 2)
                 (/= (logand (cdr (assoc 70 e_list)) 2) 2))
          (progn
            (if new_al
              (setq new_al (cons e_list new_al))
              (setq new_al (list e_list))
            )
            (setq na_ctr (1+ na_ctr))     ; count the number of new atts
          )
        )
      )
    )
    (setq i (1+ i))
  )
  na_ctr
)
;;;
;;; Compare the list of "old" to the list of "new" Attributes and make
;;; the two lists "same" and "preset". "Same" contains the old values of
;;; all the Attributes in "old" with equal tag values to some Attribute
;;; in "new" and the default values of all the other Attributes. "Preset"
;;; contains the preset Attributes in old with equal tag values to some
;;; Attribute in new.
;;;
(defun compare (/ i j)
  (setq i 0
        j 0
        pa_ctr 0
        same nil
        va_ctr 0
        preset nil)
  ;; "i" is a counter that increments until the number of new attributes
  ;; is reached.
  (while (< i na_ctr)
    (cond 
      ;; If there are old attributes AND the tag strings of the old and new 
      ;; attributes are the same...
      ((and old_al
            (= (cdr (assoc 2 (nth j old_al))) (cdr (assoc 2 (nth i new_al)))))
        ;; IS a preset attribute -- (cdr (assoc 70 e_list)) == 8)
        (if (= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
          ;; If the attribute is a preset attribute then add it to the list
          ;; of preset attributes and increment the counter "pa_ctr".
          ;; IS a preset attribute -- (cdr (assoc 70 e_list)) == 8)
          (progn
            (if preset
              (setq preset (cons (nth j old_al) preset))
              (setq preset (list (nth j old_al)))
            )
            (setq pa_ctr (1+ pa_ctr))     ; count preset atts
          )
          ;; Else, add it to the list of same attributes "same".
          (if same
            (setq same (cons (cdr (assoc 1 (nth j old_al))) same))
            (setq same (list (cdr (assoc 1 (nth j old_al)))))
          )
        )
        ;; If the attribute must be verified, increment counter "va_ctr".
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (and (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
                 ;; IS a verified attribute -- (cdr (assoc 70 e_list)) == 4)
                 (= (logand (cdr (assoc 70 (nth i new_al))) 4) 4))
          (setq va_ctr (+ 1 va_ctr))
        )
        (setq i (1+ i))
        (setq j 0)
      )
      ;; If the number of old attributes equals the old attribute counter "j"
      ((= j oa_ctr)
        ;; If this attribute is not a preset attribute, but is not in the 
        ;; old list, then add it to the list "same".
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
          (if same
            (setq same (cons (cdr (assoc 1 (nth i new_al))) same))
            (setq same (list (cdr (assoc 1 (nth i new_al)))))
          )
        )
        ;; NOT a preset attribute -- (cdr (assoc 70 e_list)) != 8)
        (if (and (/= (logand (cdr (assoc 70 (nth i new_al))) 8) 8)
                 ;; IS a verified attribute -- (cdr (assoc 70 e_list)) == 4)
                 (= (logand (cdr (assoc 70 (nth i new_al))) 4) 4))
          (setq va_ctr (+ 1 va_ctr))
        )
        (setq i (1+ i))
        (setq j 0)
      )
      ;; Increment the old attribute counter "j"...
      (t
        (setq j (1+ j))
      )
    )
  )
)
;;;
;;; Find the entity for each of the "preset" Attributes in the newly
;;; inserted Block.
;;;
(defun findpt ()
  (setq test T)
  (setq en (entnext e1))
  (setq e_list (entget en))
  (while test
    (if (and (= (cdr (assoc 0 e_list)) "ATTRIB") (= (cdr (assoc 2 e_list)) tag))
      (setq test nil)
      (progn
        (setq ex en)
        (setq en (entnext ex))
        (if e_list
          (setq e_list (entget en))
        )
      )
    )
  )
)
;;;
;;; Insert a new Block on top of each old Block and set its new Attributes
;;; to their values in the list "same". Then replace each of the "preset"
;;; Attributes with its old value.
;;;
(defun redef (/ xsf ysf zsf ls i e1 v blkref refSpace curTilemode curVport)
  (setq blkref (entget b1))
  (setq xsf (cdr (assoc 41 blkref))) ; find x scale factor
  (setq ysf (cdr (assoc 42 blkref))) ; find y scale factor
  (setq zsf (cdr (assoc 43 blkref))) ; find z scale factor
  (setq refSpace (cdr (assoc 67 blkref)))
  (setq ls (length same))
  (setq i 0)
  ;; switch spaces to that of the block reference, if necessary
  (setq curVport (getvar "CVPORT")
        curTilemode (getvar "TILEMODE"))
  ;; switch to tilemode on, if necessary
  (if (and (= refSpace 0) (= curTilemode 0))(setvar "TILEMODE" 1))
  ;; switch to tilemode off, if necessary
  (if (and (= refSpace 1) (= curTilemode 1))(setvar "TILEMODE" 0))
  ;; switch to paper space, if necessary
  (if (and (= refSpace 1) (/= curVport 1))(command "_.PSPACE"))
  (command "_.UCS" "_E" b1)  ; define the block's UCS

  ;; is block set for uniform scaling only?
  (vlax-for b (vla-get-blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
    (if (and (= (vla-get-islayout b) :vlax-false)
             (= (strcase (vla-get-name b)) bn))
      	(setq notscalable (= 1 (vla-get-blockscaling b))))
   )
      
  (if (= T notscalable)
    (command "_.-INSERT" bn "0.0,0.0,0.0" xsf "0.0")
    (command "_.-INSERT" bn "0.0,0.0,0.0" 
      "_XYZ" xsf ysf zsf "0.0")
   )
  
  (while (< i ls)                     ; set attributes to their values
    (command (nth i same))
    (setq i (1+ i))
  )
  (while (< 0 va_ctr)
    (command "")                      ; at prompts, verify attributes
    (setq va_ctr (1- va_ctr))
  )
  (setq i 0)
  (setq e1 (entlast))
  (while (< 0 pa_ctr)                    ; edit each of the "preset" attributes
    (setq tag (cdr (assoc 2 (nth i preset))))
    (setq v (cdr (assoc 1 (nth i preset))))
    (findpt)                          ; find the entity to modify
    (setq e_list (subst (cons 1 v) (assoc 1 e_list) e_list))
    (entmod e_list)                        ; modify the entity's value
    (setq i (1+ i))
    (setq pa_ctr (1- pa_ctr))
  )
  (command "_.UCS" "_P")           ; restore the previous UCS
  ;; restore the current tilemode and space
  (if (/= curTilemode (getvar "TILEMODE"))
      (setvar "TILEMODE" curTilemode)
  )
  (if (and (= curTilemode 0)
           (/= curVport (getvar "CVPORT")))
    (command "_.MSPACE")
  )
)
;;;
;;; System variable save
;;;
(defun modes (a)
  (setq mlst '())
  (repeat (length a)
    (setq mlst (append mlst (list (list (car a) (getvar (car a))))))
    (setq a (cdr a)))
)
;;;
;;; System variable restore
;;;
(defun moder ()
  (repeat (length mlst)
    (setvar (caar mlst) (cadar mlst))
    (setq mlst (cdr mlst))
  )
)
;;;
;;; Internal error handler
;;;
(defun attrerr (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= s "Function cancelled")
    (princ (strcat "\nError: " s))
  )
  (moder)                             ; restore saved modes
  ; Terminate the undo grouping
  (command "_.UNDO" "_END")
  ; Restore CMDECHO without undo recording
  (ai_setCmdEcho _attdef_oldCmdEcho)
  
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)
;;;
;;; Main program
;;;
(defun C:ATTREDEF (/ k n olderr bn sseto ssetn pt ssl new_al
                     old_al same preset b1 oa_ctr va_ctr na_ctr
                  ) 
  (setq k 0
      n 0
      test T
      olderr *error*
      *error* attrerr
  )

  (if (= (getvar "blockeditor") 1) 
	(progn
		(defun *error* (s)
		    (if olderr (setq *error* old_error))
		    (princ)
		)
		(prompt "\n** ATTREDEF command not allowed in block editor. **\n")
		(exit)
	)
  )
  
  ;;;
  ;;; Prompt for (uppercase) block-name pattern
  ;;;
  (defun getucsymbolstring (p / resp)
    (setq resp ; raw response
      (getstring
        (if (or (eq (getvar "EXTNAMES") 0)
                (eq (logand (getvar "CMDACTIVE") 4) 4))
          nil 1)
        p))
    (if (wcmatch resp "\"*\"")
      (setq resp (substr resp 2 (- (strlen resp) 2))))
    (xstrcase (ai_strtrim resp))
  )

  (modes '("ATTDIA" "ATTREQ" "GRIDMODE"
     "UCSFOLLOW"))

  (setq _attdef_oldCmdEcho (getvar "CMDECHO"))
  ; set CMECHO without undo recording
  (ai_setCmdEcho 0)

  (command "_.UNDO" "_GROUP")
  (setvar "attdia" 0)                 ; turn attdia off
  (setvar "attreq" 1)                 ; turn attreq on
  (setvar "gridmode" 0)               ; turn gridmode off
  (setvar "ucsfollow" 0)              ; turn ucsfollow off  

  (while 
    (progn
      (setq bn (getucsymbolstring
          "\nEnter name of the block you wish to redefine: "))
      (if (tblsearch "block" bn)
        (progn
          (setq sseto (ssget "_x" (list (cons 0 "INSERT") (cons 2 bn))))
          (setq test nil)
        )
        (progn
          (princ "\nBlock \"")
          (princ bn)
          (princ "\" is not defined. Please try again.\n")
        )
       )
    )
  )
  (if sseto
    (progn
      ;; Filter out references on blocked layers
      (while (< k (sslength sseto))
        ;; get reference's layer name and get the layer's flags
        ;; to check if it's on a locked layer.
        (if (= 4 (logand 4 (cdr (assoc '70 (tblsearch "LAYER" 
                           (cdr (assoc  '8 (entget (ssname sseto k)))))))))
           (setq sseto (ssdel (ssname sseto k) sseto))
           (setq k (1+ k)) ; else: step to next item in the set
        )
      )
      (setq k 0) ; reset selection set index to start position
      (while 
        (progn
          (princ "\nSelect objects for new Block... ")
          (if (null (setq ssetn (ssget "_:l")))
            (princ "\nNo new Block selected. Please try again.")
            (setq test nil)
          )
        )
      )
      ;; find the list of new attributes
      (setq na_ctr (newatts ssetn (sslength ssetn)) )
      (if (> na_ctr 0)
        (progn
          (initget 1)
          (setq pt (getpoint "\nSpecify insertion base point of new Block: "))
          (setq ssl (sslength sseto))
          ;; redefine the block
          (command "_.-BLOCK" bn "_Y" pt ssetn "") 
          (while (< k ssl)
            (setq b1 (ssname sseto k))    ; For each old block...
            (setq old_al nil)
            (oldatts)                     ; find the list of old attributes,
            (compare)                     ; compare the old list with the new,
            (redef)                       ; and redefine its attributes.
            (entdel b1)                   ; delete the old block.
            (setq k (1+ k))
          )
          ; Regen is no longer necessary, as the -BLOCK command now
          ; regens all affected blocks.         
          ; (command "_.REGENALL")
        )
        (princ "\nNew block has no attributes. ")
      )
    )
    (princ (strcat "\nNo insertions of block " bn " found to redefine. "))
  )
  (moder)                             ; restore saved modes
  (command "_.UNDO" "_END")
  ; Restore CMDECHO without undo recording
  (ai_setCmdEcho _attdef_oldCmdEcho)
  (setq *error* olderr)               ; restore old *error* handler
  (princ)
)


(defun ai_abort (app msg)
   (defun *error* (s)
      (if old_error (setq *error* old_error))
      (princ)
   )
   (if msg
     (alert (strcat " Application error: "
                    app " \n\n  " msg "  \n"))
   )
   (exit)
)

;;; Check to see if AI_UTILS is loaded, If not, try to find it,
;;; and then try to load it.
;;;
;;; If it can't be found or it can't be loaded, then abort the
;;; loading of this file immediately, preserving the (autoload)
;;; stub function.

(cond
   ((and ai_dcl (listp ai_dcl)))          ; it's already loaded.
   ((not (findfile "ai_utils.lsp"))                     ; find it
      (ai_abort "ATTREDEF"
                (strcat "Can't locate file AI_UTILS.LSP."
                        "\n Check support directory.")))

   ((eq "failed" (load "ai_utils" "failed"))   ; load it
    (ai_abort "ATTREDEF"
              " Can't load file AI_UTILS.LSP"))
)

(defun c:at () (c:attredef))
(princ 
"\nC:ATtredef loaded.  Start command with AT or ATTREDEF.")
(princ)
