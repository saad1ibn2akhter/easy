;;;
;;;
;;;    BURST.LSP
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
 
(Defun C:BURST (/ item bitset bump att-text lastent burst-one burst
                  BCNT BLAYER BCOLOR ELAST BLTYPE ETYPE PSFLAG ENAME )
 
   ;-----------------------------------------------------
   ; Item from association list
   ;-----------------------------------------------------
   (Defun ITEM (N E) (CDR (Assoc N E)))
   ;-----------------------------------------------------
   ; Error Handler
   ;-----------------------------------------------------
 
  (acet-error-init
    (list
      (list "cmdecho" 0
            "highlight" 1
      )
      T     ;flag. True means use undo for error clean up.
    );list
  );acet-error-init
 
 
   ;-----------------------------------------------------
   ; BIT SET
   ;-----------------------------------------------------
 
   (Defun BITSET (A B) (= (Boole 1 A B) B))
 
   ;-----------------------------------------------------
   ; BUMP
   ;-----------------------------------------------------
 
   (Setq bcnt 0)
   (Defun bump (prmpt)
      (Princ
         (Nth bcnt '("\r-" "\r\\" "\r|" "\r/"))
      )
      (Setq bcnt (Rem (1+ bcnt) 4))
   )
 
   ;-----------------------------------------------------
   ; Convert Attribute Entity to Text Entity
   ;-----------------------------------------------------
 
   (Defun ATT-TEXT (AENT / TENT ILIST INUM)
      (Setq TENT '((0 . "TEXT")))
      (ForEach INUM '(8
            6
            38
            39
            62
            67
            210
            10
            40
            1
            50
            41
            51
            7
            71
            72
            73
            11
            74
         )
         (If (Setq ILIST (Assoc INUM AENT))
            (Setq TENT (Cons ILIST TENT))
         )
      )
      (Setq
         tent (Subst
                 (Cons 73 (item 74 aent))
                 (Assoc 74 tent)
                 tent
              )
      )
      (EntMake (Reverse TENT))
   )
 
   ;-----------------------------------------------------
   ; Find True last entity
   ;-----------------------------------------------------
 
   (Defun LASTENT (/ E0 EN)
      (Setq E0 (EntLast))
      (While (Setq EN (EntNext E0))
         (Setq E0 EN)
      )
      E0
   )
 
   ;-----------------------------------------------------
   ; See if a block is explodable. Return T if it is, 
   ; otherwise return nil
   ;-----------------------------------------------------
 
   (Defun EXPLODABLE (BNAME / B expld)
      (setq BLOCKS (vla-get-blocks 
                     (vla-get-ActiveDocument (vlax-get-acad-object)))
       )
      
      (vlax-for B BLOCKS (if (and (= :vlax-false (vla-get-islayout B))
                                  (= (strcase (vla-get-name B)) (strcase BNAME)))
                      (setq expld (= :vlax-true (vla-get-explodable B)))
           )
       )
       expld
    )


   ;-----------------------------------------------------
   ; Burst one entity
   ;-----------------------------------------------------
 
   (Defun BURST-ONE (BNAME / BENT ANAME ENT ATYPE AENT AGAIN ENAME
                     ENT BBLOCK SS-COLOR SS-LAYER SS-LTYPE mirror ss-mirror
                     mlast)
      (Setq
         BENT   (EntGet BNAME)
         BLAYER (ITEM 8 BENT)
         BCOLOR (ITEM 62 BENT)
         BBLOCK (ITEM 2 BENT)
         BCOLOR (Cond
                   ((> BCOLOR 0) BCOLOR)
                   ((= BCOLOR 0) "BYBLOCK")
                   ("BYLAYER")
                )
         BLTYPE (Cond ((ITEM 6 BENT)) ("BYLAYER"))
      )
      (Setq ELAST (LASTENT))
      (If (and (EXPLODABLE BBLOCK) (= 1 (ITEM 66 BENT)))
         (Progn
            (Setq ANAME BNAME)
            (While (Setq
                      ANAME (EntNext ANAME)
                      AENT  (EntGet ANAME)
                      ATYPE (ITEM 0 AENT)
                      AGAIN (= "ATTRIB" ATYPE)
                   )
               (bump "Converting attributes")
               (ATT-TEXT AENT)
            )
         )
      )
         (Progn
            (bump "Exploding block")
            (acet-explode BNAME)
            ;(command "_.explode" bname)
         )
      (Setq
         SS-LAYER (SsAdd)
         SS-COLOR (SsAdd)
         SS-LTYPE (SsAdd)
         ENAME    ELAST
      )
      (While (Setq ENAME (EntNext ENAME))
         (bump "Gathering pieces")
         (Setq
            ENT   (EntGet ENAME)
            ETYPE (ITEM 0 ENT)
         )
         (If (= "ATTDEF" ETYPE)
            (Progn
               (If (BITSET (ITEM 70 ENT) 2)
                  (ATT-TEXT ENT)
               )
               (EntDel ENAME)
            )
            (Progn
               (If (= "0" (ITEM 8 ENT))
                  (SsAdd ENAME SS-LAYER)
               )
               (If (= 0 (ITEM 62 ENT))
                  (SsAdd ENAME SS-COLOR)
               )
               (If (= "BYBLOCK" (ITEM 6 ENT))
                  (SsAdd ENAME SS-LTYPE)
               )
            )
         )
      )
      (If (> (SsLength SS-LAYER) 0)
         (Progn
            (bump "Fixing layers")
            (Command
               "_.chprop" SS-LAYER "" "_LA" BLAYER ""
            )
         )
      )
      (If (> (SsLength SS-COLOR) 0)
         (Progn
            (bump "Fixing colors")
            (Command
               "_.chprop" SS-COLOR "" "_C" BCOLOR ""
            )
         )
      )
      (If (> (SsLength SS-LTYPE) 0)
         (Progn
            (bump "Fixing linetypes")
            (Command
               "_.chprop" SS-LTYPE "" "_LT" BLTYPE ""
            )
         )
      )
   )
 
   ;-----------------------------------------------------
   ; BURST MAIN ROUTINE
   ;-----------------------------------------------------
 
   (Defun BURST (/ SS1)
      (setq PSFLAG (if (= 1 (caar (vports)))
                       1 0
                   )
      )
      (Setq SS1 (SsGet (list (cons 0 "INSERT")(cons 67 PSFLAG))))
      (If SS1
         (Progn
            (Setvar "highlight" 0)
            (terpri)
            (Repeat
               (SsLength SS1)
               (Setq ENAME (SsName SS1 0))
               (SsDel ENAME SS1)
               (BURST-ONE ENAME)
            )
            (princ "\n")
         )
      )
   )
 
   ;-----------------------------------------------------
   ; BURST COMMAND
   ;-----------------------------------------------------
 
   (BURST)
 
  (acet-error-restore)
 
);end defun


(princ)