;;
;;;
;;;    BLOCKQ.LSP
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
 
;;;   DESCRIPTION
;;;
;;;   (listb <block name> <entity type>)
;;;   LISTB walks through the entities in a block definition. It also lets
;;;   you specify only one entity type to report from the definiton. For
;;;   instance, (listb "myblock" "attdef") will display only the attribute
;;;   definitons in the block. To list all of the entities in the block,
;;;   supply a NIL argument for <entity type>, as in (listb "myblock" nil).
;;;
;;;   C:BLOCK? serves as a front-end for LISTB. It lets you either supply a
;;;   block name or pick an insterted block. Then you can specify an entity
;;;   type to search for, or accept the default to list all entities in
;;;   the definition.
;;;
;;;-- listb ------------------------------------------------
;;;   list the entities in a block definition <bname>
;;;
 
 
(defun listb (bname etype / data wait)
 
 
   ;; wait for key press
   ;; if ESC, then stop
   (defun wait ()
      (print data)
      (grread (grread T)); clear the buffer
      (terpri)
      (if (and
             (setq data (entnext (acet-dxf -1 data)))
             (/= 27 (cadr (grread)))
          )
          (setq data (entget data '("*")))
          (setq data nil)
      )
   );wait
 
   ;; begin the main program
   (textscr)
   (prompt "\nPress ESC to exit or any key to continue.")
   (terpri)
;;   (print (setq data (tblsearch "block" bname)))
   (if (setq data (tblsearch "block" bname))
     (print data)
   )
   (terpri)
   (if (setq data (acet-dxf -2 data))   ; get first entity
     (setq data (entget data '("*")))   ; get assoc list
   )
 
;;   (setq data (acet-dxf -2 data)               ; get first entity
;;         data (entget data '("*"))   ; get assoc list
;;   )
   (if etype (setq etype (xstrcase etype)))
   (while data
      (cond
         (etype
            (if (= etype (acet-dxf 0 data))
                (wait)
                (setq data
                   (if (setq data (entnext (acet-dxf -1 data)))
                       (entget data '("*"))
                   )
                )
            );if
         );etype
         (T (wait))
      );cond
   );while
   (princ)
)
;;;
;;;-- c:block? -----------------------------------------------
;;;   display a block definition,
;;;   optionally show only certain components
;;;
(defun c:block? (/ old_err bname etype data)
 (setq old_err *error*)
 (defun *error* ( a / )
  (print a)
  (setq *error* old_err)
  (princ)
 );defun
 
 
   (if (= "" (setq bname
         (getstring "\nEnter block name <Return to select>: ")
       ))
       (if (setq bname (entsel "Select a block: "))
           (if (and
                  (setq  data (entget (car bname)))
                  (or (= "INSERT" (acet-dxf 0 data))
                      (= "DIMENSION" (acet-dxf 0 data))
                  )
               );and
               (setq bname (acet-dxf 2 data))
               (setq bname nil)
           );if
       );if
   );if
   (cond
      (bname
         (if
            (= "" (setq etype
                  (getstring "\nEnter an entity type <Return for all>: ")
            ))
            (setq etype nil)
         );if
         (listb bname etype)
      )
      (T  (print " no block found."))
   );cond
 (setq *error* old_err)
 (princ)
)


(princ)