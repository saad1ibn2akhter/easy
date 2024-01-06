;;
;;;
;;;    BLOCKTOXREF.LSP - Randy Kintzley
;;;
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
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; block replace - replaces one block insert with another.
(defun c:blocktoxref ()
 (acet-error-init 
  (list (list "cmdecho" 0 "highlight" 0 )
        1
  )
 )
 (acet-blocktoxref-ui)
 (acet-error-restore)
);defun c:blocktoxref
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; command line version
(defun c:-blocktoxref ()
 (acet-error-init 
  (list (list "cmdecho" 0 "highlight" 0 "cmddia" 0 "filedia" 0 )
        1
  )
 )
 (acet-blocktoxref-ui)
 (acet-error-restore)
);defun c:-blocktoxref
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BlockToXref
;
(defun acet-blocktoxref-ui ( / bna fna n prg )
 (if (not acet:blocktoxref-block)
     (setq acet:blocktoxref-block "")
 );if
 (if (= "" (getvar "refeditname"))
     (setq bna (acet-ui-table-name-get 
                (list "Select a block to be replaced with an xref" 
                  acet:blocktoxref-block
                  "block"
                  2		 ;; dis-allow xrefs
                  nil		 ;; ssget style filter
                  "acet1509.hlp" ;; help file
                  "blocktoxref"  ;; topic
                );list
               )
     );setq then not in refedit mode
     (acet-alert "Command not allowed while REFEDIT is active.")
 );if
 
 ;;default value for next time.
 (if bna
     (setq acet:blocktoxref-block bna)
 )
 
 ;; set up a default xref name
 (if (and bna 
          (not acet:blocktoxref-xref)
     );and
     (setq acet:blocktoxref-xref (strcat bna ".dwg"))
 );if
 (if (and bna
          (setq fna (ACET-FILE-WRITEDIALOG 
                      "Select an xref file" 
                      acet:blocktoxref-xref
                      "dwg"
                      "Acet:BlocktoXref"
                      1664
                    )
          );setq
     );and
     (progn
      (setq acet:blocktoxref-xref fna)
      (if (progn 
           (initget "Yes No")
           (/= "No" (getkword "\nPurge unreferenced items when finished? <Y>: "))
          )
          (setq prg T)
      );if
      (setq n (acet-block-to-xref bna fna prg))
      (princ (acet-str-format "\n%1 block inserts replaced with xref: %2" n fna))
      (if (> n 0)
          (princ "\nResults may not be apparent until next regen.")
      );if
     );progn then
 );if
);defun acet-blocktoxref-ui
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; block replace - replaces one block insert with another.
(defun c:blockreplace ()
 (acet-error-init
  (list (list "cmdecho" 0 "highlight" 0 )
        1
  )
 )
 (acet-blockreplace-ui)
 (acet-error-restore)
);defun c:blockreplace
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; command line version
(defun c:-blockreplace ()
 (acet-error-init 
  (list (list "cmdecho" 0 "highlight" 0 "cmddia" 0 )
        1
  )
 )
 (acet-blockreplace-ui)
 (acet-error-restore)
);defun c:-blockreplace
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-blockreplace-ui ( / bna bna2 n lk prg lst flag bnames  )
 (if (not acet:blockreplace-blocksearch)
     (setq acet:blockreplace-blocksearch "")
 );if
 (if (= "" (getvar "refeditname"))
     (setq bna (acet-ui-table-name-get
                (list "Select the block to be replaced" 
                  acet:blockreplace-block
                  "block"
                  2		 ;; dis-allow xrefs
                  nil		 ;; ssget style filter
                  "acet1509.hlp" ;; help file
                  "blockreplace"  ;; topic
                );list
               )
     );setq
     (acet-alert "Command not allowed while REFEDIT is active.")
 );if
 (if bna
     (setq acet:blockreplace-blocksearch bna)
 );if
 (if (not acet:blockreplace-blockreplace)
     (setq acet:blockreplace-blockreplace "")
 )
 (while (and (not flag)
             bna
             (setq bna2 (acet-ui-table-name-get
                         (list (acet-str-format "Select a block to replace %1" bna)
                             acet:blockreplace-blockreplace
                             "block"
                             2			;; dis-allow xrefs
                             nil		;; ssget style filter
                             "acet1509.hlp"	;; help file
                             "blockreplace" 	;; topic
                         );list
                        )
            );setq
        );and
   (cond
    ((acet-str-equal bna bna2)
     (acet-alert "Invalid. Block names cannot be equal.")
    )
    ;;Cannot replace bna with a block that references bna
    ((and (setq lst (acet-block-referenced-tables bna2 nil))
          (setq bnames (cdr (assoc 2 lst)))
          (member (xstrcase bna) (mapcar 'xstrcase bnames))
     );and
     (acet-alert (acet-str-format "Invalid. Circular reference. %1 references %2." bna2 bna))
    )
    (T
     (setq flag T)
    )
   );cond close
 );while
 
 (if (and bna bna2);and
     (progn
      (setq acet:blockreplace-blockreplace bna2)
      (if (progn 
           (initget "Yes No")
           (/= "No" (getkword "\nPurge unreferenced items when finished? <Y>: "))
          )
          (setq prg T)
      );if
      (setq lk (acet-layer-unlock-all))
      (if prg
          (setq lst (acet-block-referenced-tables bna nil))
      );if
      (setq n (acet-block-replace bna bna2))
      (if lk
          (command "_.-layer" "_lock" lk "")
      );if
      (if prg
          (acet-block-referenced-tables-purge lst)
      );if
      (princ (acet-str-format "\n%1 blocks replaced." n))
      (if (> n 0)
          (princ "\nResults may not be apparent until next regen.")
      );if
     );progn then
 );if
);defun acet-blockreplace-ui


(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-referenced-tables bna tblst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-referenced-tables-ent e1 tblst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-referenced-tables-purge tblst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-replace bna bna2)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-block-to-xref bna fna prg)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-apply-xref-layer-props lalst origlays laymap)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-copy-layer-props la la2)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-local-layer-name xla loclst)))
(acet-autoload2	'("BLOCKTOXREFSUP.LSP"	(acet-blocktoxref-local-to-xref-layer-map xrefname)))
(princ)
