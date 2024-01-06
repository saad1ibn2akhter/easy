;;;
;;;    COUNT.LSP
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
 
(defun c:bcount ( / ss flt a n lst)
(acet-error-init
 (list nil T)
);acet-error-init
 
;build a filter of valid block names
(setq lst (acet-table-name-list (list "block" 1 4 16))) ;exclude anonymous and xref blocks
(setq n 0)
(repeat (length lst)
(setq  a (nth n lst)
       a (cons 2 a)
     flt (cons a flt)
);setq
(setq n (+ n 1));setq
);repeat
 
(setq flt (append '((0 . "INSERT")
                    (-4 . "<OR")
                   )
                   flt
                   '((-4 . "OR>"))
          );append
);setq
(acet-ss-clear-prev)
(princ "\nPress Enter to select all or...")
 
(if (setq ss (ssget))
    (setq ss (ssget "_p" flt))
    (setq ss (ssget "_x" flt))
);if
(if ss
    (bns_count ss)
    (princ "\nNo valid objects selected.")
);if
 
(acet-error-restore)
);defun c:count
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_count ( ss / bna lst na e1 n a mx )
 
 
;get a list of all unique block names
(setq mx 1)
(setq n 0)
(repeat (sslength ss)
(setq  na (ssname ss n)
       e1 (entget na)
      bna (cdr (assoc 2 e1))
       mx (max mx (strlen bna))
);setq
(if (not (assoc bna lst))
    (setq lst (cons (cons bna 1) lst))
    (setq   a (cdr (assoc bna lst))
            a (+ a 1)
          lst (subst (cons bna a) (assoc bna lst) lst)
    );setq
);if
(setq n (+ n 1));setq
);repeat
 
(if lst
    (progn
     (setq mx (+ mx 5));setq
     (princ (bns_count_format "Block" "Count" mx))
     (setq a "\n")
     (while (< (strlen a) (+ mx 7))
;;      (setq a (strcat a "-"))
      (setq a (acet-str-format "%1-" a))
     );while
     (princ a)
    );progn then print header
);if
(setq n 0)
(repeat (length lst)
(setq a (nth n lst));setq
 (princ (bns_count_format (car a) (itoa (cdr a)) mx))
(setq n (+ n 1));setq
);repeat
);defun bns_count
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_count_format ( a b mx / )
 
 (while (<= (strlen a) mx)
;;  (setq a (strcat a "."))
  (setq a (acet-str-format "%1." a))
 );while
;; (setq a (strcat "\n" a b))
 (setq a (acet-str-format "\n%1%2" a b))
);defun bns_count_format


(princ)
