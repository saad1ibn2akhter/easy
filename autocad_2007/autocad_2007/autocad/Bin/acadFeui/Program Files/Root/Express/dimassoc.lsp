;;;
;;;    DIMASSOC.LSP
;;;    Created 8/17/98 by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:dimreassoc ( / ss )
(acet-error-init
 (list nil
       T
 );list
);acet-error-init
(princ "\nSelect dimension objects with non-associative text.")
(if (and (setq ss (ssget '((0 . "DIMENSION")
                           (-4 . "<AND")
                            (-4 . "<NOT") (1 . "") (-4 . "NOT>")     ;get dimensions with non-assoc text
                            (-4 . "<NOT") (1 . "*<>*") (-4 . "NOT>") ;
                           (-4 . "AND>")
                          )
                  );ssget
         );setq
         (setq ss (car (bns_ss_mod ss 3 T)));setq dis-allow locked layers and non-current space
    );and
    (progn
     (acet-dimreassoc ss)
     (princ (strcat "\n" (itoa (sslength ss)) " objects modified."))
    );progn then
    (princ "\nNo valid objects selected.")
);if
(acet-error-restore)
);defun c:dimreassoc
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-dimreassoc ( ss / n na e1 )
 
(setq n 0)
(repeat (sslength ss)
(setq na (ssname ss n)
      e1 (entget na '("*"))
      e1 (subst '(1 . "") (assoc 1 e1) e1)
);setq
(entmod e1)
(entupd na)
(setq n (+ n 1));setq
);repeat
 
);defun acet-dimreassoc


(princ)