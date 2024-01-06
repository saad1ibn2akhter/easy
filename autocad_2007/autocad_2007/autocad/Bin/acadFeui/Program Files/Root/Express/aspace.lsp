;;;
;;;    ASPACE.LSP - C:ALIGNSPACE Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;  Align-SPACE
(defun c:alignspace ( / ss2 p1 p2 p3 p4 flag flag2 a)
 
(acet-error-init
 (list (list   "cmdecho" 0
             "regenmode" 1
             "ucsfollow" 0
       );list
       T
 );list
);acet-error-init
 
(cond
 ((not (equal 0 (getvar "tilemode")))
  (princ "\n  Command not allowed unless TILEMODE is set to 0  ")
 );cond #1
 ((and (setq flag (acet-viewport-next-pickable))
       (car flag)
  );and
  (progn
   (command "_.mspace")
   (if (not (equal (car flag) (getvar "cvport")))
       (progn
        (princ "\nPicking in perspective view not allowed...")
        (princ "\nSwitching to next available model space viewport")
        (setvar "cvport" (car flag))
       );progn then
   );if
   (while (not flag2)
    (if (setq p1 (getpoint "\nFIRST alignment point in MODEL space: "))
        (progn
         (setq p2
               (getpoint p1 "\nSECOND point in MODEL space or <Return> for none: ")
         );setq
         (setq p1 (trans p1 1 0))
         (if p2
             (setq p2 (trans p2 1 0))
         )
        );progn then
    );if
    (if (and p1
             (equal p1 p2)
        );and
        (princ "\n*Invalid* Points cannot be the same.")
        (setq flag2 T)
    );if
   );while
   (if p1
       (progn
        (command "_.pspace")
        (setq flag2 nil)
        (while (not flag2)
         (if p2
             (setq a "\nFIRST alignment point in PAPER space: ")
             (setq a "\nAlignment point in PAPER space: ")
         );if
         (if (and (setq p3 (getpoint a))
                  p2
             );and
             (setq p4 (getpoint p3 "\nSECOND alignment point in PAPER space: "))
         );if
         (if (and p3
                  (equal p3 p4)
             );and
             (princ "\n*Invalid* Points cannot be the same.")
             (setq flag2 T)
         );if
        );while
        (setq flag2 nil)
        (command "_.mspace")
        (if (and (setq ss2 (ssget "_x" '((0 . "VIEWPORT") (67 . 1))));setq
                 (= (sslength ss2) 2)
            );and
            (setq flag2 T)
        );if
        (while (not flag2)
         (getstring "\nActivate the desired viewport to align and press ENTER to continue.")
         (if (equal (getvar "cvport")
                    (car (setq flag (acet-viewport-next-pickable)))
             );equal
             (setq flag2 T)
             (progn
              (princ "\n*Invalid*")
              (princ (strcat "\nCannot use viewports that are turned"
                             " off or have perspective view on."
                     )
              );princ
             );progn
         );if
        );while
        (if (or (and p1
                     (not p2)
                     p3
                     (not p4)
                );and
                (and p1 p2 p3 p4);and
            );or
            (alignspace p1 p2 p3 p4)
        );if
       );progn then got 2 model space points
   );if
  );progn
 );cond #2
 ((cadr flag)
  (princ (cadr flag))
 );cond #3
);cond close
 
(acet-error-restore)
(princ)
);defun c:aspace
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes two points in model space (p1 p2) and zooms in the current viewport to align
;it visually woith the two matching point in paper space (p3 p4).
;if p2 and p4 are nil then no view scaling occurrs and only p1 is matched to p3.
;NOTE model space points must be provided in world coords.
;
(defun alignspace ( p1 p2 p3 p4 / vp a b c d na vplocked )
 
(if (= 1 (getvar "cvport"))
    (command "_.mspace")
);if
(setq       na (acet-currentviewport-ename)
      vplocked (acet-viewport-lock-set na nil) ;unlock the viewport if needed.
                                               ;sets vplocked to the ename of viewport if locked/nil if not
);setq
 
(command "_.pspace")
(setq p3 (trans p3 1 3))
(if p4
    (setq p4 (trans p4 1 3))
);if
 
(command "_.mspace")
 
(acet-ucs-cmd (list "_view"))
 
(setq p1 (trans p1 0 1));setq
(if p2
    (setq p2 (trans p2 0 1))
);if
 
(if (not p2)
    (progn
     (setq vp (acet-currentviewport-ename)
           vp (entget vp)
           vp (cdr (assoc 41 vp))
            a (/ vp (getvar "viewsize"))
     );setq
    );progn
    (setq p2 (list (car p2) (cadr p2) (caddr p1)) ;rk added 5:39 PM 9/1/97
          p4 (list (car p4) (cadr p4) (caddr p3))
           a (/ (distance p3 p4)
                (distance p1 p2)
             )
    );setq else
);if
 
(setq c (trans p3 3 2)
      c (trans c 2 1)
);setq
(if p4
    (setq d (trans p4 3 2)
          d (trans d 2 1)
    );setq
);if
(if (and p2
         p4
         (not (equal (angle p1 p2) (angle c d) 0.0001))
    );and
    (progn
     (acet-ucs-cmd (list "_z" (* (- (angle p1 p2) (angle c d))
                              (/ 180.0 pi)
                           )
                   );list
     );acet-ucs-cmd
     (command "_.plan" "_c")
     (acet-ucs-cmd (list "_p"))
    );progn then
);if
(if (and p2 p4)
    (command "_.zoom" (strcat (rtos a 2 6) "xp"))
);if
(setq b (trans p3 3 2)
      b (trans b 2 1)
);setq
(command "_.-pan" p1 b)
 
;restore the original ucs
(acet-ucs-cmd (list "_p"))
 
(if vplocked
    (acet-viewport-lock-set na T) ;re-lock the viewport
)
 
(if (and p2 p4
         (or (not (equal (caddr p1) (caddr p2) 0.0001))
             (not (equal (caddr c) (caddr d) 0.0001))
         );or
    );and
    (progn
     (princ "\nWarning: Selected Points are not parallel to view")
     (princ "\nCommand results may not be obvious")
    );progn
);if
(princ "\nPaper space = Model space")
(princ (strcat "\n"
               (substr "             " 1
                       (max (- 11 (strlen (rtos 1.0))) 0)
               );substr
               (rtos 1.0)
               " = "
               (rtos (/ 1.0 a))
       );strcat
);princ
(princ (strcat "\nCurrent zoom factor = " (rtos a) "xp"))
);defun alignspace


(princ)
