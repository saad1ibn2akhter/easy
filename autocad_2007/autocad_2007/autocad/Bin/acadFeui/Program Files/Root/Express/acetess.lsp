;;;
;;;    ACETESS.LSP
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
 
; ACETESS.LSP - Selection set tools.
;
;          Exclude Options -
;               - Offers a set of alternate options for each of the standard
;               selection methods such as W, C, P, F, WP, and CP. These alternate
;               options are the opposites of their standard AutoCAD counterparts.
;               They are prefixed with "EX" meaning _exclusion_.
;                i.e.
;                 For example: "EXC" prompts for a crossing window like "C", but EXC means
;                              select everything in the drawing _EXCEPT_ what falls within
;                              the selected corner points.
;
;               The options are: EXW  - exclusion window
;                                EXC  - exclusion crossing window
;                                EXP  - exclusion previous
;                                EXF  - exclusion fence
;                                EXWP - exclusion window polygon
;                                EXCP - exclusion crossing polygon
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun c:exp ( / ss ss2 na n )
(acet-error-init (list nil nil))
(setq  ss (ssget "_p")
      ss2 (ssget "_x")
);setq
(if ss
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
    (progn
     (if ss2
         (progn
          (princ "\nNo previous selection set. Selecting everthing...")
          (if (not (equal (getvar "cmdnames") ""))
              (command ss2)
              (sssetfirst ss2 ss2)
          );if
         );progn then
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exc ( / ss ss2 na n p1 p2)
 
(acet-error-init (list nil nil))
(if (and (setq ss2 (ssget "_x"));setq
         (setq p1 (getpoint "\nSpecify first corner: "));setq
         (not (initget 32))
         (setq p2 (getcorner p1 "Specify other corner: "));setq
         (or (setq ss (ssget "_c" p1 p2))
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exc
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exw ( / ss ss2 na n p1 p2)
 
(acet-error-init (list nil nil))
(if (and (setq ss2 (ssget "_x"));setq
         (setq p1 (getpoint "\nSpecify first corner: "));setq
         (setq p2 (getcorner p1 "Specify other corner: "));setq
         (or (setq ss (ssget "_w" p1 p2))
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exw
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exf ( / lst ss ss2 na p1 n)
 
(acet-error-init (list nil nil
                      '(progn
                        (acet-sysvar-set (list "cmdecho" 0))
                        (command "_.redraw")
                        (acet-sysvar-restore)
                       )
                )
)
(if (setq ss2 (ssget "_x"));setq
    (progn
     (setq p1 (getpoint "\nSpecify first fence point: "));setq
     (while p1
      (cond
       ((equal (type p1) 'LIST)
        (setq lst (append (list p1) lst));setq
        (if (> (length lst) 1)
            (grdraw (car lst)
                    (cadr lst)
                    7
                    1
            );grdraw
        );if
       );cond #1
       ((equal p1 "Undo")
        (if (> (length lst) 1)
            (grdraw (car lst)
                    (cadr lst)
                    0
            );grdraw
        );if
        (setq lst (cdr lst)
               p1 (car lst)
        );setq
       );cond #2
      );cond
      (if p1
          (progn
           (initget "Undo _Undo" (+ 32 128))
           (setq p1 (getpoint p1 "\nSpecify an option [Undo] <Endpoint of line>: "));setq
          );progn
      );if
     );while
 
     (bns_vlist_undraw lst)
 
     (if (and (setq lst (bns_points_in_view lst))
              (or (setq ss (ssget "_f" lst))
                  (setq ss (ssadd))
              );or
         );and
         (progn
          (setq n 0)
          (repeat (sslength ss)
           (setq na (ssname ss n))
           (if (ssmemb na ss2)
               (setq ss2 (ssdel na ss2));setq then
           );if
           (setq n (+ n 1));setq
          );repeat
          (if (not (equal (getvar "cmdnames") ""))
              (command ss2)
              (sssetfirst ss2 ss2)
          );if
         );progn then
     );if
    );progn then
);if
(acet-error-restore)
);defun c:exf
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:excp ( / ss ss2 lst n na)
 
(acet-error-init (list nil nil
                      '(progn
                        (acet-sysvar-set (list "cmdecho" 0))
                        (command "_.redraw")
                        (acet-sysvar-restore)
                       )
                )
)
(if (and (setq ss2 (ssget "_x"));setq
         (setq lst (acet-ui-polygon-select 1));setq
         (or (setq  ss (ssget "_cp" lst));setq
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:excp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:exwp ( / ss ss2 lst n na)
 
(acet-error-init (list nil nil
                      '(progn
                        (acet-sysvar-set (list "cmdecho" 0))
                        (command "_.redraw")
                        (acet-sysvar-restore)
                       )
                )
)
 
(if (and (setq ss2 (ssget "_x"));setq
         (setq lst (acet-ui-polygon-select 0));setq
         (or (setq  ss (ssget "_wp" lst));setq
             (setq ss (ssadd))
         );or
    );and
    (progn
     (setq n 0)
     (repeat (sslength ss)
     (setq na (ssname ss n))
     (if (ssmemb na ss2)
         (setq ss2 (ssdel na ss2));setq then
     );if
     (setq n (+ n 1));setq
     );repeat
     (if (not (equal (getvar "cmdnames") ""))
         (command ss2)
         (sssetfirst ss2 ss2)
     );if
    );progn then
);if
(acet-error-restore)
);defun c:ewp
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a list of points
;returns a list of points that are on screen.
;
(defun bns_points_in_view ( lst / a b n len lst2 lst3 x x2 y y2 )
 
(setq  len (length lst)
      lst3 (acet-geom-view-points)
       lst (acet-geom-m-trans lst 1 2)
      lst3 (acet-geom-m-trans (acet-geom-view-points) 1 2)
         x (car (car lst3))
        x2 (car (cadr lst3))
         y (cadr (car lst3))
        y2 (cadr (cadr lst3))
);setq
(if (> (length lst) 1)
    (setq lst2 (list (bns_truncate_2_view (car lst) (cadr lst) x y x2 y2)
               );list
    );setq
);if
(setq a (car lst));setq
(setq n 1);setq
(while (< n len)
(setq b (nth n lst)
      b (bns_truncate_2_view b a x y x2 y2)
);setq
(if (not (equal b (last lst2) 0.000001))
    (setq lst2 (append lst2 (list b)));setq
);if
(setq a b);setq
(setq n (+ n 1));setq
);while
(setq b (bns_truncate_2_view b a x y x2 y2));setq
(if (not (equal b (last lst2) 0.000001))
    (setq lst2 (append lst2 (list b)));setq then
);if
 
(setq lst2 (acet-geom-m-trans lst2 2 1));setq
 
lst2
);defun bns_points_in_view
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bns_vlist_undraw ( lst / j )
 (setq j 0)
 (while (< j (- (length lst) 1))
  (grdraw (nth j lst)
          (nth (+ j 1) lst)
          0
  );grdraw
  (setq j (+ j 1));setq
 );while
);defun bns_vlist_undraw


(princ)
