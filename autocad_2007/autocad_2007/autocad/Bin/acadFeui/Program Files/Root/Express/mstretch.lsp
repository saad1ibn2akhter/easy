;;
;;;
;;;    MSTRETCH.LSP
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
 
(defun c:mstretch ( / n ss ss2 a b lst lst2 lst3 lst4 flag p1 p2 p3 p4 zflag)
 
(acet-error-init
 (list
  (list   "cmdecho" 0
        "highlight" 0
         "dragmode" (getvar "dragmode")
           "osmode" 0
          "cecolor" "6"
          "celtype" "CONTINUOUS"
         "limcheck" 0
  )
  T
  '(progn
     (acet-temp-segment nil nil nil 1)
     (acet-sysvar-set (list "cmdecho" 0));turn cmdecho off
     (command "_.redraw");do redraw
     (acet-sysvar-restore);re-set cmdecho back
     (princ)
     ;(command "_.redraw")
   )
 )
)
(sssetfirst nil nil)
 
(princ "\nDefine crossing windows or crossing polygons...")
(setvar "highlight" 1)
(setq ss (ssadd))
(command "_.select")
(while (not flag)
 
(if (not lst)
    (progn
     (initget 128 "CP C _CP C")
     (princ "\nOptions: Crossing Polygon or Crossing first point")
     (setq a (getpoint "\nSpecify an option [CP/C] <Crossing first point>: "));setq
    );progn
    (progn
     (initget 128 "CP C Undo _CP C Undo")
     (princ "\nOptions: Crossing Polygon, Crossing first point or Undo")
     (setq a (getpoint "\nSpecify an option [CP/C/Undo] <Crossing first point>: "));setq
    );progn
);if
(cond
 ((or (and (= a "C")
           (progn (initget 1) (setq a (getpoint "\nSpecify first corner: ")))
           (progn (initget 33) (setq b (getcorner a "\nSpecify other corner: ")))
           ;(setq lst2 (acet-geom-rect-points a b));setq
      );and
      (and a
           (equal (type a) 'LIST)
           (progn (initget 33) (setq b (getcorner a "\nSpecify other corner: ")))
           ;(setq lst2 (acet-geom-rect-points a b));setq
      );and
  );or
  (setq  lst (append lst (list (list a b)))
        lst4 (append lst4
                     (list (ssget "_c" a b))
             )
          p3 (trans '(0.0 0.0 0.0) 1 0)
          p4 (trans (getvar "viewdir") 1 0 T);rk added T 4:12 PM 8/12/97
  );setq
 
  (acet-lwpline-make
    (list
        (list (cons 210 p4))
        ;(acet-geom-m-trans (acet-geom-rect-points a b) 1 2)
        (acet-geom-rect-points (trans a 1 p4); p4 was 2
                     (trans b 1 p4)
        )
    );list
  );acet-lwpline-make-make
 
  (command (entlast))
  (setq lst3 (append lst3 (list (entlast))))
 );cond #1
 ((= a "CP")
  (progn
   (if (setq lst2 (acet-ui-polygon-select 1))
       (progn
        (setq lst2 (append lst2 (list (car lst2)))
               lst (append lst (list lst2))
              lst4 (append lst4
                           (list (ssget "_cp" (cdr lst2)))
                   )
                p3 (trans '(0.0 0.0 0.0) 1 0)
                p4 (trans (getvar "viewdir") 1 0 T)
                ;p4 (list (- (car p4) (car p3))
                ;         (- (cadr p4) (cadr p3))
                ;         (- (caddr p4) (caddr p3))
                ;   );list
        );setq
        (acet-lwpline-make
          (list
           (list (cons 210 p4))
           (acet-geom-m-trans
                     lst2
                     1
                     p4 ;rk 2 4:27 PM 8/12/97
           )
          );list
        );acet-lwpline-make-make
 
        (command (entlast))
        (setq lst3 (append lst3 (list (entlast))))
       );progn
   );if
  );progn
 )
 ((and lst                 ;;;;;Undo the last window definition
       (= a "Undo")
  );and
  (command "_r" (last lst3) "_a")
  (if (acet-layer-locked (getvar "clayer"))
      (progn
       (command "")
       (command "_.layer" "_unl" (getvar "clayer") "")
       (entdel (last lst3))
       (command "_.layer" "_lock" (getvar "clayer") "")
       (command "_.select")
       (if (> (length lst3) 1)
           (eval (append '(command)
                          (cdr (reverse lst3))
                 );append
           );eval
       );if
      );progn then the current layer is locked
      (entdel (last lst3))
  );if
  (setq lst3 (reverse (cdr (reverse lst3)))
        lst4 (reverse (cdr (reverse lst4)))
         lst (reverse (cdr (reverse lst)))
  );setq
 )
 ((or (= a "")
      (not a)
  );or
  (setq flag T)
 )
 (T
  (princ "\nInvalid")
 )
);cond
 
);while
(command "");end select
(setvar "highlight" 0)
 
(if lst
    (progn
     (princ "\nDone defining windows for stretch...")
     (if (acet-layer-locked (getvar "clayer"))
         (progn
          (command "_.layer" "_unl" (getvar "clayer") "")
          (setq lst (reverse lst))
          (setq n 0);setq
          (repeat (length lst3)
           (entdel (nth n lst3))
           (setq n (+ n 1));setq
          );repeat
          (command "_.layer" "_lock" (getvar "clayer") "")
         );progn then the current layer is locked
         (progn
          (setq lst (reverse lst))
          (setq n 0);setq
          (repeat (length lst3)
           (entdel (nth n lst3))
           (setq n (+ n 1));setq
          );repeat
         );progn else
     );if
 
     (setvar "highlight" 1)
     (command "_.select")
     (repeat (length lst4)
      (if (car lst4) (command (car lst4)))
      (setq lst4 (cdr lst4))
     );repeat
     (command "")
     (setq ss (ssget "_p"))
 
     (if ss
         (progn
 
          (command "_.select" ss)
          (if (assoc "OSMODE" (car acet:sysvar-list))
              (setvar "osmode" (cadr (assoc "OSMODE" (car acet:sysvar-list))))
          );if
          (setq p1 nil)
          (while (not p1)
           (initget 128 "Remove _Remove")
           (setq p1 (getpoint "\nSpecify an option [Remove objects] <Base point>: "));setq
           (if (not p1) (setq p1 (car (acet-geom-view-points))));if
           (if (and p1
                    (not (equal p1 "Remove"))
                    (not (equal (type p1) 'LIST))
               );and
               (progn
                (setq p1 nil);setq
                (princ "\nInvalid input.")
               );progn then
           );if
          );while
          (command "")
          (if (= p1 "Remove")
              (progn
               (setvar "highlight" 0)
               (acet-ss-clear-prev)
               ;(command "_.select" (entnext) "")
               ;(command "_.undo" "1")
               (setvar "highlight" 1)
 
               (command "_.select" ss "_r" "_auto")
               (setvar "cmdecho" 1)
               (while (wcmatch (getvar "cmdnames") "*SELECT*")
                (command pause)
               );while
               (if (setq ss2 (ssget "_P"));setq
                   (progn
                    (command "_.select" ss2)
                    (setq p1 (getpoint "\nSpecify base point: "));setq
                    (command "")
                    (if (not p1) (setq p1 (car (acet-geom-view-points))));if
                   );progn
               );if
              );progn then
              (setq ss2 ss);else
          );if
          (if ss2
              (progn
               ;;get the extents of the crossing window definitions
               (setq lst2 lst)
 
               (repeat (length lst2)
                (if (> (length (car lst2)) 2)
                    (setq lst2 (append (cdr lst2) (car lst2)))
                    (setq lst2 (cdr lst2))
                );if
               );repeat
 
               (if (and (> (length (car lst)) 2)                 ;;;cp_off_screen?
                        (acet-geom-zoom-for-select (car lst))
                   );and
                   (progn
                    (setvar "cmdecho" 0)
                    (command "_.select" ss2)
                    (setq p2 (getpoint p1 "\nSpecify second base point: "));setq
                    (command "")
                   );progn
                   (progn
                    (setvar "cmdecho" 0)
                    (command "_.stretch")
                    (cp_loop (car lst))
                    (command "_r" ss "_a" ss2 "" p1)
                    (setvar "cmdecho" 1)
                    (princ "\nSecond base point: ")
                    (command pause)
                    (setvar "cmdecho" 0)
                    (setq p2 (getvar "lastpoint"))
                    (setq lst (cdr lst))
                   );progn
               );if
               (if (setq zflag (acet-geom-zoom-for-select lst2))
                   (command "_.zoom" "_w" (car zflag) (cadr zflag))
               );if
 
               (setvar "highlight" 0)
               (setvar "dragmode"  0)
               (setvar "osmode"    0)
               (setq n 0);setq
               (repeat (length lst)
                (setq a (nth n lst));setq
                (command "_.stretch")
                (cp_loop a)
                (command "_r" ss "_a" ss2 "" p1 p2)
                (setq n (+ n 1));setq
               );repeat
               (if zflag (command "_.zoom" "_p"))
              );progn then ss2
              (princ "\nNothing selected")
          );if
         );progn then ss
         (princ "\nNothing selected")
     );if
    );progn then lst
);if
(acet-error-restore)
(princ)
);defun c:mstretch
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cp_loop ( lst / n)
(if (equal (length lst) 2)
    (command "_c" (car lst) (cadr lst))
    (progn
     (command "_cp")
     (setq n 0)
     (repeat (length lst)
     (command (nth n lst))
     (setq n (+ n 1));setq
     );repeat
     (command "")
    );progn
);if
);defun cp_loop


(princ)