;;;
;;;    TREX.LSP - Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TREX
;Trim and Extend combined!
;Pick to trim and shift+pick to extend.
;
(defun c:trex ( / )
 (acet-error-init
  (list '("cmdecho" 0)
         0 ;0 means place an undo begin and end mark but do not
           ;use undo to back up on an error event.
        '(if ss (acet-ss-redraw ss 4))	;; clear the redraw on any selected objects
  );list
 )
 (acet-trim-extend)
(acet-error-restore)
);defun c:trex
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-trim-extend ( / ss flt flag p1 errno lst n a u ulst endit )
 
(setq flt '((-4 . "<NOT")
             (-4 . "<OR")
              (0 . "INSERT")
              (0 . "ATTDEF")
              (0 . "DIMENSION")
              (0 . "SOLID")
              (0 . "3DSOLID")
              (0 . "3DFACE")
              (0 . "POINT")
              (-4 . "<AND")
               (0 . "POLYLINE")
               (-4 . "&") (70 . 80) ;16 64 / 3dmesh and pface mesh.
              (-4 . "AND>")
              (0 . "TRACE")
              (0 . "SHAPE")
             (-4 . "OR>")
            (-4 . "NOT>")
           )
);setq
 
(acet-trex-print-modes)
(princ "\nSelect cutting/boundary edges or press enter for implied.")
 
(if (setq ss (ssget))
    (progn
     (setq ss (acet-ss-filter
               (list ss
                     (list (list flt
                                 "\n1 object was invalid as a cutting/boundary edge."
                                 "\n%1 objects were invalid as cutting/boundary edges."
                           );list
                     );list
                     T
               );list
              );acet-ss-filter
           ss (car ss)
     );setq
     (if (not ss)
         (setq endit T)
     );if
    );progn then
);if

(while (and (not endit)
            (or
                (progn
                 (acet-ss-redraw ss 3)
                 (initget "Fence Undo Project Edge")
                 (setvar "errno" 0)
                 (setq p1 (entsel "\nPick to trim or Shift+Pick to extend [Project/Edge/Undo]: "));setq
                 (acet-ss-redraw ss 4)
                 p1
                );progn
                (equal (setq errno (getvar "errno"))
                       7
                );equal
            );or
       );and
 (setq flag (acet-sys-shift-down))
 
 (setq u 0)
 
 (cond
  ((equal p1 "Undo")
   (if ulst
       (progn
        (command "_.undo" (car ulst))
        (setq ulst (cdr ulst))
       );progn then
       (princ "\nCommand has been completely undone.")
   );if
   (setq u nil)
  );cond #1
 
  ((or (equal p1 "Project") 
       (equal p1 "Edge")
   );or
   (command "_.trim" "")
   (setvar "cmdecho" 1)
    (command (strcat "_" p1) pause)
   (setvar "cmdecho" 0)
   (acet-safe-command nil T (list "")) ;exit the trim command
   (setq u (+ u 1))
  );cond #2
 
  ((not (equal (getvar "errno") 7))
   (if (equal (type p1) 'LIST)
       (setq p1 (cadr p1))
   );if
   (if flag
       (command "_.extend")
       (command "_.trim")
   );if
   (if ss
       (command ss)
   );if
   (command "")
   (if (equal p1 "Fence")
       (progn
        (command nil)
        (setq u (+ u 1));setq
        (setq lst (acet-ui-fence-select))
        (setq flag (acet-sys-shift-down))
        (if lst
            (progn
             (if flag
                 (command "_.extend")
                 (command "_.trim")
             );if
             (if ss (command ss));if
             (command "" "_F")
             (setq n 0)
             (repeat (length lst)
              (if (setq a (nth n lst))
                  (command a)
              );if
             (setq n (+ n 1));setq
             );repeat
             (command "" "")
             (setq u (+ u 1))
            );progn then
        );if
       );progn then fence
       (progn
        (command p1 "")
        (setq u (+ u 1))
       );progn else point pick
   );if
  );cond #3
 
  ((equal (getvar "errno") 7)
   (princ "\nYou missed! Try again...")
   (setq u nil)  
  );cond #4
 );cond close
 (if (and u
          (> u 0)
     );and
     (setq ulst (cons u ulst))
 );if
);while

(acet-safe-command nil T (list "")) ;exit any active command

 
);defun acet-trim-extend
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;print the status of projmode and edgemode the same wy that trim and extend do.
(defun acet-trex-print-modes ( / a b )
 (setq a (getvar "projmode")
       b (getvar "edgemode")
 );setq
 (cond
  ((equal a 0) (setq a "Projmode = None"))
  ((equal a 1) (setq a "Projmode = UCS"))
  ((equal a 2) (setq a "Projmode = View"))
  (T (setq a ""))
 );cond close
 
 (cond
  ((equal b 0) (setq b "Edgemode = No extend"))
  ((equal b 1) (setq b "Edgemode = Extend"))
  (T (setq b ""))
 );cond close
 
 (setq a (strcat "\n(" a ", " b ")"))
 (princ a)
 
);defun acet-trex-print-modes


(princ)