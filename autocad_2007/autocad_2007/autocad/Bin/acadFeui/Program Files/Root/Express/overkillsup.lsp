;;
;;  OverkillSup.lsp
;;                    
;;
;;  Copyright © 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; over-kill - delete overlaping and un-needed entities
;; Takes single list of arguments:
;;  ss           - selection set
;;  fuz          - for numeric comparisons
;;  ignore       - (optional) list of group codes specifying which common group codes to ignore 
;;                 when comparing entities.
;;  no-plines    - (optional) flag - T means do NOT optimize segments within plines.
;;  no-partial   - (optional) flag - T means do NOT combine parallel segments that partially overlap
;;  no-endtoend  - (optional) flag - T means do NOT combine parallel segments that are end to end.
;;
(defun acet-overkill2 ( alst / lst ss fuz ignore no-plines no-partial no-EndtoEnd
                              ss2 n na plst na2 vlst j k
                     )
 
; (acet-autoload '("pljoin.lsp" "(acet-pljoin ss st fuz)"))
 
 
 ;; extract the arguments from the arg list
 (setq lst '(ss fuz ignore no-plines no-partial no-EndtoEnd))
 (setq n 0)
 (repeat (min (length alst) (length lst))
  (set (nth n lst) (nth n alst))
 (setq n (+ n 1));setq
 );repeat
 (setq lst nil)
 
 
 (acet-sysvar-set 
  '("highlight" 0 
      "ucsicon" 0 
    "pickstyle" 0 
       "osmode" 0
   )
 )
 
 (if (not no-plines)
     (progn
      ;; Break plines down to individual objects and re-assemble what's left over later
      (setq plst (acet-plines-explode ss)
              ss (car plst)			;; new selection set with plines removed and new objects added
            plst (cadr plst)			;; data used to re-build the plines later
      );setq
     );progn then ok to optimize plines
 );if
 
 ;; Delete the perfect matches first
 (setq ss2 (acet-ss-remove-dups ss fuz ignore)
        ss (car ss2)
       ss2 (cadr ss2)
 );setq
 (if ss2
     (progn
      ;(command "_.erase" ss2 "")
      (princ (acet-str-format "\n%1 duplicate(s) deleted.\n" (itoa (sslength ss2))))
     );progn then
     (setq ss2 (ssadd));setq else create an empty selection set
 );if
 
 (if (not (and no-partial	; don't do overlappers and don't do endtoend means exact 
               no-endtoend      ; dups only so we're done if both of these are true
          )
     );not 
     (progn
      (setq vlst (acet-overkill-ss->primitives2 ss 0.0000001 ignore)
               j 0
      );setq then ok to combine at least some parallel segments
     );progn then
 );if
 
 (acet-ui-progress-init "Optimizing objects" (length vlst))
 (setq n 1)
 (foreach lst vlst
   (if (> (length lst) 2)
       (progn
        (if (= 0 (car (car lst)))
            (setq k (acet-overkill-resolve-lines2 lst ss2 fuz no-partial no-endtoend)); lines
            (setq k (acet-overkill-resolve-arcs2 lst ss2 fuz no-partial no-endtoend)); arcs
        );if
        (setq ss2 (cadr k)
                k (car k)
                j (+ j k)
        );setq
        (princ "                                                      ")
        (princ "\r")
        (princ (acet-str-format "%1 object(s) deleted." (itoa j)))
       );progn then more than one object in the list
   );if
  (acet-ui-progress-safe n)
  (setq n (+ n 1));setq
 );foreach list of potential over-lapers 
 (acet-ui-progress-done)
 
 (setq na (entlast))
 
 (if (and ss2
          (> (sslength ss2) 0)
     );and
     (acet-ss-entdel ss2) ;then delete this stuff before pline re-build
 );if
 
 (if plst
     (acet-plines-rebuild plst)
 );if
 
 (if (and ss2
          (> (sslength ss2) 0)
     );and
     (progn
      (acet-ss-entdel ss2)				;; bring it back and then use erase 
      (acet-safe-command T T (list "_.erase" ss2 ""))   ;; so that can be oops'd back
     );progn then
     (acet-ss-clear-prev)
 );if
 
 (acet-sysvar-restore)
 (princ "\n")
 ss2
);defun acet-overkill
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check the provided list potential over-lappers and resolve any that are found.
;;
;;
;; Arrange each line so points are drawn left to right (or bottom to top for vertical)
;; Sort the list
;; Modify the first element to stretch past any overlapping objects and delete the overlappers.
;; If an element's lowest point is not less that the highest point so far then make that element 
;; the new stretcher element.
;;
(defun acet-overkill-resolve-lines2 ( lst ss2 fuz no-partial no-endtoend / 
                                     index m m2 n x na na2 p1 p2 p3 p4 mod j a b e1
                                   )
 
 (setq   a (car lst)
       lst (cdr lst)
         m (nth 1 a)	;; xy slope
        m2 (nth 3 a)	;; yz slope
 );setq
 
 ;; if the lines are not vertical then set index x else set it to y
 (cond
  (m (setq index 0)) 	;; slope is defined in xy plane so use x coord
  (m2 (setq index 1))	;; slope is defined in yz plane so use y coord
  (T (setq index 2))    ;; the lines is parallel to the z axis so use the z coord.
 );cond close
 
;(print a)
;(print lst)
;(print index)
;(getstring "hey")
 
 ;; Get the lines in a left to right configuration
 ;; then sort the list of lines from left to right
 ;;
 (setq lst (mapcar '(lambda ( x / a b )
                      (if (< (nth index (car x)) 
                             (nth index (cadr x))
                          )
                          (setq a (car x) 
                                b (cadr x)
                          );setq then
                          (setq b (car x) 
                                a (cadr x)
                          );setq else
                      );if
                      (list a b (caddr x))
                    )
                    lst
           );mapcar
       lst (vl-sort lst
                    '(lambda (a b) 
                      (< (nth index (car a)) (nth index (car b)))
                     )
           );vl-sort
         x (car lst)
        p1 (car x)
        p2 (cadr x)
        na (caddr x)
         j 0
 );setq
 
 (setq n 1)
 (repeat (- (length lst) 1)
 (setq   x (nth n lst)
        p3 (car x)
        p4 (cadr x)
       na2 (caddr x)
 );setq
 (cond
  ((equal (nth index p3) (nth index p2) fuz)
   (if (not no-endtoend)
       (progn
        (if (> (nth index p4) (nth index p2))
            (setq  p2 p4
                  mod T
            );setq then partial overlap
        );if
        (setq ss2 (ssadd na2 ss2))
        ;(entdel na2)
        (setq j (+ j 1))
       );progn then ok to combine endtoend
   );if
  );cond #1 end to end
 
  ((< (nth index p3) (nth index p2))
   (if (not no-partial)
       (progn
        (if (> (nth index p4) (nth index p2))
            (setq  p2 p4
                  mod T
            );setq then partial overlap
        );if
        (setq ss2 (ssadd na2 ss2))
        ;(entdel na2)
        (setq j (+ j 1))
       );progn then ok to combine partially overlaping objects
   );if
  );cond #2 overlap-age
  (T
   (if mod
       (progn
        (setq e1 (entget na)
              e1 (subst (cons 10 p1) (assoc 10 e1) e1)
              e1 (subst (cons 11 p2) (assoc 11 e1) e1)
        );setq
        (entmod e1)
       );progn then modify the first ent before moving on to the next non-overlaper
   );if
   (setq p1 p3
         p2 p4
         na na2
   );setq
   (setq mod nil)
  );cond #3 no overlap
 );cond close
 (setq n (+ n 1))
 );repeat
 (if mod
     (progn
      (setq e1 (entget na)
            e1 (subst (cons 10 p1) (assoc 10 e1) e1)
            e1 (subst (cons 11 p2) (assoc 11 e1) e1)
      );setq
      (entmod e1)
     );progn then modify
 );if
 
 
 ;; Return the number of objects deleted and the update selection set
 (list j ss2)
);defun acet-overkill-resolve-lines
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check the potential over-lappers and resolve any that are found.
;;
;;Arrange each arc angles so that start angle is always less than end angle
;;Sort the list by start angle
;;Modify the first element to stretch past any overlapping objects and delete the overlappers.
;;If an element's lowest angle is not less than or equal to the highest point so far then 
;;make that element the new stretcher element.
;;
(defun acet-overkill-resolve-arcs2 ( lst ss2 fuz no-partial no-endtoend / 
                                    index slope n x na na2 a b a2 b2 mod j e1
                                  )
 
 (setq lst (cdr lst)
       lst (mapcar '(lambda ( x / a b )
                      (setq a (acet-angle-format (nth 2 x))
                            b (acet-angle-format (nth 3 x))
                      )
                      (if (<= b a)
                          (setq b (+ b pi pi))
                      );if
                      (list (nth 0 x) (nth 1 x) a b (nth 4 x))
                    )
                    lst
           );mapcar
       lst (vl-sort lst
                    '(lambda (a b) 
                      (< (nth 2 a) (nth 2 b))
                     )
           );vl-sort
         x (car lst)
         a (nth 2 x) ;start angle
         b (nth 3 x) ;end angle
        na (nth 4 x)
         j 0
 );setq
 (setq n 1)
 (repeat (- (length lst) 1)
 (setq   x (nth n lst)
        a2 (nth 2 x)
        b2 (nth 3 x)
       na2 (nth 4 x)
 );setq
 
 (cond
  ((equal a2 b 0.00000001)
   (if (not no-endtoend)
       (progn 
        (if (> b2 b)
             (setq   b b2
                   mod T
             );setq then
        );if
        (setq ss2 (ssadd na2 ss2))
        ;(entdel na2)
        (setq j (+ j 1))
       );progn ok to combine end to end
   );if
  );cond #1 end to end
 
  ((< a2 b)
   (if (not no-partial)
       (progn
        (if (> b2 b)
            (setq   b b2
                  mod T
            );setq then
        );if
        (setq ss2 (ssadd na2 ss2))
        ;(entdel na2)
        (setq j (+ j 1))
       );progn then ok to combine partial overlap
   );if
  );cond #2 overlap
 
  (T
      (if mod
          (progn
           (setq e1 (entget na))
           (if (acet-angle-equal a b 0.00000001)
               (progn
                (setq e1 (subst '(0 . "CIRCLE") (assoc 0 e1) e1)
                      e1 (vl-remove (assoc 50 e1) e1)
                      e1 (vl-remove (assoc 51 e1) e1)
                );setq
                (while (assoc 100 e1)
                  (setq e1 (vl-remove (assoc 100 e1) e1));setq
                );while
                (entmake e1)
                (entdel na)
                (setq na (entlast))
               );progn then change it to a circle by entmaking a new circle and deleting the arc
               (progn
                (setq e1 (subst (cons 50 a) (assoc 50 e1) e1)
                      e1 (subst (cons 51 b) (assoc 51 e1) e1)
                );setq
                (entmod e1)
               );progn else just entmod the arc
           );if
          );progn then modify the first ent before moving on to the next non-overlaper
      );if
      (setq  a a2
             b b2
            na na2
      );setq
      (setq mod nil)
  );cond #3 no overlap
 );cond close
 (setq n (+ n 1))
 );repeat
 (if mod
     (progn
      (setq e1 (entget na))
      (if (acet-angle-equal a b 0.00000001)
          (progn
           (setq e1 (subst '(0 . "CIRCLE") (assoc 0 e1) e1)
                 e1 (vl-remove (assoc 50 e1) e1)
                 e1 (vl-remove (assoc 51 e1) e1)
           );setq
           (while (assoc 100 e1)
            (setq e1 (vl-remove (assoc 100 e1) e1));setq
           );while
           (entmake e1)
           (entdel na)
           (setq na (entlast))
          );progn then change it to a circle by entmaking a new circle and deleting the arc
          (progn
           (setq e1 (subst (cons 50 a) (assoc 50 e1) e1)
                 e1 (subst (cons 51 b) (assoc 51 e1) e1)
           );setq
           (entmod e1)
          );progn else just entmod the arc
      );if
     );progn then modify the first ent before moving on to the next non-overlaper
 );if
 
 ;; Return the number of objects deleted and the update selection set
 (list j ss2)
);defun acet-overkill-resolve-arcs
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-overkill-line-data2 ( e1 fuz genprops / p1 p2 dx dy dz m b m2 b2 xv th )
 
 (setq p1 (cdr (assoc 10 e1))
       p2 (cdr (assoc 11 e1))
       dx (- (car p2) (car p1))
       dy (- (cadr p2) (cadr p1))
       dz (- (caddr p2) (caddr p1))
 );setq
 ;; first get the slope and y intercept in the xy plane.
 (if (and (/= dx 0.0)
          (setq m (/ dy dx))		;slope
          (< (abs m) 1.0e+010)
     );and
     (progn
      (setq b (- (cadr p1)	;y-intercept -> b=y-m*x
                 (* m (car p1))
              )
      );setq
     );progn then
     (setq m nil      ;undefined slope
           b (car p1) ;x-intercept
     );setq else
 );if
 ;; Now get the slope and z intercept in a different plane
 (if (and m
          (equal m 0.0 0.00000001)
     );and
     (progn
      ;; then use the xz plane because the slope is undefined in the yz
      (if (and (/= dx 0.0)
               (setq m2 (/ dz dx))		;slope
               (< (abs m2) 1.0e+010)
          );and
          (setq b2 (- (caddr p1)	;z-intercept -> b2=z-m2*x
                      (* m2 (car p1))
                   )
          );setq then
          (setq m2 nil       ;undefined slope
                b2 (car p1) ;z-intercept
          );setq else
      );if
     );progn then use xz plane
     (progn
      ;; else use yz plane
      (if (and (/= dy 0.0)
               (setq m2 (/ dz dy))		;slope
               (< (abs m2) 1.0e+010)
          );and
          (setq b2 (- (caddr p1)	;z-intercept -> b2=z-m2*y
                      (* m2 (cadr p1))
                   )
          );setq then
          (setq m2 nil       ;undefined slope
                b2 (cadr p1) ;z-intercept
          );setq else
      );if
     );progn else use yz plane
 );if
 (if m
     (setq m (acet-calc-round m 0.00000001))	;; xy plane slope
 );if
 (if m2
     (setq m2 (acet-calc-round m2 0.00000001))	;; yz slope
 );if
 (setq  b (acet-calc-round b fuz)			;; y intercept
       b2 (acet-calc-round b2 fuz)			;; z intercept
 );setq
 (if (setq th (cdr (assoc 39 e1)))
     (setq xv (cdr (assoc 210 e1))
           xv (mapcar '(lambda (x) (acet-calc-round x 0.00000001)) xv)
     );setq then it has thickness so we need to bring the extrusion vector along for the ride
 );if
 (if xv
     (list 0 m b m2 b2
           xv
           (acet-overkill-gen-prop-get2 e1 genprops)	;; general data
     );list
     (list 0 m b m2 b2
           (acet-overkill-gen-prop-get2 e1 genprops)	;; general data
     );list
 );if
);defun acet-overkill-line-data
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes an elist and a list of group codes and returns a list of dotted pairs for that entity.
;
(defun acet-overkill-gen-prop-get2 ( e1 genprops / a lst )
 (foreach gcode genprops
  (if (not (setq a (assoc gcode e1)))
      (setq a (list gcode))
  );if
  (setq lst (cons a lst))
 );foreach
 lst
);defun acet-overkill-gen-prop-get
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;similar to ai_utils version except more precision is allowed for small floating point numbers
(defun acet-rtos2 (val / a b units old_dimzin)
  (setq units (getvar "lunits"))
  ;; No fiddling if units are Architectural or Fractional
  (if (or (= units 4) (= units 5))
    (rtos val)
    ;; Otherwise work off trailing zeros
    (progn
      (setq old_dimzin (getvar "dimzin"))
      ;; Turn off bit 8
      (setvar "dimzin" (logand old_dimzin (~ 8)))
      (setq a (rtos val))
      ;; Turn on bit 8
      (setvar "dimzin" (logior old_dimzin 8))
      (setq b (rtos val units 15))
      ;; Restore dimzin
      (setvar "dimzin" old_dimzin)
      ;; Fuzz factor used in equality check.
      (if (equal (distof a) (distof b) 0.00000000000001) a b)
    )
  )
);defun acet-rtos
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build a master list is a list of sub-lists that contain data about potential overlapping objects.
;; For example: a line sublist will contain a general data sublist as the first element (for assoc 
;; purposes) and real-data sublists for individual lines that match that general data will follow. 
;; Each element in this sublist is a potential overlapper with other elements in the same sublist.
;;
;; Takes:
;; ss-  a selection set
;; fuz- a fuz value used for rounding reals
;; props - list that contains groups codes for additional properties to include in the 
;;         general data sublists. This gives control over whether objects with differing
;;         layers should or should not be removed. Or color or linetype ...etc.
;;
(defun acet-overkill-ss->primitives2 ( ss fuz ignore / 
                                      flt lst gcode genprops n na e1 tp a b gen c d xv 
                                      vlst tmp alst lst2 lst3 j len k
                                    )
 
 (acet-ss-clear-prev)
 (command "_.select" ss)
 (while (wcmatch (getvar "cmdnames") "*SELECT*") (command ""))
 (setq flt '((-4 . "<OR")
              (0 . "LINE") (0 . "ARC") (0 . "CIRCLE") (0 . "LWPOLYLINE")
              (-4 . "<AND")
               (0 . "POLYLINE")
              (-4 . "<NOT") (-4 . "&") (70 . 88) (-4 . "NOT>") ;8 16 64 not 3dpoly mesh or pface mesh
              (-4 . "AND>")
             (-4 . "OR>")
            )
        ss (ssget "_p" flt)
 );setq
 (if (not ss)
     (setq ss (ssadd))
 );if
 
 
 ;; build a general props list of group codes the does not include any gcs from the ignore list
 ;; layer	8
 ;; linetype	6
 ;; thickness	39
 ;; color	62
 ;; lweight	370
 ;; plotstyle	390
 
 (setq lst '(8 6 39 62 370 390));setq	;; general properties
 (foreach gcode lst
  (if (not (member gcode ignore))
      (setq genprops (cons gcode genprops)) 
  );if
 );foreach
 (setq lst nil)
 
 (setq len (sslength ss)
         k (/ len 5)
         j 1
 )
 (acet-ui-progress-init "Gathering line, arc and circle data " len)
 
 (setq n 0)
 (repeat (sslength ss)
  (setq  na (ssname ss n)
        lst nil
  );setq
 
  (cond
   ((and (setq e1 (entget na)
               tp (cdr (assoc 0 e1))
         );setq
         (= tp "LINE")
    );and
    (setq     a (cdr (assoc 10 e1))
              b (cdr (assoc 11 e1))
            lst (list a b na)
            gen (acet-overkill-line-data2 e1 fuz genprops)
    );setq
   );cond #1
 
   ((or (= tp "ARC")
        (= tp "CIRCLE")
    );or
    (setq   a (cdr (assoc 50 e1))
            b (cdr (assoc 51 e1))
            c (cdr (assoc 10 e1))			;; center
            d (cdr (assoc 40 e1)) 			;; radius
            c (list (acet-calc-round (car c) fuz)
                    (acet-calc-round (cadr c) fuz)
                    (acet-calc-round (caddr c) fuz)
              );list
            d (acet-calc-round d fuz)
           xv (cdr (assoc 210 e1))
           xv (list (acet-calc-round (car xv) 0.00000001)
                    (acet-calc-round (cadr xv) 0.00000001)
                    (acet-calc-round (caddr xv) 0.00000001)
              );list
          gen (list 1						;; arc type
                    c 						;; center
                    d						;; radius
                    xv						;; extrusion vector (slightly rounded)
                    (acet-overkill-gen-prop-get2 e1 genprops)	;; general props
              );list
    );setq
    (if (not a)
        (setq a 0.0
              b (+ pi pi)
        );setq then circle
    );if
    (setq lst (list (cdr (assoc 10 e1))  	;; real center
                    (cdr (assoc 40 e1))  	;; real radius
                    a				;; start angle
                    b				;; end angle
                    na
              );list
    );setq
   );cond #2
  );cond close
 
  (if (= j k)
      (progn
       (acet-ui-progress-safe (fix (* 0.5 n))) 
       (setq j 1)
      );progn then
      (setq j (+ j 1))
  );if
 
  (if lst
      (setq vlst (cons (list gen lst);list
                       vlst
                 );cons
      );setq then
  );if
 (setq n (+ n 1));setq
 );repeat
 
 (setq j (/ len 2));setq
 (acet-ui-progress-safe j)
 
 ;;The approach:
 ;; -split in two: lines and arcs
 ;; for lines:
 ;; -sort by y-intercept
 ;; for arcs:
 ;; - sort by radius
 ;; -lines...
 ;;  - Use a while loop to group the lines with identical y-intercept
 ;;  - Then foreach group use acet-list-group-by-assoc to split into
 ;;    truly unique groups. 
 ;;    Assemble the main list along the way using cons for length of 1 
 ;;    and append for greater length.
 ;; - arcs...
 ;;   Handle arcs in same as lines but use radius instead of y-int.
 ;;
 
 
 (setq vlst (vl-sort vlst
                     '(lambda ( a b )
                       (> (car (car a)) (car (car b)))	;0 or 1 (line or arc respectively)
                      )
            )
 );setq
 (while (and (setq a (car vlst))
             (= (car (car a)) 1)
        );and
  (setq alst (cons a alst)
        vlst (cdr vlst)
  );setq
 );while
 
 
 (setq j (+ j (fix (* 0.05 len))))
 (acet-ui-progress-safe j)
 
 (setq vlst (vl-sort vlst			;; sort the line list
                     '(lambda ( a b )
                       (setq a (car a)
                             b (car b)
                       )
                       (< (nth 2 a) (nth 2 b))	;0 slope y-int  
                      )
            )
 );setq
 
 (setq j (+ j (fix (* 0.2 len))))
 (acet-ui-progress-safe j)
 
 (setq alst (vl-sort alst			;; sort the arc list
                     '(lambda ( a b )
                       (setq a (car a)
                             b (car b)
                       )
                       (< (nth 2 a) (nth 2 b))	;1 center radius  
                      )
            )
 );setq
 
 (setq j (+ j (fix (* 0.1 len))))
 (acet-ui-progress-safe j)
 
 (while (setq lst (car vlst))			;; group by items that have save y-int
  (setq vlst (cdr vlst)
           a (nth 2 (car lst))	;y-int
        lst2 (list lst)
  );setq
  (while (and (setq b (car vlst))
              (equal a
                     (nth 2 (car b))
              );equal
         );and
   (setq vlst (cdr vlst)
         lst2 (cons b lst2)
   );setq
  );while
  (setq lst3 (cons lst2 lst3));setq
 );while
 
 (setq j (+ j (fix (* 0.05 len))))
 (acet-ui-progress-safe j)
 
 (setq vlst nil
       lst2 nil
 );setq
 (foreach lst lst3				;; for each group of equal y-int, group by identical car
  (setq lst2 (acet-list-group-by-assoc lst))
  (if (equal 1 (length lst2))
      (setq vlst (cons (car lst2) vlst))
      (setq vlst (append lst2 vlst))
  );if
 );foreach
 
 (setq lst3 nil)
 (while (setq lst (car alst))
  (setq alst (cdr alst)
           a (nth 2 (car lst))	;radius
        lst2 (list lst)
  );setq
  (while (and (setq b (car alst)) 
              (equal a 
                     (nth 2 (car b))
              );equal
         );and
   (setq alst (cdr alst)
         lst2 (cons b lst2)
   );setq
  );while
  (setq lst3 (cons lst2 lst3));setq
 );while
 
 (setq j (+ j (fix (* 0.05 len))))
 (acet-ui-progress-safe j)
 
 (setq alst nil
       lst2 nil
 );setq
 (foreach lst lst3
  (setq lst2 (acet-list-group-by-assoc lst))
  (if (equal 1 (length lst2))
      (setq alst (cons (car lst2) alst))
      (setq alst (append lst2 alst))
  );if
 );foreach
 
 (acet-ui-progress-done)
 
 (append vlst alst)
);defun acet-overkill-ss->primitives


(princ)
