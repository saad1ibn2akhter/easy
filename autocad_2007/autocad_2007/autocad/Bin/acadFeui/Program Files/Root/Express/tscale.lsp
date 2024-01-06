;;
;;  tscale.lsp - text scaling utilities
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
;; Description:
;; 
;; Series of annotation scaling utilities that work with TEXT/MTEXT/ATTDEFS/ATTRIBS.
;; Also includes an interference checker for annotation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Find annotation objects that have obstructing or overlapping geometry and build a 
;selection set.
;
(defun c:tSpaceInvaders ( / flt ss ss2 ans lok )
 (acet-error-init 
  (list (list   "cmdecho" 0 
                "ucsicon" 0
                 "osmode" 0
              "ucsfollow" 0
        )
        0
        '(progn
          (if na (redraw na 4))
          (if (tblobjname "view" "acet-tspaceinvaders")
              (progn
               (acet-sysvar-set (list "cmdecho" 0))
               ;(command "_.view" "_r" "acet-tspaceinvaders")
               (command "_.view" "_d" "acet-tspaceinvaders")
               (acet-sysvar-restore)
              );progn then delete the view
          );if
         );progn
  );list
 )
 (setq flt '((-4 . "<OR")
               (0 . "ATTDEF")
               (0 . "TEXT")
               (0 . "MTEXT")
               (0 . "RTEXT")
               (-4 . "<AND")
                 (0 . "INSERT")
                 (66 . 1)
               (-4 . "AND>")
             (-4 . "OR>")
            )
       lok (acet-layer-unlock-all)
 );setq
 
 (if (and (not (and (= (getvar "viewmode") 1)
                    (princ "\n** Command not allowed in perspective view **")
               );and
          );not
          (setq ss (ssget flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
          (setq ss (acet-ss-annotation-filter ss))
     );and
     (progn
      (if (setq ss2 (acet-tspaceinvaders ss))
          (progn
           (princ (acet-str-format "\n%1 text objects were found to have overlapping objects." (sslength ss2)))
           (if (= (getvar "cmdnames") "")
               (progn
                (initget "Yes No")
                (setq ans (getkword "\nStep through each one for visual verification? <N>: "))
                (if (= "Yes" ans)
                    (setq ss2 (acet-tspaceinvaders-interact ss2))
                );if
               );progn then not transparent so allow interactive mode
           );if
          );progn then
          (princ "\nNo text with overlapping objects found.")
      );if
     );progn then
 );if
 (if lok
     (command "_.-layer" "_lock" lok "");then re-lock the layers we unlocked.
 );if
 
 (acet-error-restore)
 (if ss2
     (progn
      (princ (acet-str-format "\n%1 objects were placed in the current selection set." 
                              (sslength ss2)
             )
      )
      (sssetfirst ss2 ss2)
     );progn then
 );if
 (princ)
);defun c:tSpaceInvaders
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-tspaceinvaders ( ss / flt n na e1 lst lst2 ss2 ss3 j p1 p2 p3 getx )
 
 (if (tblobjname "view" "acet-tspaceinvaders")
     (command "_.view" "_d" "acet-tspaceinvaders")
 );if
 (command "_.view" "_s" "acet-tspaceinvaders")
 
 ;; sort the selction set to improve performance
 (defun getx ( e1 ) (car (cdr (assoc 10 e1))));defun
 (setq ss (acet-ss-sort ss 'getx))
 
 (setq flt '((-4 . "<OR")
               (0 . "ATTDEF")
               (0 . "TEXT")
               (0 . "MTEXT")
               (0 . "RTEXT")
               (-4 . "<AND")
                 (0 . "INSERT")
                 (66 . 1)
               (-4 . "AND>")
             (-4 . "OR>")
            )
       ss3 (ssadd)
 );setq
 
 (acet-ui-progress-init "Searching for text with overlapping geometry..." (sslength ss))
 
 (setq n 0)
 (repeat (sslength ss)
  (setq   na (ssname ss n)
          e1 (entget na)
         lst (acet-geom-textbox e1 0.0)
  );setq
  (acet-ui-progress-safe n)
 
  (if (and lst
           (not (equal (car lst) (cadr lst)))
           (not (equal (car lst) (caddr lst)))
           (not (equal (cadr lst) (caddr lst)))
      );and
      (progn
       (setq lst2 (acet-geom-m-trans lst 1 0))
       (acet-ucs-cmd (list "_3p" (car lst) (cadr lst) (caddr lst)))
       (setq lst2 (acet-geom-m-trans lst2 0 1));setq
 
       (acet-tspaceinvaders-get-text-on-screen e1)
 
       (acet-ss-visible (ssadd na (ssadd)) 1)			;; make it invisible
       (entupd na)
       (setq ss2 (ssget "_c" (car lst2) (caddr lst2)));setq     ;; get a selection set of intruders
       (acet-ss-visible (ssadd na (ssadd)) 0)			;; make is visible again
       (entupd na)
 
      );progn then
      (setq ss2 nil)
  );if
 
  (if ss2
      (setq ss3 (ssadd na ss3))
  );if
  (acet-ucs-cmd (list "_p"))
  (setq n (+ n 1));setq
 );repeat
 
 (acet-ui-progress-done)
 
 (command "_.view" "_r" "acet-tspaceinvaders")
 (command "_.view" "_d" "acet-tspaceinvaders")
 
 (if (= (sslength ss3) 0)
     (setq ss3 nil)
 );if
 ss3
);defun acet-tspaceinvaders
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-tspaceinvaders-interact ( ss / na e1 lst n vpna ans p1 p2 p3 j )
 
 (if (tblobjname "view" "acet-tspaceinvaders")
     (command "_.view" "_d" "acet-tspaceinvaders")
 );if
 (command "_.view" "_s" "acet-tspaceinvaders")
 
 
 (setq vpna (acet-currentviewport-ename));setq
 (if (acet-viewport-is-perspective vpna)
     (princ "\nUnable to zoom to objects while in perspective view.")
     (progn
      (acet-ui-progress-init "Searching for text with overlapping geometry..." (sslength ss))
 
      (setq j 0)
      (setq n 0)
      (while (< n (sslength ss))
 
       (acet-ui-progress-safe j) 
       (setq j (+ j 1))
 
       (setq  na (ssname ss n)
              e1 (entget na)
             lst (acet-geom-textbox e1 10.0)
             lst (acet-geom-list-extents lst)
              p1 (car lst)
              p2 (cadr lst)
              p3 (acet-geom-midpoint p1 p2)
              p1 (acet-geom-point-scale p1 p3 2.5)
              p2 (acet-geom-point-scale p2 p3 2.5)
       );setq
       (setq vpna (acet-currentviewport-ename));setq
       (if (acet-viewport-is-perspective vpna)
           (princ "\nUnable to zoom to objects while in perspective view.")
           (progn
            (command "_.zoom" p1 p2)
            (redraw na 3)
            (acet-blink-and-show-object (list na 2))
            (initget "Yes No eXit")
            (setq ans (getkword "\nInclude this one in the selection set? [eXit] <Y>: "))
            (cond
             ((= ans "No")
              (setq ss (ssdel na ss)
                     n (- n 1)
              );setq then
             )
             ((= ans "eXit")
              (setq n (sslength ss))
             )
            );cond close
            (redraw na 4)
           );progn then
       );if
       (setq n (+ n 1));setq
      );while
      (acet-ui-progress-done)
     );progn else
 );if
 (command "_.view" "_r" "acet-tspaceinvaders")
 (command "_.view" "_d" "acet-tspaceinvaders")
 
 ss
);defun acet-tspaceinvaders-interact
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tspaceinvaders-get-text-on-screen ( e1 / lst h x a b )
 
 (setq lst (acet-geom-textbox e1 0.0)
         h (cdr (assoc 40 e1)) ;height
         x 0.01  
 );setq
 (if (or (acet-geom-zoom-for-select lst)
         (< h (* x (getvar "viewsize")))
     );or
     (command "_.zoom" "_c"
              (acet-geom-midpoint (car lst) (caddr lst))
              (max (* 1.1 (distance (car lst) (caddr lst)))
                   (* (/ 1.0 x) h)
              );max
     );command
 );if
);defun acet-tspaceinvaders-get-text-on-screen
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun c:tscale ( / flt ss mode val base )
 (acet-error-init '(("cmdecho" 0 
                     "highlight" nil
                    )
                    1
                   )
 );acet-error-init
 
 (setq flt '((-4 . "<OR")
               (0 . "ATTDEF")
               (0 . "TEXT")
               (0 . "MTEXT")
               (0 . "RTEXT")
               (-4 . "<AND")
                 (0 . "INSERT")
                 (66 . 1)
               (-4 . "AND>")
             (-4 . "OR>")
            )
 );setq
 (if (and (setq ss (ssget "_:L" flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
          (setq ss (acet-ss-annotation-filter ss))
     );and
     (progn
      (setq base (acet-tscale-ui-get-scalebase)
            mode (acet-tscale-ui-get-mode)
      );setq
      (if (= mode 0)
          (setq val (acet-tscale-ui-get-factor))
          (setq val (acet-tscale-ui-get-height))
      );if
      (acet-tscale ss base mode val)
     );progn then
 );if
 (acet-error-restore)
);defun c:tscale
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pstscale - paper-space-text-scale
;
(defun c:psTscale ( / flt ss mode h base na e1 n setmode vh xd vs val id )
 (acet-error-init '(("cmdecho" 0 
                     "highlight" nil
                    )
                    1
                   )
 );acet-error-init
 (setq id "ACET-PSTSCALE")
 (if (not (tblobjname "appid" id))
     (regapp id)
 );if
 
 (setq flt '((-4 . "<OR")
               (0 . "ATTDEF")
               (0 . "TEXT")
               (0 . "MTEXT")
               (0 . "RTEXT")
               (-4 . "<AND")
                 (0 . "INSERT")
                 (66 . 1)
               (-4 . "AND>")
             (-4 . "OR>")
            )
 );setq
 (if (and (setq ss (ssget "_:L" flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
          (setq ss (acet-ss-annotation-filter ss))
     );and
     (progn
      (setq setmode (acet-pstscale-ui-get-mode)) ;; set or update - 1 or 0 respectively
      (if (= setmode 1)
          (progn
           (setq h (acet-pstscale-ui-get-height))
           (setq n 0)
           (repeat (sslength ss)
            (setq na (ssname ss n))
            (acet-ps-scale-set-xdata na h id)
            (setq n (+ n 1));setq
           );repeat
          );progn then set the paper space height value
      );if
      (setq base (acet-tscale-ui-get-scalebase))
      (cond
       ((= (getvar "tilemode") 1)
        (princ "\n** Update not allowed in Model Tab **")
       );cond #1
       ((not (setq na (acet-currentviewport-ename)))
        (princ "\nUnable to get current viewport.")
       );cond #2
       ((acet-viewport-is-perspective na)
        (princ "\n** Update cannot be performed in a perspective view **")
       );cond #3
       (T
        (setq  e1 (entget na '("ACAD"))
               vs (cdr (assoc 41 e1))		;; view size
               xd (cdr (assoc -3 e1))
               xd (cdr (assoc "ACAD" xd))
               xd (acet-list-m-assoc 1040 xd)
               vh (cdr (nth 1 xd))		;; view height
              val (/ vh vs)
        );setq vh/vs=ps/ms
        (acet-tscale ss base 2 val) ;pass a mode of 2 to indicate ps scaling.
       );cond #4
      );cond close
     );progn then
 );if
 (acet-error-restore)
);defun c:psTscale
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-viewport-is-perspective ( na / e1 xd flag )
  (if na
      (setq   e1 (entget na '("ACAD"))                     
              xd (cdr (assoc -3 e1))
              xd (cdr (assoc "ACAD" xd))           
              xd (cdr (member (assoc 1070 xd) xd))         ; Strip out Extended data version #
              xd (cdr (assoc 1070 xd))                     ; and get the view mode
            flag (= 1 (logand 1 xd))                     ; Is the 1 bit set (perspective)?
      );setq then
  );if
  flag
);defun acet-viewport-is-perspective
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-ps-scale-set-xdata ( na scale appid / a e1 xd )
 (setq e1 (entget na))
 (if (not (equal (type scale) 'LIST))
     (setq scale (list scale))
 );if
 (foreach a scale
  (setq xd (cons (cons 1041 a) xd));setq
 );foreach
 (setq xd (list -3 (cons appid xd))) ;; the value will be scaled along with the object
 (setq e1 (append e1 (list xd)))
 (entmod e1)
);defun acet-ps-scale-set-xdata
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-ps-scale-get-xdata ( na appid / e1 xd )
 (setq appid (xstrcase appid)
          e1 (entget na (list appid))
          xd (cdr (assoc -3 e1))
          xd (cdr (assoc appid  xd))
          xd (mapcar 'cdr xd)
 );setq
 xd
);defun acet-ps-scale-get-xdata
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tscale ( ss base mode val / na n j id )
 (setq id "ACET-PSTSCALE")
 (acet-ui-progress-init "Scaling text" (sslength ss))
 (setq j 0)
 (setq n 0) 
 (repeat (sslength ss)
  (setq na (ssname ss n));setq
  (acet-ui-progress-safe n)
  (if (acet-tscale-ent na base mode val id)
      (setq j (+ j 1));setq
  );if
 (setq n (+ n 1));setq
 );repeat
 (acet-safe-command T T (list "_.move" ss "" "0,0" "0,0")) ;; force an update (for attributes)
 (acet-ui-progress-done)
 j
);defun acet-tscale
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tscale-ent ( na base mode val id / e1 e2 n h ss2 psh )
  (setq e1 (entget na (list id)))
  (cond
   ((= mode 0)				;; scale
    (setq h (cdr (assoc 40 e1))
          h (* h val)
    );setq
   );cond #1
   ((= mode 1)				;; set explicit height
    (setq h val);set final height
   );cond #2
   ((= mode 2)				;; get ps height from xdata
    (if (setq psh (car (acet-ps-scale-get-xdata na id)))
        (setq h (* psh val));setq then 			(val=ps/ms)
        (setq h (cdr (assoc 40 e1)));setq else
    );if
   );cond #3
  );cond close
  (cond 
   ((= "Existing" base)
    (setq e1 (subst (cons 40 h) (assoc 40 e1) e1)) ;; change the height
    (setq e2 (entmod e1))
   );cond #2
   (T
     (setq ss2 (ssadd na (ssadd))) 
     (acet-tjust ss2 base)				;; change the justification
     (setq e2 (entget na)
           e2 (subst (cons 40 h) (assoc 40 e2) e2)	;; change the height
     );setq
     (setq e2 (entmod e2))
     (acet-tjust ss2 (acet-tjust-keyword e1))		;; change the justification back
   );cond #3
  );cond close
 e2
);defun acet-tscale-ent
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-tscale-ui-get-scalebase ( / def ans id )
  (setq  id "ACET-TSCALE-SCALEBASE"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def "Existing")
  );if
  (princ "\nSpecify justification to use as base point for scaling...")
  (initget "Existing Start Center Middle Right TL TC TR ML MC MR BL BC BR")
  (setq ans (getkword
              (acet-str-format "\n[Existing/Start/Center/Middle/Right/TL/TC/TR/ML/MC/MR/BL/BC/BR] <%1>: "
                               def
              );acet-str-format
            );getkword
  );setq
  (if ans
      (acet-setvar (list id ans 3))
      (setq ans def)
  );if
  ans
);defun acet-tscale-ui-get-scalebase
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun acet-tscale-ui-get-mode ( / def ans id )
  (setq  id "ACET-TSCALE-BYHEIGHT"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 0)
  );if
  (if (= def 0)
      (setq ans "Scale")
      (setq ans "Height")
  );if
  (initget "Scale Height")
  (setq ans (getkword (acet-str-format "\nSpecify size change by [Scale (factor)/Height] <%1>: " ans)))
  (if ans
      (progn
       (if (= ans "Scale")
           (setq def 0)
           (setq def 1)
       );if
       (acet-setvar (list id def 3))
      );progn then
  );if
  def
);defun acet-tscale-ui-get-mode
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tscale-ui-get-factor ( / def ans id )
  (setq  id "ACET-TSCALE-FACTOR"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget 6)
  (setq ans (getdist (acet-str-format "\nScale factor <%1>: " 
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if ans
      (acet-setvar (list id ans 3))
      (setq ans def)
  );if
  ans
);defun acet-tscale-ui-get-factor
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-tscale-ui-get-height ( / def ans id )
  (setq  id "ACET-TSCALE-HEIGHT"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def (getvar "textsize"))
  );if
  (initget 6)
  (setq ans (getdist (acet-str-format "\nFinal height <%1>: " def)))
  (if ans
      (acet-setvar (list id ans 3))
      (setq ans def)
  );if
  ans
);defun acet-tscale-ui-get-height
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;returns 1 for set and 0 for update
;
(defun acet-pstscale-ui-get-mode ( / def ans id )
  (setq  id "ACET-PSTSCALE-SET"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1)
  );if
  (if (= def 1)
      (setq ans "Set")
      (setq ans "Update")
  );if
  (initget "Set Update")
  (setq ans (getkword 
             (acet-str-format "\nUpdate or set paper space text height [Set/Update] <%1>: " ans)
            )
  );setq
  (if ans
      (progn
       (if (= ans "Set")
           (setq def 1)
           (setq def 0)
       );if
       (acet-setvar (list id def 2)) ;set this one in the reg only
      );progn then
  );if
  def
);defun acet-pstscale-ui-get-mode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-pstscale-ui-get-height ( / def ans id )
  (setq  id "ACET-PSTSCALE-HEIGHT"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def (getvar "textsize"))
  );if
  (initget 6)
  (setq ans (getdist (acet-str-format "\nSpecify desired text height in paper space units <%1>: " def)))
  (if ans
      (acet-setvar (list id ans 3))
      (setq ans def)
  );if
  ans
);defun acet-pstscale-ui-get-height


(princ)