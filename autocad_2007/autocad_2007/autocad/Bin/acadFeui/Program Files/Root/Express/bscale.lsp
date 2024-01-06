;;
;;  Bscale.lsp - Express Tools block scaling utilities
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
;; Series of block/xref scaling utilities.
;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun c:bscale ( / flt ss absolute val )
 (acet-error-init '(("cmdecho" 0
                     "highlight" nil
                     "regenmode" 0
                    )
                    1
                   )
 );acet-error-init
 
 (setq flt '((0 . "INSERT")));setq
 (if (and (setq ss (ssget "_:L" flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
     );and
     (progn
      (setq absolute (acet-bscale-ui-get-mode));setq
      (if (= absolute 0)
          (setq val (acet-bscale-ui-get-relative-factors))
          (setq val (acet-bscale-ui-get-absolute-factors))
      );if
      (acet-bscale ss absolute (car val) (cadr val) (caddr val))
     );progn then
 );if
 (acet-error-restore)
);defun c:bscale
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;PSBSCALE - paper-space-block-scale
;
(defun c:PSBSCALE ( / flt ss mode scale na e1 n setmode vh xd vs val id )
 (acet-error-init '(("cmdecho" 0 
                     "highlight" nil
                     "regenmode" 0
                    )
                    1
                   )
 );acet-error-init
 (setq id "ACET-PSBSCALE")
 (if (not (tblobjname "appid" id))
     (regapp id)
 );if
 
 (setq flt '((0 . "INSERT")));setq
 (if (and (setq ss (ssget "_:L" flt))
          (setq ss (car (acet-ss-filter (list ss nil T))));setq filter out non-current space
     );and
     (progn
      (setq setmode (acet-PSBSCALE-ui-get-mode)) ;; set or update - 1 or 0 respectively
      (if (= setmode 1)
          (progn
           (setq scale (acet-PSBSCALE-ui-get-scale-factors))
           (setq n 0)
           (repeat (sslength ss)
            (setq na (ssname ss n))
            (acet-ps-scale-set-xdata na scale id)
            (setq n (+ n 1));setq
           );repeat
          );progn then set the paper space height value
      );if
 
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
        (acet-bscale ss 2 val val val) ;; pass a mode of 2 to indicate ps scaling.
       );cond #4
      );cond close
 
     );progn then
 );if
 (acet-error-restore)
);defun c:PSBSCALE
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Returns 1 for set and 0 for update
;
(defun acet-PSBSCALE-ui-get-mode ( / def ans id )
  (setq  id "ACET-PSBSCALE-SET"
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
             (acet-str-format "\nUpdate or set paper space scale for blocks [Set/Update] <%1>: " ans)
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
);defun acet-PSBSCALE-ui-get-mode
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of 3 scale factors- (x y z)
;
(defun acet-PSBSCALE-ui-get-scale-factors ( / def ans x y z id )
  (setq  id "ACET-PSBSCALE-SCALE-X"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget "Xyz" 6)
  (setq ans (getdist (acet-str-format "\nX scale factor relative to paper space or [XYZ] <%1>: "
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if (not ans)
      (setq ans def)
  );if
  (if (= ans "Xyz")
      (progn
       (initget 6)
       (setq ans (getdist (acet-str-format "\nX scale factor relative to paper space <%1>: "
                                           (rtos def 2 (getvar "luprec"))
                          )
                 )
       );setq
       (if (not ans)
           (setq x def)
           (setq x ans)
       );if
       (initget 6)
       (setq ans (getdist "\nY scale factor relative to paper space <use X scale factor>: "))
       (if (not ans)
           (setq y x)
           (setq y ans)
       );if
       (initget 6)
       (setq ans (getdist "\nZ scale factor relative to paper space <use X scale factor>: "))
       (if (not ans)
           (setq z x)
           (setq z ans)
       );if
      );progn then XYZ
      (progn
       (setq x ans
             z ans
       );setq
       (initget 6)
       (setq ans (getdist "\nY scale factor relative to paper space <use X scale factor>: "))
       (if ans
           (setq y ans)
           (setq y x)
       );if
      );progn else
  );if
 
  (if (and x y z)
      (progn
       (acet-setvar (list id x 3))
       (setq ans (list x y z))
      );progn
      (setq ans nil)
  );if
  ans
);defun acet-PSBSCALE-ui-get-scale-factors
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scale relative or absolute.
;
(defun acet-bscale-ui-get-mode ( / def ans id )
  (setq  id "ACET-BSCALE-ABSOLUTE"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 0)
  );if
  (if (= def 0)
      (setq ans "Relative")
      (setq ans "Absolute")
  );if
  (initget "Absolute Relative")
  (setq ans (getkword
             (acet-str-format 
               "\nSpecify type of scaling [Absolute (final)/Relative (multiply)] <%1>: "
               ans
             )
            )
  );setq
  (if ans
      (progn
       (if (= ans "Absolute")
           (setq def 1)
           (setq def 0)
       );if
       (acet-setvar (list id def 3))
      );progn then
  );if
  def
);defun acet-bscale-ui-get-mode
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-bscale-ui-get-relative-factors ( / def ans id x y z )
  (setq  id "ACET-BSCALE-RELATIVE-FACTOR-X"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget "Xyz" 6)
  (setq ans (getdist (acet-str-format "\nX scale factor or [XYZ] <%1>: "
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if (not ans)
      (setq ans def)
  );if
  (if (= ans "Xyz")
      (progn
       (initget 6)
       (setq ans (getdist (acet-str-format "\nX scale factor <%1>: "
                                           (rtos def 2 (getvar "luprec"))
                          )
                 )
       );setq
       (if (not ans)
           (setq x def)
           (setq x ans)
       );if
       (initget 6)
       (setq ans (getdist "\nY scale factor <use X scale factor>: "))
       (if (not ans)
           (setq y x)
           (setq y ans)
       );if
       (initget 6)
       (setq ans (getdist "\nZ scale factor <use X scale factor>: "))
       (if (not ans)
           (setq z x)
           (setq z ans)
       );if
      );progn then XYZ
      (progn
       (setq x ans
             z ans
       );setq
       (initget 6)
       (setq ans (getdist "\nY scale factor <use X scale factor>: "))
       (if ans
           (setq y ans)
           (setq y x)
       );if
      );progn else
  );if
 
  (if (and x y z)
      (progn
       (acet-setvar (list id x 3))
       (setq ans (list x y z))
      );progn
      (setq ans nil)
  );if
  ans
);defun acet-bscale-ui-get-relative-factors
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-bscale-ui-get-absolute-factors ( / def ans id x y z )
  (setq  id "ACET-BSCALE-ABSOLUTE-FACTOR-X"
        def (acet-getvar (list id))
  );setq
  (if (not def)
      (setq def 1.0)
  );if
  (initget "Xyz" 6)
  (setq ans (getdist (acet-str-format "\nAbsolute X scale or [XYZ] <%1>: "
                                      (rtos def 2 (getvar "luprec"))
                     )
            )
  );setq
  (if (not ans)
      (setq ans def)
  );if
  (if (= ans "Xyz")
      (progn
       (initget 6)
       (setq ans (getdist (acet-str-format "\nAbsolute X scale <%1>: "
                                           (rtos def 2 (getvar "luprec"))
                          )
                 )
       );setq
       (if (not ans)
           (setq x def)
           (setq x ans)
       );if
       (initget 6)
       (setq ans (getdist "\nAbsolute Y scale <use X scale>: "))
       (if (not ans)
           (setq y x)
           (setq y ans)
       );if
       (initget 6)
       (setq ans (getdist "\nAbsolute Z scale <use X scale>: "))
       (if (not ans)
           (setq z x)
           (setq z ans)
       );if
      );progn then XYZ
      (progn
       (setq x ans
             z ans
       );setq
       (initget 6)
       (setq ans (getdist "\nAbsolute Y scale <use X scale>: "))
       (if ans
           (setq y ans)
           (setq y x)
       );if
      );progn else
  );if
 
  (if (and x y z)
      (progn
       (acet-setvar (list id x 3))
       (setq ans (list x y z))
      );progn
      (setq ans nil)
  );if
  ans
);defun acet-bscale-ui-get-absolute-factors
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Takes a selection set a scaling mode and values.
; Scaling modes are as follows:
; 0 relative
; 1 absolute
; 2 paperspace
;
; Returns number of objects successfully modified.
;
(defun acet-Bscale ( ss mode xs ys zs / id na n j tmp )
 (setq  id "ACET-PSBSCALE"
       tmp "acet-tmp-block"
 );setq
 (acet-sysvar-set 
  (list "regenmode" 0 
           "attreq" 0
          "ucsicon" 0
  )
 )
 (acet-ui-progress-init "Scaling block inserts" (sslength ss))
 (setq j 0)
 (setq n 0) 
 (repeat (sslength ss)
  (setq na (ssname ss n));setq
  (acet-ui-progress-safe n)
  (if (acet-Bscale-ent na mode xs ys zs id tmp)
      (setq j (+ j 1));setq
  );if
 (setq n (+ n 1));setq
 );repeat
 (acet-ui-progress-done)
 (acet-sysvar-restore)
 (if (tblobjname "block" tmp)
     (acet-table-purge "block" tmp T)
 );if
 j
);defun acet-Bscale
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-Bscale-ent ( na mode xs ys zs id tmp / e1 ps bna na2 ixs iys izs )
  (setq  e1 (entget na (list id))
        ixs (cdr (assoc 41 e1))
        iys (cdr (assoc 42 e1))
        izs (cdr (assoc 43 e1))
  );setq
  (cond
   ((= mode 0)				;; scale relative
    (setq xs (* xs ixs)
          ys (* ys iys)
          zs (* zs izs)
    );setq
   );cond #1
   ((= mode 2)				;; get ps height from xdata
    (if (setq ps (acet-ps-scale-get-xdata na id))
        (setq xs (* (car ps) xs)
              ys (* (cadr ps) ys)
              zs (* (caddr ps) zs)
        );setq then 			(val=ps/ms)
        (setq xs nil);setq else no xdata
    );if
   );cond #2
  );cond close
 
  (if (and xs ys zs)
      (progn
       (setq bna (cdr (assoc 2 e1)))
       (if (or (/= 1 (cdr (assoc 66 e1)))
               (= (substr bna 1 1) "*")
           );or
           (progn
            (setq e1 (subst (cons 41 xs) (assoc 41 e1) e1)
                  e1 (subst (cons 42 ys) (assoc 42 e1) e1)
                  e1 (subst (cons 43 zs) (assoc 43 e1) e1)
            );setq
            (setq e1 (entmod e1))
           );progn then either no attribs or it's annonymous
           (progn 
            (acet-ucs-cmd (list "_ob" na))
            (if (tblobjname "block" tmp)
                (command "_.-block" tmp "_y" "0,0" na "")
                (command "_.-block" tmp "0,0" na "")
            );if
            (if (not (entget na))
                (progn
                 (setq na2 (entlast))
                 (command "_.-insert" tmp "0,0" "_xyz" 
                          (/ xs ixs) 
                          (/ ys iys)
                          (/ zs izs)
                          "0"
                 )
                 (if (not (equal na2 (entlast)))
                     (acet-explode (entlast))
                 );if
                );progn then the block command worked
            );if
            (acet-ucs-cmd (list "_p"))
           );progn else it has attributes and it is NOT annonymous.
       );if
      );progn then
      (setq e1 nil);setq else
  );if
  e1
);defun acet-Bscale-ent


(acet-autoload2	'("tscale.lsp"	(acet-ps-scale-get-xdata na appid)))
(acet-autoload2	'("tscale.lsp"	(acet-ps-scale-set-xdata na scale appid)))
(acet-autoload2	'("tscale.lsp"	(acet-viewport-is-perspective na)))
(princ)
