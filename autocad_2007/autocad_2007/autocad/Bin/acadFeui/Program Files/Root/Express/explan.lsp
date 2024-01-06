;;
;;  explan.lsp - Express Tools plan replacement command
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
;;  Similar to PLAN except that EXPLAN automatically zooms to the center 
;; of the extents of selected objects after performing a plan view to
;; the specified ucs. i.e. In other words; it keeps the zoom distance 
;; consistant.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun c:explan ( / ss c s lst d )
 (acet-error-init 
   (list '( "cmdecho" 0
            "ucsicon" nil
          ) 
         1
         '(setq ACET:UCS-LIST nil)
   );list
 )
 (setq s (getvar "viewsize"))
 (princ "\nSelect objects to zoom to or press <enter> to select everything on screen...")
 (if (and (setq ss (ssget))
          (setq c (acet-geom-ss-extents ss nil))
     );and
     (setq c (acet-geom-midpoint (car c) (cadr c)));setq then
     (progn
      (setq lst (acet-explan-view-extents)
            lst (acet-geom-m-trans lst 0 1)
            lst (acet-geom-cube-points lst)
            lst (acet-geom-list-extents lst)
              c (acet-geom-midpoint (car lst) (cadr lst))
              d (distance (car lst) (cadr lst))
      );setq
      (if (> d s)
          (setq s (/ (+ d s) 2.0));then use the average of the two
      );if
      ;(command "_.line" c pause "")
     );progn else
 );if
 (princ "\nEnter an option [Current ucs/Ucs/World] <Current>: ")
 (command "_.plan")
 (setvar "cmdecho" 1)
 (while (wcmatch (getvar "cmdnames") "*PLAN*")
  (command pause)
 );while
 (setvar "cmdecho" 0)
 (if c
     (command "_.zoom" "_c" c s)
 );if
 (acet-error-restore)
);defun c:explan
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Selects everything  on screen and uses the extents of that combined with the screen 
;corcer points to determine the center point (wcs) to use for the zoom after the plan 
;command.
;Returns list of points in wcs
;
(defun acet-explan-view-extents ( / ss p1 p2 p3 p4 lst px )
 (princ "\nSelecting objects on screen...")
 (acet-sysvar-set (list "ucsicon" 0))
 (acet-ucs-cmd (list "_view"))
 (setq  p1 (acet-geom-view-points)
        p2 (cadr p1)
        p1 (car p1)
        px (acet-geom-pixel-unit)
        p1 (list (+ (car p1) px)
                 (+ (cadr p1) px)
                 (caddr p1)
           )
        p2 (list (- (car p2) px)
                 (- (cadr p2) px)
                 (caddr p2)
           )
 );setq
 (if (or (setq ss (ssget "_w" p1 p2))
         (setq ss (ssget "_c" p1 p2))
     );or
     (setq p3 (acet-geom-ss-extents ss nil) ;no shrinkwrap
           p4 (cadr p3)
           p3 (car p3)
           p1 (list (car p1)		;; combine the view extents xy with extents of objects in z
                    (cadr p1)
                    (min (caddr p3) (caddr p4))
              )
           p2 (list (car p2)
                    (cadr p2)
                    (max (caddr p3) (caddr p4))
              )
     );setq then
 );if
 (setq lst (acet-geom-cube-points (list p1 p2)) ;generate the remaining points based on lower left and upper right
       lst (acet-geom-m-trans lst 1 0)
 );setq
 (acet-ucs-cmd (list "_previous"))
 (acet-sysvar-restore)
 (princ "Done")
 lst
);defun acet-explan-view-extents
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Returns a list of 8 points 4 top and 4 bottom.
;
(defun acet-geom-cube-points ( lst / p1 p2 z1 z2 )
 (setq lst (acet-geom-list-extents lst)
        p1 (car lst)
        p2 (cadr lst)
        z1 (min (caddr p1) (caddr p2))
        z2 (max (caddr p1) (caddr p2))
 );setq
 (list (list (car p1) (cadr p1) z1)
       (list (car p2) (cadr p1) z1)
       (list (car p2) (cadr p2) z1)
       (list (car p1) (cadr p2) z1)
       (list (car p1) (cadr p1) z2)
       (list (car p2) (cadr p1) z2)
       (list (car p2) (cadr p2) z2)
       (list (car p1) (cadr p2) z2)
 );list
);defun acet-geom-cube-points


(princ)