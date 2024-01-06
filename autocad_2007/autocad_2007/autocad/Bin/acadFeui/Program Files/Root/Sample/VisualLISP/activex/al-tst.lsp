;;;                                                                    ;
;;;  AL-TST.LSP                                                        ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998, 1999    ;
;;;  by Autodesk, Inc. All Rights Reserved.                            ;
;;;                                                                    ;
;;;  You are hereby granted permission to use, copy and modify this    ;
;;;  software without charge, provided you do so exclusively for       ;
;;;  your own use or for use by others in your organization in the     ;
;;;  performance of their normal duties, and provided further that     ;
;;;  the above copyright notice appears in all copies and both that    ;
;;;  copyright notice and the limited warranty and restricted rights   ;
;;;  notice below appear in all supporting documentation.              ;
;;;                                                                    ;
;;;  Incorporation of any part of this software into other software,   ;
;;;  except when such incorporation is exclusively for your own use    ;
;;;  or for use by others in your organization in the performance of   ;
;;;  their normal duties, is prohibited without the prior written      ;
;;;  consent of Autodesk, Inc.                                         ;
;;;                                                                    ;
;;;  Copying, modification and distribution of this software or any    ;
;;;  part thereof in any form except as expressly provided herein is   ;
;;;  prohibited without the prior written consent of Autodesk, Inc.    ;
;;;                                                                    ;
;;;  AUTODESK PROVIDES THIS SOFTWARE "AS IS" AND WITH ALL FAULTS.      ;
;;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF           ;
;;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,       ;
;;;  INC. DOES NOT WARRANT THAT THE OPERATION OF THE SOFTWARE          ;
;;;  WILL BE UNINTERRUPTED OR ERROR FREE.                              ;
;;;                                                                    ;
;;;  Restricted Rights for US Government Users.  This software         ;
;;;  and Documentation are provided with RESTRICTED RIGHTS for US      ;
;;;  US Government users.  Use, duplication, or disclosure by the      ;
;;;  Government is subject to restrictions as set forth in FAR         ;
;;;  12.212 (Commercial Computer Software-Restricted Rights) and       ;
;;;  DFAR 227.7202 (Rights in Technical Data and Computer Software),   ;
;;;  as applicable.  Manufacturer is Autodesk, Inc., 111 McInnis       ;
;;;  Parkway, San Rafael, California 94903.                            ;
;;;                                                                    ;

;;;--------------------------------------------------------------------;
;;; ******  THIS FILE IS A MEMBER OF THE VLA-TST PROJECT.  ********    ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;  This file demonstrates adding 2000 circles using AutoLISP. Each   ;
;;;  circle's visible property is then modified to visible and then    ;
;;;  invisible. Then each circle is erased individually. An elapsed    ;
;;;  timer is displayed after the creation and deletion of the circles ;
;;;  created.                                                          ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  GET-UTIME                                         ;
;;;                                                                    ;
;;;    Description:  GET-UTIME converts a fraction of a day into       ;
;;;                  seconds by multiplying the result from            ;
;;;                  (getvar "tdusrtimer") and 86400.0.                ;
;;;                                                                    ;
;;;                  Example:                                          ;
;;;                     (getvar "tdusrtimer") returns a fraction of    ;
;;;                     one day. So... (getvar "tdusrtimer") might     ;
;;;                     return: 0.138439                               ;
;;;                     In order to return elapsed second we determine ;
;;;                     Seconds in One Hour:                           ;
;;;                                     (* 60.00 60.00) = 3600.0       ;
;;;                     And seconds in One 24 Hour period:             ;
;;;                                     (* 24 3600.0) = 86400.0        ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  Returns a real number which is:                   ;
;;;                  Elapsed time in seconds from when the drawing was ;
;;;                  opened.                                           ;
;;;                                                                    ;
;;;          Usage: (get-utime)                                        ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun get-utime ()
  (* 86400 (getvar "tdusrtimer"))
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:AL-TST                                          ;
;;;                                                                    ;
;;;    Description:  This keeps track of the elapsed time from the     ;
;;;                  creation of 2000 circles to their erasure.        ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          get-utime                                 ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                  Elapsed time in seconds from when the drawing was ;
;;;                  opened.                                           ;
;;;                                                                    ;
;;;          Usage: (C:AL-TST) or AL-TST from the Command: prompt.     ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun c:al-tst	(/ t0 t1 cmde blipm osm asm plm)
;;; Drawing Limits, Zoom, OSMODE, and VIEWRES may all significantly affect
;;; the times it takes for these functions to return.
  (command "VIEWRES" "Y" "1000")
  (command "LIMITS" "-750,-750" "750,750")
  (command "ZOOM" "W" "-750,-750" "750,750")
  (princ "\n")
  (setq t0 (get-utime))
  (setq cmde (getvar "CMDECHO"))
  (setq blipm (getvar "BLIPMODE"))
  (setq osm (getvar "OSMODE"))
  (setq asm (getvar "AUTOSNAP"))
  (setq plm (getvar "PLINETYPE"))
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setvar "OSMODE" 0)
  (setvar "AUTOSNAP" 0)
  (setvar "PLINETYPE" 2)
  ;; Testing function place
  (aal-tst)
  (setvar "CMDECHO" cmde)
  (setvar "BLIPMODE" blipm)
  (setvar "OSMODE" osm)
  (setvar "AUTOSNAP" asm)
  (setvar "PLINETYPE" plm)
  (setq t1 (get-utime))
  (princ "\n; Time (secs): ")
  (princ (- t1 t0))
  (terpri)
  (princ)
)
;;;--------------------------------------------------------------------;
;;;       Function:  AL-TST                                            ;
;;;                                                                    ;
;;;    Description:  This function creates 2000 circles with           ;
;;;                  equidistant offsets. Each circle's visible        ;
;;;                  property is then modified to visible and then     ;
;;;                  invisible. Then each circle is erased             ;
;;;                  individually.                                     ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          change-property-s                         ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  nil                                               ;
;;;                                                                    ;
;;;          Usage: (AL-TST)                                           ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun aal-tst (/ ent centerPoint nPoint i ind offs ss cnt lwh2 lwh pt1
		pt2 pt3 pt4)

(setq offs (car (getvar "snapunit")))

(setq lwh2 (/ (setq lwh 5.0) 2.0))
 
(princ "Creating 2000 PolyLines.\n")
  (setq i 0)
  (while (< i 2000)
    ;; creates an LWPolyLine in model space
    (setq pt1 (list (* -1.0 lwh2) (* -1.0 lwh2)))
    (setq pt2  (list lwh2 (* -1.0 lwh2)))
    (setq pt3  (list lwh2 lwh2))
    (setq pt4  (list (* -1.0 lwh2) lwh2))
;   (setq entm (entmake
;		 (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(8 . "0") '(100 . "AcDbPolyline")
;		       '(38 . 0.0) '(39 . 0.0) '(90 . 4) '(70 . 1)
;		        (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)
;		 ))
;    )
;    (entupd (entlast))
    (command "PLINE" pt1 pt2 pt3 pt4 "C")
    (setq lwh2 (/ (setq lwh (+ lwh offs)) 2.0))

    (setq i (1+ i))
  )
  (setq cnt (SSLENGTH (setq ss (ssget "_X"))))
;; Changes the Color
;;  (command "_.change" ss "" "P" "C" "1" "")

  (princ "Changing 2000 LWPolylines to Red Color.\n")
  (change-property-s ss 62 1)

;; Sets to Invisible  - Cannot be Done Via AutoLisp
;;  (change-property-s ss 60 acFalse)

  (princ "Erasing 2000 LWPolylines.\n")
  (setq i (1- (SSLENGTH ss)))
  (while (>= i 0)
    (setq ent (ssname ss i)
	  i   (1- i)
    )
    (command "_.erase" ent "")
  )
  (setq ss nil)
)


;;;--------------------------------------------------------------------;
;;;       Function:  CHANGE-PROPERTY-S                                 ;
;;;                                                                    ;
;;;    Description:  This function changes the visibility for each     ;
;;;                  object in an ACAD selection set.                  ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             sset =  ACAD Selection Set.                            ;
;;;                                                                    ;
;;;             code =  ACAD group code to change                      ;
;;;                     This argument expects an Integer.              ;
;;;                                                                    ;
;;;            value =  A valid value which is allowed for the         ;
;;;                     code argument.                                 ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                  An updated list of the entity if succesfull nil   ;
;;;                  otherwise.                                        ;
;;;                                                                    ;
;;;          Usage: (change-property-s (ssget "x") 62 6)               ;
;;;                  Will change all entities to color 6 magenta       ;
;;;--------------------------------------------------------------------;
(defun change-property-s (sset code value / ent as entlist i cnt)
  (setq i (1- (SSLENGTH sset)))
  (while (>= i 0)
    (setq ent	  (ssname sset i)
	  i	  (1- i)
	  entlist (entget ent)
	  as	  (assoc code entlist)
    )
    (if	as
      (progn
       (entmod (subst (cons code value) as entlist))
       (entupd (cdr (assoc '-1 entlist)))
      )
      (progn
	(entmod (setq entlist (append entlist (list (cons code value)))))
	(entupd (cdr (assoc '-1 entlist)))
      )
    )
  )
)

;; EOF
