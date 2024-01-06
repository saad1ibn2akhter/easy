;;;                                                                    ;
;;;  VLA-TST.LSP                                                       ;
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

;;; Load the AutoCAD 2000 COM object model functions here
(vl-load-com)

;;;--------------------------------------------------------------------;
;;; General Note: The command function c:vla-tst requires a function   ;
;;;               named get-utime located in file al-tst.lsp           ;
;;; ******  THIS FILE IS A MEMBER OF THE VLA-TST PROJECT.  ********    ;
;;;--------------------------------------------------------------------;
;;;  This file demonstrates adding 2000 circles using ActiveX          ;
;;;  Automation. Each circle's visible property is then modified       ;
;;;  to visible and then invisible. Then each circle is erased         ;
;;;  individually. An elapsed timer is displayed after the creation    ;
;;;  and deletion of the circles created.                              ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  VLA-TST                                           ;
;;;                                                                    ;
;;;    Description:  This function creates 2000 circles with           ;
;;;                  equidistant offsets using ActiveX Automation.     ;
;;;                  Each circle's visible property is then modified   ;
;;;                  to visible and then invisible. Then each circle   ;
;;;                  is erased individually.                           ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  last erased vla object                            ;
;;;                                                                    ;
;;;          Usage: (vla-tst)                                          ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun vvla-tst (/ acadApp acadDoc mSpace i plObj offs
		nPoint lwh2 lwh j saPntLst plen pntlist varPntLst ent
	       )
  (Setq	acadApp	    (vlax-get-acad-object)
	acadDoc	    (vla-get-ActiveDocument acadApp)
	mSpace	    (vla-get-ModelSpace acadDoc)
	offs	    (car (getvar "snapunit"))
  )
(setq lwh2 (/ (setq lwh 5.0) 2.0))

  (princ "Creating 2000 LWPolylines.\n")

  ;; Create 2000 Polylines in Model space
  (setq i 0)
   (setq saPntLst (vlax-make-safearray vlax-vbDouble '(0 . 7)))
   (while (< i 2000)
   (setq plen  (length
    (setq pntlist
	   (list (* -1.0 lwh2) (* -1.0 lwh2)
                  lwh2 (* -1.0 lwh2)
                  lwh2 lwh2
                 (* -1.0 lwh2) lwh2
           )
   )
   ))
   (setq j 0)
   (while (< j plen)
     (vlax-safearray-put-element saPntLst j (nth j pntlist))
     (setq j (1+ j))
   )
   (setq varPntLst (vlax-make-variant saPntLst (logior vlax-vbArray vlax-vbDouble)))
     ;; creates an LightWeightPolyline object in model space
    (setq plObj (vla-AddLightWeightPolyline mSpace varPntLst))
    (vla-Put-Closed plObj acTrue)
    (vla-Update plObj)
    (setq lwh2 (/ (setq lwh (+ lwh offs)) 2.0))
    (setq i (1+ i))
 )

  (princ "Changing 2000 LWPolylines to Red Color.\n")

  ;; Change color
  (vlax-for ent mSpace
    (vla-put-Color ent 1)
     (vla-Update ent)
  )

    (princ "Erasing 2000 LWPolylines.\n")

  ;; Erase all objects
  (vlax-for ent mSpace
    (vla-Erase ent)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:VLA-TST                                         ;
;;;                                                                    ;
;;;    Description:  This keeps track of the elapsed time from the     ;
;;;                  creation of 2000 circles to their erasure.        ;
;;;                                                                    ;
;;;      Arguments:  None                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                  Elapsed time in seconds from when the drawing was ;
;;;                  opened.                                           ;
;;;                                                                    ;
;;;          Usage: (C:VLA-TST) or VLA-TST from the Command: prompt.   ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
(defun c:vla-tst (/ t0 t1 cmde blipm osm asm)
;;; Drawing Limits, Zoom, OSMODE, and VIEWRES may all significantly affect
;;; the times it takes for these functions to return.
  (command "VIEWRES" "Y" "1000")
  (command "LIMITS" "-750,-750" "750,750")
  (command "ZOOM" "W" "-750,-750" "750,750")
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
  (vvla-tst)
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

(princ "\n; To test: AL-TST, VLA-TST, (c:al-tst), or (c:vla-tst)\n")
(princ)

;;; EOF
