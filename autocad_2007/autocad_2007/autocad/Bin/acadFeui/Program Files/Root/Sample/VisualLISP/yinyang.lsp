;;;                                                                    ;
;;;  YINYANG.LSP                                                       ;
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

;;;--------------------------------------------------------------------;
;;;  This file is referenced from the Visual LISP documentation        ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  YINYANG                                           ;
;;;                                                                    ;
;;;    Description:  Draws a Yin-Yang symbol.                          ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;Dislayed Values:  A Yin-Yang symbol containing arcs and circles.    ;
;;;                                                                    ;
;;;          Usage:  (yinyang)                                         ;
;;;--------------------------------------------------------------------;
(defun yinyang (/ origin radius i-radius half-r origin-x origin-y os)
  (setq os (getvar "OSMODE"))                  ;; Save OSNAP mode
  (setvar "OSMODE" 0)                          ;; Turn off OSNAP
  (setq origin (getpoint "\nOrigin of inyn sign: "))
  (setq radius (getdist "\nRadius of inyn sign: " origin))
  (setq i-radius (getdist "\nRadius of internal circle: "
    origin)
  )

  (if (> i-radius radius) (setq i-radius (/ radius 4)))
  
  (setq half-r (/ radius 2))
  (setq origin-x (car origin))
  (setq origin-y (cadr origin))

  (command "_.CIRCLE" origin radius)
  (command "_.ARC"
       "_C"
       (list origin-x (+ origin-y half-r))     ;;center
       (list origin-x (+ origin-y radius))     ;;start point
       origin                                  ;;end point
  )
  (command "_.ARC"
       "_C"
       (list origin-x (- origin-y half-r))     ;;center
       (list origin-x (- origin-y radius))     ;;start point
       origin                                  ;;end point
  )
  (command "_.CIRCLE"
       (list origin-x (+ origin-y half-r))
       ;;centre
       i-radius
  )
  (command "_.CIRCLE"
       (list origin-x (- origin-y half-r))
       ;;centre
       i-radius
  )
  (setvar "OSMODE" os)                         ;; Restore OSNAP mode
  (princ)
)

