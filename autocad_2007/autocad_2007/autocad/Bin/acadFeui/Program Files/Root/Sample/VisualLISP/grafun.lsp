;;;                                                                    ;
;;;  GRAFUN.LSP                                                        ;
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
;;;  This file demonstrates adding a 3D mesh utilizing activeX methods ;
;;;  and native AutoLISP command function. The Mesh is produced by     ;
;;;  applying a function to the beginning coordinate for the mesh,     ;
;;;  a length in Y direction for the mesh, and a length in the x       ;
;;;  for the mesh.                                                     ;
;;;  coordinate.                                                       ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;; General Note:  ACAD user timer must be ON                          ;
;;; (command "_.time" "_ON" "")                                        ;
;;;--------------------------------------------------------------------;

;;; Load the AutoCAD 2000 COM object model functions here
(vl-load-com)

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
;;; Returned Value:  Returns a real number whose meaning is:           ;
;;;                  Elapsed time in seconds from when the drawing was ;
;;;                  opened.                                           ;
;;;                                                                    ;
;;;          Usage: (get-utime)                                        ;
;;;                                                                    ;
;;;--------------------------------------------------------------------;
;;; returns the user time
(defun get-utime ()
  (* 86400 (getvar "tdusrtimer"))
  )
;;;--------------------------------------------------------------------;
;;;       Function:  AL-GRAFUN                                         ;
;;;                                                                    ;
;;;    Description:  Draws a mesh utilizing the AutoLISP Command       ;
;;;                  function according to the arguments supplied.     ;
;;;                  This function prints elapsed time in minutes      ;
;;;                  and seconds during processes and returns a list   ;
;;;                  points which were used to create the 3D mesh.     ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          get-utime                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;                  Fun = Function to apply.                          ;
;;;                  xy0 = Lower left corner of rectangular mesh.      ;
;;;                  xy1 = Upper right corner of rectangular mesh.     ;
;;;                  dx  = Density factor for x direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                  dy  = Density factor for y direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                                                                    ;
;;; Returned Value:  A list points which were used to create           ;
;;;                  the 3D mesh.                                      ;
;;;                                                                    ;
;;;Dislayed Values: Displays time of evaluation in the                 ;
;;;                 following format:                                  ;
;;;                 (TT == NCT + DMT)                                  ;
;;;                 Where:                                             ;
;;;                 TT  = Total elapsed time.                          ;
;;;                 NCT = Elapsed time from start of function which    ;
;;;                       draws the mesh until the function has        ;
;;;                       finished its number crunching.               ;
;;;                 DMT = Elapsed time of the mesh drawing portion     ;
;;;                       after number crunching has finished.         ;
;;;                                                                    ;
;;;          Usage: (al-grafun fun xy0 xy1 dx dy)                      ;
;;;--------------------------------------------------------------------;
(defun aal-grafun (fun xy0 xy1 dx dy
	/ x0 y0 x1 y1 x_coord y_coord z_coord cx cy pts ids t0 t1 t2 t3 ce bm os)
  (setq t0 (get-utime))
  (setq x0 (car xy0)
	y0 (cadr xy0)
	x1 (car xy1)
	y1 (cadr xy1)
	)
  (setq x_coord x0
	cx 0)
  (while (<= x_coord x1)
    (setq y_coord y0
	  cy 0 )
    (while (<= y_coord y1)
      (setq z_coord (fun x_coord y_coord))
      (setq pts (cons (list x_coord y_coord z_coord) pts))
      (setq y_coord (+ y_coord dy)
	    cy (1+ cy) )
      )
    (setq x_coord (+ x_coord dx)
	  cx (1+ cx) )
    )
  (setq pts (reverse pts))
  (setq t1 (get-utime))

  (setq ce (getvar "CMDECHO"))
  (setq bm (getvar "BLIPMODE"))
  (setq os (getvar "OSMODE"))
  (setvar "CMDECHO" 0)
  (setvar "BLIPMODE" 0)
  (setvar "OSMODE" 0)
  (command "_.3dmesh" cx cy)
  (FOREACH p pts (command p))
  (setq pts (entlast))
  (command "_.CIRCLE" '(5.0 5.0 0.0) 5.0)
  (setvar "CMDECHO" ce)
  (setvar "BLIPMODE" bm)
  (setvar "OSMODE" os)
  (setq t2 (get-utime))

  ;(command "")
  
  (princ "\n; Time: ")
  (princ (list (- t2 t0) '== (- t1 t0) '+ (- t2 t1)))
  pts
  )
;;;--------------------------------------------------------------------;
;;;       Function:  VL-GRAFUN                                         ;
;;;                                                                    ;
;;;    Description:  Draws a mesh utilizing Visual LISP Automation     ;
;;;                  extensions (known as activeX methods) according   ;
;;;                  to the arguments supplied.                        ;
;;;                  This function prints elapsed time in minutes      ;
;;;                  and seconds during processes and returns a list   ;
;;;                  points which were used to create the 3D mesh.     ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          get-utime                                 ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;                  Fun = Function to apply.                          ;
;;;                  xy0 = Lower left corner of rectangular mesh.      ;
;;;                  xy1 = Upper right corner of rectangular mesh.     ;
;;;                  dx  = Density factor for x direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                  dy  = Density factor for y direction.             ;
;;;	                       The smaller the number denser the mesh. ;
;;;                                                                    ;
;;; Returned Value:  A list points which were used to create           ;
;;;                  the 3D mesh.                                      ;
;;;Dislayed Values: Displays time of evaluation in the                 ;
;;;                 following format:                                  ;
;;;                 (TT == NCT + DMT)                                  ;
;;;                 Where:                                             ;
;;;                 TT  = Total elapsed time.                          ;
;;;                 NCT = Elapsed time from start of function which    ;
;;;                       draws the mesh until the function has        ;
;;;                       finished its number crunching.               ;
;;;                 DMT = Elapsed time of the mesh drawing portion     ;
;;;                       after number crunching has finished.         ;
;;;                                                                    ;
;;;          Usage: (vl-grafun fun xy0 xy1 dx dy)                      ;
;;;--------------------------------------------------------------------;
(defun avl-grafun (fun xy0 xy1 dx dy
	/ x0 y0 x1 y1 x_coord y_coord z_coord cx cy pts t0 t1 t2 t3 *ModelSpace*)
  (setq t0 (get-utime))
  (setq x0 (car xy0)
	y0 (cadr xy0)
	x1 (car xy1)
	y1 (cadr xy1)
	)
  (setq x_coord x0
	cx 0)
  (while (<= x_coord x1)
    (setq y_coord y0
	  cy 0 )
    (while (<= y_coord y1)
      (setq z_coord (fun x_coord y_coord))
      (setq pts (vl-list* z_coord y_coord x_coord pts))
      (setq y_coord (+ y_coord dy)
	    cy (1+ cy) )
      )
    (setq x_coord (+ x_coord dx)
	  cx (1+ cx) )
    )
  (setq pts (reverse pts))

;; New for AutoCAD 2000 - Create and Fill A Variant Array for the Mesh Points
  (setq ptlstlen (length pts))
  (setq PointDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- ptlstlen))))
  (vlax-safearray-fill PointDataA pts)
  (setq PointData (vlax-make-variant PointDataA (logior vlax-vbarray vlax-vbDouble)))
;; New for AutoCAD 2000 - Create and Fill A Variant Array for the Mesh Points

  (setq t1 (get-utime))
  (setq *ModelSpace* (vla-get-ModelSpace
		       (vla-get-ActiveDocument
		         (vlax-get-acad-object) )))
  
  (setq pmesh (vla-Add3Dmesh *ModelSpace* cx cy PointData) )
  (vla-Update pmesh)
  (vla-AddCircle *ModelSpace* (vlax-3d-Point '(5.0 5.0 0.0)) 5.0)
  (setq t2 (get-utime))
  (princ "\n; Time: ")
  (print (list (- t2 t0) '== (- t1 t0) '+ (- t2 t1)))
  pmesh
  )
;;;--------------------------------------------------------------------;
;;;       Function:  SQR                                               ;
;;;                                                                    ;
;;;    Description:  Function to return the square of a number.        ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             x   = a valid number.                                  ;
;;;                   Note: no error checking is performed             ;
;;;                                                                    ;
;;; Returned Value: a number either real or integer.                   ;
;;;                 If an integer is passed an integer is returned.    ;
;;;                 If a real number is passed a real is returned.     ;
;;;                                                                    ;
;;;          Usage: (sqr 7) returns 49                                 ;
;;;                 (sqr 7.0) returns 49.00                            ;
;;;--------------------------------------------------------------------;
(defun sqr(x) (* x x))

;;;--------------------------------------------------------------------;
;;;       Function:  SPHE5                                             ;
;;;                                                                    ;
;;;    Description:  Function which calculates a half of sphere.       ;
;;;                  Where: C(5,5,0), R(5)                             ;
;;;                  and Z coordinate is:                              ;
;;; 		            Z = F(X,Y)                                 ;
;;;                                                                    ;
;;;                  This function provides a seed algorithim to       ;
;;;                  calculate 1/2 a sphere to utilize within          ;
;;;                  our 3dMesh creation functions:                    ;
;;;                     vl-grafun                                      ;
;;;                     al-grafun                                      ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          sqr                                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               x_coord  = X coordinate of point to determine the Z  ;
;;;                    coordinate for the mesh.                        ;
;;;               y_coord  = Y coordinate of point to determine the Z  ;
;;;                    coordinate for the mesh.                        ;
;;;                                                                    ;
;;; Returned Value:  Z coordinate.                                     ;
;;;                                                                    ;
;;;          Usage: (sphe5 x_coord y_coord)                            ;
;;;--------------------------------------------------------------------;
(defun sphe5 (x_coord y_coord)
  (sqrt (max 0.0 (- 25 (sqr (- x_coord 5)) (sqr (- y_coord 5)))))
  )

;;;--------------------------------------------------------------------;
;;;       Function:  AZVPOINT                                          ;
;;;                                                                    ;
;;;    Description:  This function changes the vpoint of the current   ;
;;;                  viewport to values represented in pt argument.    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;               pt = A list of three numbers which represent view    ;
;;;                    coordinate and direction.                       ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (zvpoint '( 0 0 1))                                ;
;;;--------------------------------------------------------------------;
(defun zvpoint (pt / ce)
  (setq ce (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (COMMAND "_.ZOOM" "_E")
  (COMMAND "_.VPOINT" pt)
  (setvar "CMDECHO" ce)
  (princ)
  )
;;;--------------------------------------------------------------------;
;;;       Function:  C:AL-GF                                           ;
;;;                                                                    ;
;;;    Description:  This excecutes the native AutoLISP function to    ;
;;;                  create a 3D mesh.                                 ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          al-grafun                                 ;
;;;                          zvpoint                                   ;
;;;                          sphe5                                     ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (c:AL-GF) or AL-GF from the ACAD comand prompt.    ;
;;;--------------------------------------------------------------------;
(defun C:AL-GF ()
  (aAL-GRAFUN SPHE5 '(-1 -1) '(11 8) 0.2 0.2)
  (ZVPOINT '(10 10 3))
  )
;;;--------------------------------------------------------------------;
;;;       Function:  C:VLA-GF                                          ;
;;;                                                                    ;
;;;    Description:  This excecutes ActiveX Automation functions to    ;
;;;                  create a 3D mesh.                                 ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          vl-grafun                                 ;
;;;                          zvpoint                                   ;
;;;                          sphe5                                     ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (c:VLA-GF) or VLA-GF from the ACAD comand prompt.  ;
;;;--------------------------------------------------------------------;
;;; Use ActiveX Automation. 
;;; Returns a response similar to this:(your may vary)
;;;  "; Time: 
;;; (1.54 == 0.71 + 0.83) 
(defun C:VLA-GF ()
  (aVL-GRAFUN SPHE5 '(-1.0 -1.0) '(11.0 8.0) 0.2 0.2)
  (ZVPOINT '(10 10 3))
  )

;;;--------------------------------------------------------------------;
;;;                                                                    ;
;;; On Your Own:                                                       ;
;;;    Try out the following code fragment in the Visual LISP console  ;
;;;    after loading this file into the editor.                        ;
;;;  (VL-GRAFUN SPHE5 '(0 0) '(4 4) 0.2 0.2)                           ;
;;;--------------------------------------------------------------------;

;;; Print an aid for the command above.
(princ "; To test: AL-GF, VLA-GF, (c:al-gf), or (c:vla-gf) \n")
(princ)

;;; EOF

