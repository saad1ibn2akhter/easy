;;;                                                                    ;
;;;  PIPE.LSP                                                          ;
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
;;; General Note:  THIS FILE IS A MEMBER OF THE REAC-TST PROJECT       ;
;;;--------------------------------------------------------------------;
;;;    This file contains a demonstration of a path based on a         ;
;;;    polyline.                                                       ;
;;;                                                                    ;
;;;    Creates a path - polyline and a base curve - circle.            ;
;;;                                                                    ;
;;;    The 3d figure will be created as an extrusion of the circle     ;
;;;    along the polyline (they are colored red). When you change the  ;
;;;    path or the circle the 3d figure will be updated.               ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  GET-VECTOR-ALONG                                  ;
;;;                                                                    ;
;;;    Description:  This function performs a subtraction of the       ;
;;;                  startPoint and EndPoint of a vla line object.     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            line = a valid vla line object.                         ;
;;;                                                                    ;
;;; Returned Value:  A vector list.                                    ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (get-vector-along vla-line-Object )              ;
;;;--------------------------------------------------------------------;
(defun get-vector-along	(line / from to)
  (setq	from (vla-get-startPoint line)
	to   (vla-get-EndPoint line)
  )
  (mapcar '- to from)
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-PIPE                                         ;
;;;                                                                    ;
;;;    Description:  This function extrudes a circle in the direction  ;
;;;                  of a path line. Note: Line and circle can not be  ;
;;;                  coplanar.                                         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     get-model-space                                ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            line = a valid vla line object.                         ;
;;;          circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A list of a vla 3d Solid Object. Such as:         ;
;;;                  (#<VLA-OBJECT IAcad3DSolid 02d23f2c>)             ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                   (make-pipe vla-line-Object vla-circle-object)    ;
;;;--------------------------------------------------------------------;
(defun make-pipe
       (line circle / mSpace region-list normal-vector exrude-list)
  (vla-move circle
	    (vlax-3d-point (vlax-curve-getstartPoint circle))
	    (vlax-3d-point (vlax-curve-getstartPoint line))
  )
(setvar "ISOLINES" 25)
(setq circleAa (vlax-make-safearray vlax-vbObject '(0 . 0)))
(vlax-safearray-put-element circleAa 0 circle)
(setq circleA (vlax-make-variant circleAa (logior vlax-vbarray vlax-vbObject)))  

  (setq	mSpace	    (get-model-space))
  (setq	region-list (vlax-safearray->list
		      (vlax-variant-value
			(vla-AddRegion mSpace circleA)
		      )
		     )
  )
  (setq	exrude-list (mapcar
		      (function
			(lambda	(region)
			  (vla-AddExtrudedSolidAlongPath mSpace region line)
			)
		      )
		      region-list
		    )
  )
  (foreach region region-list
    (if	(not (vlax-erased-p region))
      (vla-Erase region)
    )
  )
  exrude-list
)

;;;--------------------------------------------------------------------;
;;;       Function:  PROPERTY-CHANGED-P                                ;
;;;                                                                    ;
;;;    Description:  This function serves as a predicate. Testing for  ;
;;;                  the integrity of the data retreived from the      ;
;;;                  object to be the same as the supplied property.   ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-obj = a valid vla object.                              ;
;;;        property = a property list to compare.                      ;
;;;                                                                    ;
;;; Returned Value:  T if the property has changed. Nil otherwise.     ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (property-changed-p vla-line-Object prop-List)     ;
;;;--------------------------------------------------------------------;
(defun property-changed-p (vla-obj property)
  (and (eq 'VLA-OBJECT (type vla-obj))
       (vlax-read-enabled-p vla-obj)
       (vlax-property-available-p vla-obj property)
       (not (equal (vlax-get vla-obj property)
		   (vlax-ldata-get vla-obj property)
	    )
       )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-PIPE-CIRCLE                               ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event.                              ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     reactor-pipe-line                              ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;     (reactor-pipe-circle notifier reactor arg-list)                ;
;;;--------------------------------------------------------------------;
(defun reactor-pipe-circle (notifier reactor arg-list)
  (reactor-pipe-line
    (vlax-ldata-get notifier "line")
    reactor
    arg-list
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-PIPE-LINE                                 ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event and is invoked by             ;
;;;                  reactor-pipe-circle reactor call back. Its        ;
;;;                  purpose is to modify the reactor which            ;
;;;                  was invoked.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     property-changed-p                             ;
;;;                     change-pipe-list                               ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object. Filled in by the calling     ;
;;;                   reactor.                                         ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;       (reactor-pipe-line notifier reactor arg-list)                ;
;;;--------------------------------------------------------------------;
(defun reactor-pipe-line (notifier reactor arg-list)
  (if (or t
	  (property-changed-p notifier "StartPoint")
	  (property-changed-p notifier "EndPoint")
      )
    ;;    (change-pipe notifier)
    ;|{|;
    (progn
      (if (null *editor-pipe-updating-reactor*)
	(setq *editor-pipe-updating-reactor* (VLR-Editor-reactor))
      )
      (if (not (VLR-added-p *editor-pipe-updating-reactor*))
	(vlr-add *editor-pipe-updating-reactor*)
      )
      (VLR-Data-Set
	*editor-pipe-updating-reactor*
	(cons notifier (VLR-Data *editor-pipe-updating-reactor*))
      )
      (vlr-reaction-set
	*editor-pipe-updating-reactor*
	:vlr-commandEnded
	(function change-pipe-list)
      )
    )
    ;|}|;
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  CHANGE-PIPE-LIST                                  ;
;;;                                                                    ;
;;;    Description:  This function will be called inside a             ;
;;;                  :vlr-modified event and is invoked by             ;
;;;                  reactor-pipe-circle reactor call back. Its        ;
;;;                  purpose is to modify the reactor which            ;
;;;                  was invoked.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     change-pipe                                    ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vlr object reactor. Filled in by the     ;
;;;                   calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                   Filled in by the calling reactor.                ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:  Intended to be called from a reactor call back.   ;
;;;                 (change-pipe-list reactor arg-list)                ;
;;;--------------------------------------------------------------------;
(defun change-pipe-list	(reactor arg-list)
  (foreach pipe	(VLR-Data reactor)
    (change-pipe pipe)
  )
  (VLR-Data-Set reactor nil)
)

;;;--------------------------------------------------------------------;
;;;       Function:  CHANGE-PIPE                                       ;
;;;                                                                    ;
;;;    Description:  This function will modify the pipe created        ;
;;;                  from the path line.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     make-pipe                                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         vla-obj = a valid vla object.                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object.                               ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (change-pipe vla-obj)                              ;
;;;--------------------------------------------------------------------;
(defun change-pipe (vla-obj)
  (if (and vla-obj (not (vlax-erased-p vla-obj)))
    (progn
      (foreach extrudion (vlax-ldata-get vla-obj "extrude-list")
	(if (not (vlax-erased-p extrudion))
	  (vla-Erase extrudion)
	)
      )
      (if (not (vlax-erased-p vla-obj))
	(vlax-ldata-put
	  vla-obj
	  "extrude-list"
	  (make-pipe vla-obj (vlax-ldata-get vla-obj "circle"))
	)
      )
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  MAKE-PIPE-REACTOR                                 ;
;;;                                                                    ;
;;;    Description:  This function will modify the pipe created        ;
;;;                  from the path line.                               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     change-pipe                                    ;
;;;                     reactor-pipe-line                              ;
;;;                     reactor-pipe-circle                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;            line = a valid vla line object.                         ;
;;;          circle = a valid vla circle object.                       ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr object reactor                        ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (make-pipe-reactor                                 ;
;;;                        vla-line-object vla-circle-Object)          ;
;;;--------------------------------------------------------------------;
(defun make-pipe-reactor (line circle)
  (vlax-ldata-put line "circle" circle)
  (vlax-ldata-put circle "line" line)
  (change-pipe line)
  (list
    (VLR-Object-reactor
      (list line)
      nil
      (list (cons :vlr-modified (function reactor-pipe-line)))
    )
    (VLR-Object-reactor
      (list circle)
      nil
      (list (cons :vlr-modified (function reactor-pipe-circle)))
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-PIPE-BASE                                     ;
;;;                                                                    ;
;;;    Description:  This function is responsible for building an      ;
;;;                  ActiveX circle object for the pipe base.          ;
;;;                                                                    ;
;;;                  Note: It's possible use (entsel).                 ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     get-model-space                                ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla circle object.                        ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (get-pipe-base)                                    ;
;;;--------------------------------------------------------------------;
(defun get-pipe-base (/ obj)
  (setq	obj (VLA-addCIRCLE
	      (get-model-space)
	      (vlax-3d-point '(5.0 5.0 0.0))
	      5
	    )
  )
  (vla-put-Normal obj (vlax-3d-point '(0.0 0.0 1.0)))
  (vla-put-Color obj acred)
  obj
)

;;;--------------------------------------------------------------------;
;;;       Function:  GET-PIPE-EXTRUDE                                  ;
;;;                                                                    ;
;;;    Description:  This function is responsible for building an      ;
;;;                  ActiveX object for the pipe extrusion.            ;
;;;                                                                    ;
;;;                  Note: It's possible use (entsel).                 ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                     get-model-space                                ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla polyline object.                      ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;                 (get-pipe-extrude)                                 ;
;;;--------------------------------------------------------------------;
(defun get-pipe-extrude (/ obj Points ptlstlen PointDataA PointData)

(setq Points (mapcar 'float '(0 0 0  0 10 0  -7 23 0  -10 30 0)))
(setq ptlstlen (length Points))
(setq PointDataA (vlax-make-safearray vlax-vbDouble (cons 0 (1- ptlstlen))))
(vlax-safearray-fill PointDataA Points)
(setq PointData (vlax-make-variant PointDataA (logior vlax-vbarray vlax-vbDouble)))

  (setq obj (vla-Addpolyline
              (get-model-space)
              ;; all points need to be reals
              PointData
            )
  )
  ;;; all normals need to be reals
  (vla-put-Normal obj (vlax-3d-point '(0.0 1.0 0.0)))
  (vla-put-Color obj acred)
  obj
)


;;;--------------------------------------------------------------------;
;;;       Function:  C:PIPE-TST                                        ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object and a path which will create a "smart"     ;
;;;                  pipe able to be modified.                         ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    get-pipe-extrude                                ;
;;;                    get-pipe-base                                   ;
;;;                    make-pipe-reactor                               ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid vla object reactor.                       ;
;;;                                                                    ;
;;;          Usage: (C:PIPE-TST) or PIPE-TST from                      ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:PIPE-TST (/ line circle)
  (setq line (get-pipe-extrude))
  (setq circle (get-pipe-base))
  (if (and line circle)
    (make-pipe-reactor line circle)
  )
(princ)
  )

;;;--------------------------------------------------------------------;
;;;       Function:  C:PIPE-INFO                                       ;
;;;                                                                    ;
;;;    Description:  This function displays a help file in the ACAD    ;
;;;                  Command: prompt.                                  ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: (C:COPYSELF-INFO) or COPYSELF-INFO from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:PIPE-INFO ()
  (textscr)
    (princ "\nThis file contains a demonstration of a path based on a polyline.")
    (princ "\nCreates a path - polyline and a base curve - circle. ")
    (princ "\n")
    (princ "\nThe 3d figure will be created as an extrusion of the circle ")
    (princ "\nalong the polyline (they are colored red). When you change the ")
    (princ "\npath or the circle the 3d figure will be updated.")
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "PIPE-TST" "PIPE-INFO")
	     *REACT-TEST-COMMANDS-INFO*
       )
)
