;;;                                                                    ;
;;;  COPYSELF.LSP                                                      ;
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
;;; This file contains various reactor utilities                       ;
;;;--------------------------------------------------------------------;

;;;--------------------------------------------------------------------;
;;;       Function:  CREATE-SAME-COPY-REACTOR                          ;
;;;                                                                    ;
;;;    Description:  This function copies a reaction to all objects    ;
;;;                  contained in the argument obj-list.               ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      add-object-to-data                            ;
;;;                      reaction                                      ;
;;;                      add-object-to-data                            ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        obj-list = a valid list of vla-objects to be modified with  ;
;;;                   the new reaction call back function in           ;
;;;                   the reaction argument.                           ;
;;;        reaction = a call back function to a vlr reactor object.    ;
;;;                                                                    ;
;;; Returned Value:  A valid vlr reactor object.                       ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;     (create-same-copy-reactor                                      ;
;;;         '(vla-Object1 vla-Object1 )                                ;
;;;             'myfunction)                                           ;
;;;--------------------------------------------------------------------;
(defun create-same-copy-reactor	(obj-list reaction)
  (function add-object-to-data)
  (vlr-object-reactor
    obj-list				;;owners
    obj-list				;;user data - receivers
    (list (cons :vlr-modified reaction)
	  (cons :vlr-copied (function add-object-to-data))
    )
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  ADD-OBJECT-TO-DATA                                ;
;;;                                                                    ;
;;;    Description:  This function will be called inside               ;
;;;                  :vlr-copied reaction for this reaction arg-list   ;
;;;                  contains only one element -  entname of new       ;
;;;                  object because we can't make VLA object from      ;
;;;                  entname inside this reaction we first store       ;
;;;                  it to DATA                                        ;
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
;;;     (add-object-to-data notifier reactor arg-list)                 ;
;;;--------------------------------------------------------------------;
(defun add-object-to-data (notifier reactor arg-list)
  (vlr-data-set
    reactor
    (cons (car arg-list) (vlr-data reactor))
  )
  ;|
  ;; We need to make the reactor sensitive to
  ;: vlr-openedForModify reaction.  We will update reactors
  ;; owners and data inside update-object-in-reactor callback.
  ;; Unfortunatively this doesn't work
  (vlr-reaction-set
    reactor
    :vlr-openedForModify
    'reactor-update-object-in-reactor
  )
|;
  ;; To overcome the problem noted above, we will update the reactors' owner
  ;; and data properties during the :vlr-commandEnded callback.
  (if (null *editor-updating-reactor*)
    (setq *editor-updating-reactor* (vlr-editor-reactor))
  )
  (if (not (vlr-added-p *editor-updating-reactor*))
    (vlr-add *editor-updating-reactor*)
  )
  (vlr-data-set
    *editor-updating-reactor*
    (cons reactor (vlr-data *editor-updating-reactor*))
  )
  (vlr-reaction-set
    *editor-updating-reactor*
    :vlr-commandended
    (function editor-update-object-in-reactor)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  REACTOR-UPDATE-OBJECT-IN-REACTOR                  ;
;;;                                                                    ;
;;;    Description:  This function updates the reactor's object        ;
;;;                  property according to the arg-list                ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      update-object-in-reactor                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;        notifier = a valid vla object.                              ;
;;;         reactor = a valid vlr object reactor.                      ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage:  	We need to make the reactor sensitive to       ;
;;;			vlr-openedForModify reaction.  We will update  ;
;;;			reactors owners and data inside                ;
;;;			update-object-in-reactor callback.             ;
;;;			Unfortunatively this doesn't work!             ;
;;;--------------------------------------------------------------------;
(defun reactor-update-object-in-reactor	(notifier reactor arg-list)
  (if (update-object-in-reactor reactor)
    ;; If all etity was replaced by VLA objects then
    ;; make reactor insensitive to :vlr-openedForModify reaction
    ;; not to catch this reaction any more
    (vlr-reaction-set reactor :vlr-openedformodify nil)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  EDITOR-UPDATE-OBJECT-IN-REACTOR                   ;
;;;                                                                    ;
;;;    Description:  This function updates the reactor's object        ;
;;;                  property according to the arg-list                ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                      update-object-in-reactor                      ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vla object reactor. Filled in by the     ;
;;; 		      calling reactor.                                 ;
;;;        arg-list = argument list filled in by the calling reactor.  ;
;;;                                                                    ;
;;; Returned Value:  none                                              ;
;;;                                                                    ;
;;;          Usage: Note: Intended to be called as a call              ;
;;;		    back function within a reactor.                    ;
;;;		    (editor-update-object-in-reactor                   ;
;;;				vla-reactor-Object                     ;
;;;					arg-list )                     ;
;;;--------------------------------------------------------------------;
(defun editor-update-object-in-reactor (reactor arg-list / new-data)
  (foreach obj-reactor (vlr-data reactor)
    (if	(not (update-object-in-reactor obj-reactor))
      (setq new-data (cons obj-reactor new-data))
    )
  )
  (vlr-data-set reactor new-data)
  (if (null new-data)
    (vlr-remove *editor-updating-reactor*)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  UPDATE-OBJECT-IN-REACTOR                          ;
;;;                                                                    ;
;;;    Description:  This function updates the reactor's object        ;
;;;                  property.                                         ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         reactor = a valid vla object reactor                       ;
;;;                                                                    ;
;;; Returned Value:  returns the status of not-release. If not-release ;
;;;                  is nil then the return value is T otherwise nil.  ;
;;;                                                                    ;
;;;          Usage: (update-object-in-reactor vla-reactor-Object)      ;
;;;--------------------------------------------------------------------;
(defun update-object-in-reactor	(reactor / vlaobj new-data not-release)
  ;;edit reactor on fly!
  (foreach obj (vlr-data reactor)
    (cond ((eq (type obj) 'ename)
	   (cond ;; now we can try to make VLA object from entity
		 ((setq vlaobj (vlax-ename->vla-object obj))
		  ;; to make it notify other circles add it to list of owners
		  (setq new-data (cons vlaobj new-data))
		  (vlr-owner-add reactor vlaobj)
		 )
		 (t
		  (setq	new-data    (cons obj new-data)
			not-release t
		  )
		 )
	   )
	  )
	  (t (setq new-data (cons obj new-data)))
    )
  )
  ;; to make it recieve notification from others circles
  ;; add it to the list of recievers
  (vlr-data-set reactor (reverse new-data))
  (not not-release)
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:COPYSELF-TEST                                   ;
;;;                                                                    ;
;;;    Description:  This function aids in the creation of a circle    ;
;;;                  object which will contain reactors for:           ;
;;;                  COPY, MIRROR or ARRAY                             ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    create-same-copy-reactor                        ;
;;;                    add-circle                                      ;
;;;                    reactor-make-same-radius-color                  ;
;;;                                                                    ;
;;;                                                                    ;
;;;      Arguments:  none                                              ;
;;;                                                                    ;
;;; Returned Value:  A valid reactor object such as:                   ;
;;;                 #<VLR-Object-reactor>                              ;
;;;                                                                    ;
;;;          Usage: (C:COPYSELF-TEST) or COPYSELF-TEST from            ;
;;;                 the ACAD Command: prompt.                          ;
;;;--------------------------------------------------------------------;
(defun C:COPYSELF-TEST (/ vla-obj reactor)
  (setq vla-obj (ADD-CIRCLE))
  (if vla-obj
    (progn
      (vla-put-color vla-obj acred)
      (setq reactor (create-same-copy-reactor
		      (list vla-obj)
		      (function reactor-make-same-radius-color)
		    )
      )
    )
  )
  reactor
)

;;;--------------------------------------------------------------------;
;;;       Function:  C:COPYSELF-INFO                                   ;
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
(defun C:COPYSELF-INFO ()
  (terpri)
  (textscr)
  (princ
    "\nThis test demonstrates a special reactor for a circle.  You will"
  )
  (princ
    "\nbe prompted to define the center point and radius for a circle,"
  )
  (princ
    "\nwhich is initially colored red.  A reactor is bound to it."
  )
  (princ
    "\nAny subsequent circles created by COPY, MIRROR or ARRAY of the"
  )
  (princ
    "\noriginal circle, will be updated to match the radius and color"
  )
  (princ "\nof the original circle.")
  (princ "\n\nRun COPYSELF-TEST to run the demonstration.")
  (terpri)
  (princ)
)

;;;--------------------------------------------------------------------;
;;; Add the functions within this file to the global functions list    ;
;;; to be used by the C:REACT-TEST-INFO function in R-INFO.LSP         ;
;;;--------------------------------------------------------------------;
(setq *REACT-TEST-COMMANDS-INFO*
       (cons (list "COPYSELF-TEST" "COPYSELF-INFO")
	     *REACT-TEST-COMMANDS-INFO*
       )
)

