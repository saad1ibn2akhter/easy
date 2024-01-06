;;;                                                                    ;
;;;  GPREACT.LSP                                                       ;
;;;                                                                    ;
;;;  Copyright 1987, 1988, 1990, 1992, 1994, 1996, 1997, 1998          ;
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
;;;  This file is from the Garden Path tutorial, and represents the    ;
;;;  state of the application at the end of Lesson 6.  Use this file   ;
;;;  to check your work, or to start off Lesson 7 with the code as it  ;
;;;  appears in the tutorial.                                          ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  General Notes:                                                    ;
;;;--------------------------------------------------------------------;
;;;  After the execution of these reactor functions, you might         ;
;;;  experience difficulty in returning to Visual Lisp.  If this does  ;
;;;  happen, type VLide at the AutoCAD command prompt and focus will   ;
;;;  be returned to Visual Lisp.                                       ;
;;;--------------------------------------------------------------------;
;;;  There are three types of reactors which we will be using:         ;
;;;          1. an object reactor                                      ;
;;;          2. a command reactor                                      ;
;;;          3. a drawing reactor                                      ;
;;;  We will define two functions that will notify us when the user    ;
;;;  has modified or changed the garden path.                          ;
;;;--------------------------------------------------------------------;
;;; Object Reactor                                                     ;
;;;----------------------|---------------------|-----------------------;
;;; Event                |Function to call     | Description           ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-modified        |gp:outline-changed   | Function called       ;
;;;                      |                     | when object declared  ;
;;;                      |                     | in owners is modified ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-erased	         |gp:outline-erased    | Function called       ;
;;;                      |                     | when object is erased ;
;;;----------------------|---------------------|-----------------------;
;;;                                                                    ;
;;; Command Reactor                                                    ;
;;;----------------------|---------------------|-----------------------;
;;; Event                |Function to call     | Description           ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-commandWillStart|gp:command-will-start| Function called when  ;
;;;                      |                     | a command is typed    ;
;;;                      |                     | at the command prompt ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-commandEnded	 |gp:command-ended     | Function called when  ;
;;;                      |                     | a command has ended   ;
;;;----------------------|---------------------|-----------------------;
;;;                                                                    ;
;;; Drawing Reactor                                                    ;
;;;----------------------|---------------------|-----------------------;
;;; Event                |Function to call     | Description           ;
;;;----------------------|---------------------|-----------------------;
;;; :vlr-beginClose      |gp:clean-all-reactors| Function to clean all ;
;;;                      |                     | existing reactors     ;
;;;                      |                     | before ACAD exits     ;
;;;--------------------------------------------------------------------;
;;; Since reactor events occur in sequence (commandWillStart occuring  ;
;;; before the object modified reactor, for example), we need a few    ;
;;; global variables to keep track of what changes are occuring to the ;
;;; path.  The following globals are used:                             ;
;;;          *polyToChange*                                            ;
;;;          *reactorsToRemove*                                        ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;     Function: gp:command-will-start                                ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a reactor to any command starting            ;
;;;--------------------------------------------------------------------;
;;;  This is the function where we figure out what *will* be happening ;
;;;  to the garden path (not what *is* happening).  Reset the global   ;
;;;  variables *polyToChange* and *reactorsToRemove* so that           ;
;;;  subsequent reactor events will perform the correct actions.       ;
;;;  (This is necessary since this function may be called more than    ;
;;;  once, and the *polyToChange* pointer could be pointing to a       ;
;;;  polyline other than the one the user just selected for editing!)  ;
;;;  Also, reset the *reactorsToRemove* global, for the same reason.   ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:command-will-start (reactor command-list)
  ;; Reset the global variable
  (setq	*polyToChange*  nil
	*reactorsToRemove* nil
  ) ;_ end of setq

  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ (list 'gp:command-will-start reactor command-list))
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (alert
    (strcat
      (format-reactor-message 'gp:command-will-start)
      "\n\tThis reactor-callback function's responsibility will be to:\n"
      "\n\tReset any of the global variables used within reactor functions"
      "\n\tto an initial nil state.  It will also note what AutoCAD command"
      "\n\thas been issued and respond accordingly.\n"
      "\n\tAssociated Actions:"
      "\n\tIf a U, UNDO, STRETCH, MOVE, ROTATE, or SCALE command is being"
      "\n\tstarted, break the associativity between the tiles and the "
      "\n\tpolyline boundary."
    ) ;_ end of strcat
  ) ;_ end of alert
  (princ)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:outline-erased                                    ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is triggered when the path     ;
;;;               outline is being erased.  If this happens, we need to;
;;;                    1) Erase all of the tiles (the user is taking   ;
;;;                       care of the rest of the work)                ;
;;;                    2) Set a global variable that stores the        ;
;;;                       reactor assigned to this polyline, so that   ;
;;;                       it can be removed when command-ended fires   ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:outline-erased (outlinePoly reactor parameterList)
  ;; Store the reactor assigned to this entity to the global
  ;; *reactorsToRemove* so that it can be removed later
  (setq	*reactorsToRemove*
	 (append *reactorsToRemove* (list reactor))
  ) ;_ end of setq

  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ
    (list 'gp:outline-erased outlinePoly reactor parameterList)
  ) ;_ end of princ
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (alert
    (strcat
      (format-reactor-message 'gp:outline-erased)
      "\nThis reactor-callback function's responsibility will be to:\n"
      "\n\tBuild upon a list that records pointers to any reactors for"
      "\n\tany polyline or polylines being erased by the user.  This is "
      "\n\tdone so the reactors can be removed once the erase command "
      "\n\thas ended."
    ) ;_ end of strcat
  ) ;_ end of alert
  (princ)
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:outline-changed                                   ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is fired if the path outline   ;
;;;               is changed.  If this happens we need to:             ;
;;;                    1) Erase the tiles                              ;
;;;                    2) Remove the tile information from the reactor ;
;;;                       data (information stored to the reactor)     ;
;;;                    3) Save a pointer to the polyline for further   ;
;;;                       processing when the command-ended reactor    ;
;;;                       fires.                                       ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:outline-changed
       (outlinePoly reactor parameterList / tile tiles reactorData)
  ;; Set up the global variable that stores the pointer to the
  ;; polyline (as described in the description above)
  (setq *polyToChange* outlinePoly)

  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ
    (list 'gp:outline-changed outlinePoly reactor parameterList)
  ) ;_ end of princ
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))

  (alert
    (strcat
      (format-reactor-message 'gp:outline-changed)
      "\n\tThis reactor-callback function's responsibility will be to:\n"
      "\n\tAct upon the notification that the outline has been modified."
      "\n\tAssociated Actions:"
      "\n\t\t1. Erase the tiles"
      "\n\t\t2. Remove any associativity to field 100"
      "\n\t\t   (the field that holds a list of tile objects)"
      "\n\t\t3. Save the Reactor and Polyline for further processing"
      "\n\t\t   once the command now in progress has ended."
    ) ;_ end of strcat
  ) ;_ end of alert

  (princ)
) ;_ end of defun




;;;--------------------------------------------------------------------;
;;;     Function: gp:command-ended                                     ;
;;;--------------------------------------------------------------------;
;;;  Description: This reactor function is called at the end of any    ;
;;;               command.                                             ;
;;;--------------------------------------------------------------------;
;;;  This is where the majority of work is done.  Once the command     ;
;;;  that the user is performing has ended, we can get to work.  (We   ;
;;;  cannot modify entities while they are being modified by AutoCAD   ;
;;;  itself, so we have to wait until we get a notification that the   ;
;;;  command in progress is complete, and we can have access to the    ;
;;;  entities.)                                                        ;
;;;--------------------------------------------------------------------;
;;;  THIS FUNCTION IS CURRENTLY IN A STUBBED-OUT STATE!!!              ;
;;;--------------------------------------------------------------------;
(defun gp:command-ended	(reactor command-list)
  ;; Print to the console to see the results of the incoming data
  (terpri)
  (princ (list 'gp:command-ended reactor command-list))
  (terpri)
  (if *polyToChange*
    (progn
      (princ "\nPolyline being modified is ")
      (princ *polyToChange*)
    ) ;_ end of progn
  ) ;_ end of if
  (if *reactorsToRemove*
    (progn
      (princ "\nReactors that need to be removed: ")
      (princ *reactorsToRemove*)
    ) ;_ end of progn
  ) ;_ end of if
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (alert
    (strcat
      (format-reactor-message 'gp:command-ended)
      "\nThis reactor-callback function's responsibility will be to:\n"
      "\n\tNote what AutoCAD command has ended and respond accordingly."
      "\n\tAssociated Actions:"
      "\n\t\t1. If the polyline has been erased, remove associated reactors"
      "\n\t\t2. If the associatvity has been lost, then erase application"
      "\n\t\t   data from the reactor."
      "\n\t\t3. If the outline has not lost associativity and has been "
      "\n\t\t   stretched using Grips, then straighten it up."
    ) ;_ end of strcat
  ) ;_ end of alert
  (princ)
) ;_ end of defun

;;;--------------------------------------------------------------------;
;;;     Function: format-reactor-message                               ;
;;;--------------------------------------------------------------------;
;;;  Description: This is a temporary function used to format the      ;
;;;               messages that appear in the stubbed-out reactor      ;
;;;               callback function alerts.                            ;
;;;               It uses the vl-symbol-name function to convert the      ;
;;;               reactorFunction symbol into a string, and returns    ;
;;;               this as a formatted message presentable for the      ;
;;;               alert dialog box.                                    ;
;;;--------------------------------------------------------------------;
(defun format-reactor-message (reactorFunction)
  (strcat "****************************| Callback function: "
	  (vl-symbol-name reactorFunction)
	  " |***************************\n"
  ) ;_ end of strcat
) ;_ end of defun


;;;--------------------------------------------------------------------;
;;;     Function: gp:clean-all-reactors                                ;
;;;--------------------------------------------------------------------;
;;;  Description: Used to clean all reactors before exiting AutoCAD.   ;
;;;               This is a Very Important Function!                   ;
;;;--------------------------------------------------------------------;
(defun gp:clean-all-reactors (reactor command-list)
  (terpri)
  (princ (list 'gp:clean-all-reactors reactor command-list))
  (terpri)
  (princ (setq reactorData (vlr-data reactor)))
  (terpri)
  (princ (list command-list " has been issued"))
  (cleanReactors)
) ;_ end of defun


