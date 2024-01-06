;;;                                                                    ;
;;;  OBJ-PROPS.LSP                                                     ;
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
;;;  This file demonstrates how VLISP interacts with the properties    ;
;;;  manager - changes propagated via selection sets and reactors      ;
;;;--------------------------------------------------------------------;

(command "_.properties")
(setq message
       (strcat
	 "Observe the property manager's state right now.\n"
	 "Type \"change-OPM\" at the command-line to see a
	 change to Model Space propagate to the OPM\n"
       )
)
(princ message)


(defun C:change-OPM ()

  (vl-load-com)

  (setq	*ModelSpace*
	 (vla-get-ModelSpace
	   (setq activeDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
	 ) ;_ end of vla-get-ModelSpace
  ) ;_ end of setq

  (setq	myObj
	 (vla-AddMtext
	   *ModelSpace*
	   (vlax-3d-point '(3 6 0))
	   7
	   "While this MTEXT is selected,
	   \\P\\Plook at Properties Manager
	   \\PDo you see this entity's properties displayed?
	   \\P\\PWatch Properties change again:
	   \\PHit ESC to de-select this entity"
	 )
  )
  (vla-ZoomAll (vlax-get-acad-object))
  (vla-update myObj)
					; make a pickfirst selection set out of new MTEXT object
  (sssetfirst nil (ssget "L"))

  
  (princ "\n Notice the new entity is selected\n
  and the manager has changed to display the properties of the new mtext\n
  Next...")
  
  (initget 1 "Y N")
  (if (equal "Y"
	     (getkword
	       "Attach an object reactor to this new mtext? [Y/N] "
	     )
      )
      (attach-reactor myObj)
  )
)

(defun attach-reactor (obj)
  (vl-load-reactors)

  (setq	entityReactor
	 (vlr-object-reactor
	   (list obj)
	   nil
	   '((:vlr-modified . chain-reaction))
	 )
  )

  (princ
    (strcat "\n New reactor object:  "
	    "entityReactor"
	    "\n It's '(<event> . <callback function>) is rigged as:\n\t"
    )
  )
  (princ (vlr-reactions entityReactor))
  (princ "\nNow changes to this mtext entity will trigger an event notification.")
  (princ)
)

(defun chain-reaction (notifier-object reactor-object param-list)
  (princ "\nYou've modified the MTEXT's properties\n")
)