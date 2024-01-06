;;;
;;;  VLX_FILE.LSP
;;;
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
;;;
;;;
;;; This VLX application demonstrates that variables can be exchanged
;;; between application and document namespaces and between application
;;; namespace and session blackboard.
(vl-load-com)

(vl-doc-export 'import_objs)
(defun C:import_objs ()
  (setq	thisDoc	      (vla-get-ActiveDocument (setq app (vlax-get-acad-object)))
	docs	      (vla-get-documents app)
	thisDocName   (vla-get-name thisDoc)
	thisDocBlocks (vla-get-blocks thisDoc)
	thisMS	      (vla-get-modelspace thisDoc)
  )


  (setq exclude_docs (list thisDocName))

  ; Retrieve and increment counter from active document's namespace.
  ; Counter reflects number of times this function has been invoked.
  (setq import_objs_cntr (vl-doc-set 'mdi-vlx-cntr
					 (1+ (vl-doc-ref 'mdi-vlx-cntr))))
   

  (while (equal "Y" (progn
		      (initget 0 "Y N")
		      (getkword "\nImport a transfer set into active document? [Y|N] ")
		      )
		)
    ; prompt user to identify an open document
    (setq message (strcat "\nCurrently in drawing "
			  thisDocName
			  ".\nXREF in drawing from open document:"
		  )
    )

    (setq
      chosenDoc	   (vla-item docs
			     (choose_item docs message exclude_docs)
		   )
      chosenName   (vla-get-name chosenDoc)

      chosenFile   (strcat "trans" (itoa import_objs_cntr) chosenName)

      chosenSelSet (vla-item (vla-get-SelectionSets chosenDoc)
			     "mdi-vlx"
		   )
    )
    (terpri)
    (princ chosenFile)

    (vla-wblock chosenDoc chosenFile chosenSelSet)

    				;clear chosenSelSet for next loop pass

    (princ
      (strcat "\n" (vla-get-fullname chosenDoc) " is selected\n")
    )

    (setq exclude_docs (cons chosenName exclude_docs))

    (setq InsertPt
	   (vlax-3d-point (getpoint "\nChoose insertion point.\n"))
    )
    (setq importBlock
	   (vla-InsertBlock
	     thisMS
	     InsertPt
	     chosenfile
	     1
	     1
	     1
	     0
	   )
    )

    (vl-file-delete chosenFile)
    (vla-regen thisDoc acActiveViewport)
    (vla-ZoomAll (vlax-get-acad-object))
  )
)

(defun choose_item
       (collection msg excludeGrp / obj index keywds curName)
       ;|
  Allows user to identify an item in a collection, excluding members of "excludeGrp".
  excludeGrp is a list of names which might coincide with the names of items
  in the ActiveX "collection".  The user is prompted to select one number from a menu of acceptable
  collection items.  The index of the item is then returned.
  |;
  (if (or (not (listp excludeGrp))
					; collection not a vla-object
      )
    (quit)
  )

  (terpri)
  (princ msg)
  (terpri)

  (setq	index 0
	keywds ""
  )
  (terpri)
  (vlax-for obj	collection
    (if	(not (member (setq curName (vla-get-name obj))
		     excludeGrp
	     )
	)
      (progn
	(princ (strcat "[" (itoa index) "] " curName))
	(terpri)
	(setq keywds (strcat keywds (itoa index) " "))
      )
    )
    (setq index (1+ index))
  )

  (initget 1 keywds)
  (atoi (getkword "\nEnter number for an open :"))
)





