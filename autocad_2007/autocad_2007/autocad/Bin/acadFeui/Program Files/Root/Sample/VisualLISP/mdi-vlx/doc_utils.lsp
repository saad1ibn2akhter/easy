;;;
;;;  DOC_UTILS.LSP
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
(princ
  "\n\t\t\tTo find out how to continue the mdi-vlx sample, type \"sample-instrs\"."
)

(defun c:sample-instrs ()
  (alert
    (strcat
      "************************** Next Step ****************************"
      "\nYou should first define selection sets in this session's opened documents"
      "\nUse command modify-trans-set to alter any document's \"transferible\" selection set."
      "\n\nNext, go to an open or new document and issue a command of load-vlxfile."
      "\nWithin the context of this loading document, you will be able to import the"
      "\nentities of other documents' transferible selection sets as blocks."

    )
  )
)


(defun *error* (msg)
  (alert (strcat "oops: " msg))
  (quit)
)


(defun c:load-vlxfile (/ vlx_app vlx_name ext)
					; vlx_file.lsp should be compiled into an optimized VLX-T application with
					; a separate namespace.
  (setq	vlx_app	 "vlx_app"
	vlx_name vlx_app
	ext	 ".vlx"
  )
  (if (and (not (findfile (strcat vlx_app ext))) ;assume "vlx_app.vlx"
	   (not	(setq vlx_app (getfiled	"Choose compiled VLX application"
					""
					"vlx"
					0
			      )
		)
	   )
      )
    (*error*
      "\nNo VLX application to be found.  \nYou may need to
    compile \"vlx_file.lsp\" using the IDE's \"New Application Wizard\""
    )
  )

  (setq vlx_name (vl-filename-base vlx_app))

  (if (not (vl-vlx-loaded-p vlx_name))
    (progn
      (load vlx_app)			; if load fails, error msg results by default
      (princ
	"\n The following functions are now available to you from this document:\n"
      )
      (car (vl-list-exported-functions vlx_name))
    )
    (progn
      (terpri)
      (princ (strcat vlx_name " already loaded\n"))
      (princ)
    )
  )
)

					;(defun init_doc ()

(defun c:modify-trans-set (/ thisDoc selsets)
					; sampset will be a global document variable
  (setq thisDoc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if sampset
    (vla-clear sampset)			; start from scratch
    (progn
      (setq selsets (vla-get-SelectionSets thisDoc))
	; each document sustains a selection set with variable sampset
      (setq sampset (vla-add selsets "mdi-vlx"))
    )
  )
  (vla-SelectOnScreen sampset)
)






