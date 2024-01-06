;;;                                                                    ;
;;;  TYPELIB.LSP                                                       ;
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
;;;  This file shows how to import type library files and how to use   ;
;;;  them.                                                             ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  First of all we have to init the ActiveX interface.               ;
;;;--------------------------------------------------------------------;
(vl-load-com)


;;;--------------------------------------------------------------------;
;;;  Import the Microsoft WinWord type library. You have to replace    ;
;;;  the path with the path to your WinWord type library file.         ;
;;;  Import the type library only if it is not already loaded.         ;
;;;--------------------------------------------------------------------;
(if (equal nil mswc-wd100Words) ; check for a WinWord constant
  (vlax-import-type-library
    :tlb-filename "c:/program files/Microsoft Office/msword8.olb"
    :methods-prefix "mswm-"
    :properties-prefix "mswp-"
    :constants-prefix "mswc-"
  ) ;_ end of vlax-import-type-library
) ;_ end of if

;;;--------------------------------------------------------------------;
;;;  After importing the type library file you can use the Apropos     ;
;;;  Window to see the added VisualLisp functions.                     ;
;;;  Go to the "View" menu, select "Apropos Windows..." and enter      ;
;;;  "mswm-" in the edit box to get a list of all WinWord methods.     ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the Model Space portion of the active drawing.  This  ;
;;;  variable, named *ModelSpace* will be created at load time.        ;
;;;--------------------------------------------------------------------;
(setq *ModelSpace*
  (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
) ;_ end of setq

;;;--------------------------------------------------------------------;
;;;  For ActiveX functions, we need to define a global variable which  ;
;;;  "points" to the AutoCAD application object.  This variable, named ;
;;;  *AcadApp* will be created at load time.                           ;
;;;--------------------------------------------------------------------;
(setq *AcadApp*
  (vlax-get-acad-object)
) ;_ end of setq


;;;--------------------------------------------------------------------;
;;;  This is the main function. It will iterate the model space and    ;
;;;  collect all Text entities. Then it gets the text strings and adds ;
;;;  them to a newly created Microsoft WinWord document.               ;
;;;--------------------------------------------------------------------;
(defun c:ExtractText ( / msw docs doc ent pgs pg range
		         text varTextpos arrayTextpos textinfo)
  ; Get the Microsoft WinWord application object
  (setq msw (vlax-get-object "Word.Application.8"))
  (if (equal nil msw)
    (progn
      ; WinWord is not running. Start it.
      (setq msw (vlax-create-object "Word.Application.8"))
      (vla-put-visible msw 1)
    )
  )
  (if (/= nil msw)
    (progn
      ;; Get the WinWord's document collection object.
      ;; The Application object of WinWord is accessed by the
      ;; vla-get-xxx functions. For some ActiveX properties and methods
      ;; there is no generated VisualLisp function. In this case you have
      ;; to use the vlax-get-property / vlax-put-property and
      ;; vlax-invoke-method functions.
      ;; Example: For the CommandBars property of the WinWord application
      ;; object there is no VisualLisp function, but you can get this
      ;; object the following way:
      ;; (setq ComBars (vlax-get-property msw "CommandBars"))
      (setq docs (vla-get-documents msw))
      ; Add a new document
      (setq doc (mswm-add docs))
      ; Get the paragraphs of the document (to do some formatting)
      (setq pgs (mswp-get-paragraphs doc))
      ; Now iterate the AutoCAD model space and export
      ; every text entity to WinWord.
      (vlax-for ent *ModelSpace*
	(if (equal (vla-get-ObjectName ent) "AcDbText")
	  (progn
	    ; Get some information from the text entity
	    (setq text (vla-get-TextString ent)
		  textpos (vla-get-InsertionPoint ent)
		  arrayTextpos (vlax-variant-value textpos)
		  textinfo (strcat
			     (rtos (vlax-safearray-get-element arrayTextpos 0) 2 2)
			     ", "
			     (rtos (vlax-safearray-get-element arrayTextpos 1) 2 2)
			     ", "
			     (rtos (vlax-safearray-get-element arrayTextpos 2) 2 2)
			   )
            ) ;_ end of setq
	    ; Print some info (with formatting)
	    ; 1) Get the last paragraph
	    (setq pg (mswp-get-last pgs))
	    ; 2) Get a range object
	    (setq range (mswp-get-range pg))
	    ; 3) Do some formatting
	    (mswp-put-bold range 1)
	    (mswp-put-underline range mswc-wdUnderlineSingle)
	    ; 4) Show the info text
	    (mswm-InsertAfter range (strcat "AcDbText at position " textinfo "\n"))
	    ; Now show the text string (from the ACAD text entity)
	    (setq pg (mswp-get-last pgs))
	    (setq range (mswp-get-range pg))
	    (mswp-put-bold range 0)
	    (mswp-put-underline range mswc-wdUnderlineNone)
	    (mswm-InsertAfter range (strcat text "\n\n"))
	  ) ;_ end of progn
	) ;_ end of if AcDbText
      ) ;_ end of vlax-for
    ) ;_ end of progn
    (princ "\nNo Microsoft WinWord found.\n")
  ) ;_ end of if (/= nil msw)
  (princ)
)


;;; Display a message to let the user know the command name
(princ "\nType ExtractText to export all text entities to Microsoft WinWord.\n")
(princ)