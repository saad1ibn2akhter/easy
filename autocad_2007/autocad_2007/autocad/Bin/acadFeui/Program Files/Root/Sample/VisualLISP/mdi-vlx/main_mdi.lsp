;;;                                                                    ;
;;;  MAIN_MDI.LSP                                                      ;
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
;;;  This file shows how to use the new VARIANT and SAFEARRAY          ;
;;;  variables.                                                        ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;  First of all, initialize the ActiveX interface for this session   ;
;;;  if need be.  						       ;
;;;--------------------------------------------------------------------;
(vl-load-com)

; make sure AutoCAD is in MDI mode
(setvar "SDI" 0)

; spread utility definitions to all current and future documents for session
(vl-load-all "doc_utils.lsp")

; variable will be used by sample's VLX application and be evident in each
; of this session's documents
(setq mdi-vlx-cntr 0)
(vl-propagate 'mdi-vlx-cntr)

(setq drawings nil)
(while (setq choice (getfiled "Choose another DWG to open" "" "dwg" 0))
  (setq drawings (reverse (cons choice drawings)))
  )

(setq docs (vla-get-Documents (vlax-get-acad-object)))

;perform sequential open
(foreach file drawings (vla-open docs file))



