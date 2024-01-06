;;;                                                                    ;
;;;  SEL-REAC.LSP                                                      ;
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
;;; This file contains various Example how to select all reactors      ;
;;; binded to the vla object                                           ;
;;;--------------------------------------------------------------------;


;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-OBJECT-REACTORS                            ;
;;;                                                                    ;
;;;    Description:  This function passes a vla object to              ;
;;;                  select-object-reactors-id. And serves as a        ;
;;;                  constructor.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    select-object-reactors-id                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;      vla_object = a valid vla object.                              ;
;;;                                                                    ;
;;; Returned Value:  A list of vla objects.                            ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(select-object-reactors                                ;
;;;                       vla_object                                   ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun select-object-reactors (vla_object)
  (select-object-reactors-id
    (vla-get-ObjectID vla_object)
    (VLR-reactors :VLR-Object-Reactor)
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  SELECT-OBJECT-REACTORS                            ;
;;;                                                                    ;
;;;    Description:  This function passes a vla object to              ;
;;;                  select-object-reactors-id. And serves as a        ;
;;;                  constructor.                                      ;
;;;                                                                    ;
;;;                  Required Functions:                               ;
;;;                    select-object-reactors-id                       ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;         objId = a valid vla object id.                             ;
;;;     react-lst = a list of vlr reactors.                            ;
;;;                                                                    ;
;;; Returned Value:  A list of vla objects.                            ;
;;;                                                                    ;
;;;          Usage:                                                    ;
;;;		(select-object-reactors-id                             ;
;;;                       objId                                        ;
;;;                       react-lst                                    ;
;;;              )                                                     ;
;;;--------------------------------------------------------------------;
(defun select-object-reactors-id (objId react-lst)
  ;; it is important to use object Id for checking object eqe.
  ;; because the same object may have many different vla-object
  ;; pointed to it but all of them have the same objectId.
  (cond
    ((null react-lst) nil)
    ((member objId
	     (mapcar
	       (function vla-get-ObjectID)
	       (VLR-Owners (car react-lst))
	     )
     )
     (cons (car react-lst)
	   (select-object-reactors-id objId (cdr react-lst))
     )
    )
    (t (select-object-reactors-id objId (cdr react-lst)))
  )
)