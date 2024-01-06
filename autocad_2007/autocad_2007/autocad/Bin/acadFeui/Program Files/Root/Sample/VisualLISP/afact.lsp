;;;                                                                    ;
;;;  AFACT.LSP                                                         ;
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
;;;       Function:  FACT2                                             ;
;;;                                                                    ;
;;;    Description:  This function is recursive to fact1. It uses fact1;
;;;                  as a helper recursive function to aid solving the ;
;;;                  factorial of the number supplied. Returns the     ;
;;;                  factorial of number.                              ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          fact1                                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             nber =  A number.                                      ;
;;;                                                                    ;
;;; Returned Value:                                                    ;
;;;                  Returns the factorial of number.                  ;
;;;                                                                    ;
;;;          Usage: (fact2 10)                                         ;
;;;                   returns: 3628800                                 ;
;;;--------------------------------------------------------------------;
(defun fact2 (nber)
  (if
    (= nber 0)
     1
     (* nber (fact1 (1- nber)))
  )
)

;;;--------------------------------------------------------------------;
;;;       Function:  FACT1                                             ;
;;;                                                                    ;
;;;    Description:  This function is recursive to fact2. It uses fact2;
;;;                  as a helper recursive function to aid solving the ;
;;;                  factorial of the number supplied. Returns the     ;
;;;                  factorial of number.                              ;
;;;                                                                    ;
;;;                 Required Functions:                                ;
;;;                          fact2                                     ;
;;;                                                                    ;
;;;      Arguments:                                                    ;
;;;             numbr =  A number.                                     ;
;;;                                                                    ;
;;; Returned Value:                                                    ;
;;;                  Returns the factorial of number.                  ;
;;;                                                                    ;
;;;          Usage: (fact1 10 )                                        ;
;;;                   returns: 3628800                                 ;
;;;--------------------------------------------------------------------;
(defun fact1 (numbr)
  (if
    (= numbr 0)
     1
     (* numbr (fact2 (1- numbr)))
  )
)

