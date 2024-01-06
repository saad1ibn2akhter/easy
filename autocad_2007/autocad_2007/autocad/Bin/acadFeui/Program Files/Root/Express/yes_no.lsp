;;;
;;;    YES_NO.LSP
;;;    Copyright © 1999 by Autodesk, Inc.
;;;
;;;    Your use of this software is governed by the terms and conditions of the
;;;    License Agreement you accepted prior to installation of this software.
;;;    Please note that pursuant to the License Agreement for this software,
;;;    "[c]opying of this computer program or its documentation except as
;;;    permitted by this License is copyright infringement under the laws of
;;;    your country.  If you copy this computer program without permission of
;;;    Autodesk, you are violating the law."
;;;
;;;    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;;    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;;    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;;    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;;    UNINTERRUPTED OR ERROR FREE.
;;;
;;;    Use, duplication, or disclosure by the U.S. Government is subject to
;;;    restrictions set forth in FAR 52.227-19 (Commercial Computer
;;;    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;;    (Rights in Technical Data and Computer Software), as applicable.
;;;
;;;  ----------------------------------------------------------------
 
;BNS_GET_YES_NO
;Takes message and size arguments and asks the user to pick Yes or No.
;Returns 1 for yes and 0 for no.
;The first argument is a list of strings. The first item is the title
;string and the second string is the body of the message.
;(Include "\n" to force line feeds.)
;The second argument specifies the size of the dialog and is a list of
;two integers. i.e. (width height)
;
;Current valid sizes are as follows:
;60 15
;40 10
;20  5
;
(defun bns_get_yes_no ( lst size / lst2 n iv flag ttl msg)
 
 (if (and lst
          (> (setq iv (load_dialog "yes_no"));setq
             0
          );test
     );and
     (progn
      (setq  ttl (car lst)
             msg (cadr lst)
            size (strcat (itoa (car size)) "_" (itoa (cadr size)))
      );setq
      (if (new_dialog (strcat "bns_yes_no" size) iv)
          (progn
           (setq lst2 (acet-str-to-list "\n" msg))
           (setq n 0)
           (repeat (length lst2)
           (set_tile (strcat "msg" (itoa n)) (nth n lst2))
           (setq n (+ n 1));setq
           );repeat
           (set_tile "title" ttl)
           (set_tile "msg" msg)
           (action_tile "accept" "(done_dialog 1)")
           (action_tile "cancel" "(done_dialog 0)")
 
           (setq flag (start_dialog));setq ;START_DIALOG MAKES THE BUTTONS ACTIVE
 
          );progn then initialize the tiles and activate the dialog box
          (alert "Unable to display dialog box")
      );if new dialog
      (unload_dialog iv);unload it when done
     );progn then
     (alert "Unable to load dialog box");else
 );if load
flag
);defun bns_get_yes_no


(princ)
