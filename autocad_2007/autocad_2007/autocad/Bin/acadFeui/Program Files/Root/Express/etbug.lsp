;;
;;  etbug.lsp - report an express tool bug and email it to the xpress Tools team.
;;                    
;;
;;  Copyright © 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
;;  AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
;;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
;;  DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
;;  UNINTERRUPTED OR ERROR FREE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:etbug ( / fna ans msg path )
  (if (equal "failed" (load "acadinfo.lsp" "failed"))
      (progn
       (alert "Could not load \"acadinfo.lsp\". Check the AutoCAD support path in OPTIONS.")
       (exit)
      );progn then
  );if
 
  ;; Get the default location of the express directory just in 
  ;; case express is not on the support path.
  (if (setq path (findfile "acad.exe"))
      (setq path (substr path 1 (- (strlen path) 8))    ;; the acad root dir
            path (strcat path "express\\")		;; add the express dir
      );setq then
      (setq path "")
  );if
 
  (princ "\nGathering AutoCAD information...\n")
  (acet-etbug-acadinfo (+ 1 2 4 8 16 32 64)) ;; do not include lisp dump (128)
 
  (cond 
   ((not (setq fna (findfile "acadinfo.txt")))
    (alert "Acadinfo.txt was not successfully created.")
   );cond #1
 
   ((and (not (member "acetutil.arx" (arx)))
         (equal "failed" (arxload "acetutil.arx" "failed"))
         (equal "failed" (arxload (strcat path "acetutil.arx") "failed"))
    );and 
    (alert (strcat " \nCould not load \"acetutil.arx\". "
                   "\nCheck the AutoCAD support path in OPTIONS."
                   "\nNOTE: You can manually e-mail the acadinfo.txt file just created."
           )
    );alert
   );cond #2
 
   ((and (not (member "acetmail.arx" (arx)))
         (equal "failed" (arxload "acetmail.arx" "failed"))
         (equal "failed" (arxload (strcat path "acetmail.arx") "failed"))
    );and 
    (alert (strcat "Unable to send email."
                   " \nCould not load \"acetmail.arx\". "
                   "\nCheck the AutoCAD support path in OPTIONS."
                   "\nNOTE: You can manually e-mail the acadinfo.txt file just created."
           )
    );alert
   );cond #3
 
   ((setq msg (acet-etbug-get-userinfo))
    (acet-send-bug msg)
   );cond #4
 
  );cond close
 
(princ)
);defun c:etbug
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-etbug-get-userinfo ( / fna fh str a )
 (setq fna (findfile "acadinfo.txt"))
 (cond 
  ((not fna)
   (princ "\nCould not find acadinfo.txt")
  );cond #1
  ((not (setq fh (open "acadinfo.txt" "r")))
   (princ "\nCould open acadinfo.txt for read.")
  );cond #2
  (T
   (repeat 20 (read-line fh));skip the header information
   (setq str "")
   (while (setq a (read-line fh))
    (setq str (strcat str "\r\n" a))
   );while
   (close fh)
   (setq a (acet-ui-txted 
               (strcat "Problem Description: \r\n\r\n\r\n" 
                       "Steps to reproduce: \r\n" 
                       "- \r\n- \r\n- \r\n- \r\n"
               );strcat
               "ETBUG: Enter a description of the problem and steps to reproduce"
           )
   );setq
   (if a
       (progn
        (setq str (strcat a str))
        (if (setq fh (open fna "w"))
            (progn
             (write-line str fh)
             (close fh)
            );progn then
            (princ "\nError: Could not open acadinfo.txt for write.")
        );if
       );progn then
       (progn
        (setq str nil)
        (princ "\nOperation canceled.")
       );progn else
   );if
  );cond #3
 );cond close
 str
);defun acet-etbug-get-userinfo
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-send-bug ( msg / host err )
 (princ "\n ")
 (princ "\n ")
 (princ "\r")
 (princ "                                                         ")
 (princ "\n")
 (setq host (acet-get-email-host))
 (if (/= host "")
     (progn
      (setq err (acet-smtp-send
                 host
                 (getvar "loginname")
                 "expresstools@autodesk.com"
                 "Beta Bug"
                 msg
                )
      );setq
      (if err
          (alert err)
          (princ "\nMessage successfully sent.")
      );if
     );progn then
     (princ "\nNo email host provided.")
 );if
 
);defun acet-send-bug
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-get-email-host ( / def a )
 (setq def (getenv "acet_email_host"))
 (if (or (not def)
         (equal def "")
     );or
     (setq def "autodesk.com")
 );if
 (if (/= def "")
     (setq a (getstring (strcat "\nEnter the name of your outgoing mail server <" def ">: ")))
     (setq a (getstring "\nEnter your email host name: "))
 );if
 (if (and (= a "")
          (/= def "")
     );and
     (setq a def)
 );if
 (setenv "acet_email_host" a)
 a
);defun acet-get-email-host
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;Takes a flags arg that is the sum of the following:
; 0 do nothing
; 1    (acet-acadinfo-do-header fh)
; 2    (acet-acadinfo-do-general fh)
; 4    (acet-acadinfo-do-express fh)
; 8    (acet-acadinfo-do-fileloads fh)
; 16   (acet-acadinfo-test-load ...)
; 32   typelib
; 64   sysvars
; 128  Lisp dump
;
;
(defun acet-etbug-acadinfo ( flags / fna fh op)
 
;initialize a bare bones error handler that will recover from a first try
;load_test failure and re-issue this command.
 
  (if (not Acet:Acadinfo-Olderr)
      (setq Acet:Acadinfo-Olderr *error*)
  );if
 
  (defun *error* ( msg / )
    (if Acet:Acadinfo-Error-On-Load-Test
      (progn
        (if fh
            (close fh)
        );if
        (setq *error* Acet:Acadinfo-Olderr
              Acet:Acadinfo-Olderr nil
        );setq
        (acet-etbug-acadinfo flags)
        (setq Acet:Acadinfo-Error-On-Load-Test nil)
      );progn
      (princ msg)
    );if
  );defun *error*
 
 
  (setq fna "acadinfo.txt");setq
 
  (if Acet:Acadinfo-Error-On-Load-Test
    (setq op "a")
    (progn
       (setq op "w")
    );progn then
  );if
 
  (if (setq fh (open fna op))
    (progn
      (close fh) (setq fh (open fna op)) ;close and re-open again in case of
                                         ;garbage echo from error recovery.
 
      (if (not Acet:Acadinfo-Error-On-Load-Test)
        (progn
 
          (if (= 1 (logand 1 flags))
              (progn
               (setq flags (- flags 1))
               (acet-acadinfo-do-header fh)
              );progn
          );if
          (if (= 2 (logand 2 flags))
              (progn
               (setq flags (- flags 2))
               (acet-acadinfo-do-general fh)
              );progn
          );if
          (if (= 4 (logand 4 flags))
              (progn
               (setq flags (- flags 4))
               (acet-acadinfo-do-express fh)
              );progn
          );if
          (if (= 8 (logand 8 flags))
              (progn
               (setq flags (- flags 8))
               (acet-acadinfo-do-fileloads fh)
              );progn
          );if
 
          (if (= 16 (logand 16 flags))
              (progn
               (setq flags (- flags 16))
               (princ "\Performing load tests...")
               (write-line "Tests for successful load of LISP initialization files." fh)
               (write-line (acet-acadinfo-test-load "acad2000.lsp") fh)
               (write-line (acet-acadinfo-test-load "acad2000doc.lsp") fh)
               (write-line (acet-acadinfo-test-load "acettest.fas") fh)
               (write-line (acet-acadinfo-test-load "acetutil.fas") fh)
               (write-line (acet-acadinfo-test-load "acetmain.mnl") fh)
              );progn
          );if
 
        );progn then
        (progn
          (write-line "" fh)
          (write-line "*****FAILURE during lisp file load tests.**** " fh)
          (write-line "One of the following files causes an error on load: " fh)
          (write-line "  acad2000.lsp"  fh)
          (write-line "  acad2000doc.lsp"  fh)
          (write-line "  acettest.fas" fh)
          (write-line "  acetutil.fas" fh)
          (write-line "  acetmain.mnl" fh)
        );progn else
      );if
 
 
      (if (= 32 (logand 32 flags))
          (progn
           (setq flags (- flags 32))
           (write-line " ------------------------- TYPELIB TEST -------------------------" fh)
           (write-line "" fh)
           (acet-acadinfo-check-typelib fh)
           (write-line "" fh)
          );progn
      );if
 
      (if (= 64 (logand 64 flags))
          (progn
           (setq flags (- flags 64))
           (write-line "" fh)
           (write-line (strcat "(arx) -> " (acet-acadinfo-item-to-string (arx))) fh)
           (write-line "" fh)
           (write-line " ------------------- SYSTEM VARIABLE SETTINGS -------------------" fh)
           (write-line "|;" fh)
           (close fh)
           (acet-acadinfo-vars-to-scr fna -1);append
          );progn
          (close fh)
      );if
      (if (= 128 (logand 128 flags))
          (progn
           (setq flags (- flags 128))
           (acet-acadinfo-lisp-dump fna)
          );progn
      );if
 
      (princ "\nDone.")
    );progn then
    (princ "\nCannot open file for write.")
  );if
 
  (setq *error* olderr)
 
  (princ)
);defun acet-etbug-acadinfo


(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-header fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-general fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-express fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-do-fileloads fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-test-load fna)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-check-typelib fh)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-item-to-string a)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-vars-to-scr fna flag)))
(acet-autoload2	'("acadinfo.lsp"	(acet-acadinfo-lisp-dump fna)))
(princ)
