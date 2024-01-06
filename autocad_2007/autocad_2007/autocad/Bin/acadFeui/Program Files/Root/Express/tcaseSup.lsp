;;
;;  TcaseSup.lsp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Valid modes are "Sentence" "Lower" "Upper" "Title" "Toggle"
;
(defun acet-tcase-change-string ( a mode / n lst b c d str j )
 (setq mode (xstrcase mode))
 
 (cond
  ((= mode "UPPER") (setq a (xstrcase a)));cond #1
 
  ((= mode "LOWER") (setq a (strcase a T)));cond #2
 
  ((= mode "SENTENCE")
   (setq   a (strcase a T) ;force to lower
         lst (acet-str-to-list "." a)	;split it apart using "." as delimiter
           d ""
   );setq
   ;; re-build the main string forcing the first non-blank character in each element to upper case.
   (setq j 0)
   (repeat (length lst)
    (setq str (nth j lst))
    (setq n 1)
    (if (< (+ j 1) (length lst))
        (setq str (strcat str "."))
    );if
    (while (and (<= n (strlen str))
                (or (= " " (substr str n 1))
                    (= "\t" (substr str n 1))
                );or
           );and
     (setq n (+ n 1))
    );while
    (if (> n 1)
        (setq b (substr str 1 (- n 1)))
        (setq b "")
    );if
    (setq c (substr str (+ n 1))
          d (strcat d
                    b
                    (xstrcase (substr str n 1))
                    c
            );strcat
    );setq
    (setq j (+ j 1))
   );repeat
   (setq a d)
  );cond #3
 
  ((= mode "TITLE")
   (setq   a (strcase a T) ;force to lower
         lst (acet-str-to-list " " a)	;split it apart using " " as delimiter
           d ""
   );setq
   ;; re-build the main string forcing the first character in each element to upper case.
   (setq j 0)
   (repeat (length lst)
    (setq str (nth j lst))
    (if (< (+ j 1) (length lst))
        (setq str (strcat str " "))
    );if
    (setq d (strcat d
                    (xstrcase (substr str 1 1))
                    (substr str 2)
            );strcat
    );setq
    (setq j (+ j 1))
   );repeat
   (setq a d)
  );cond #4
 
  ((= mode "TOGGLE")
   (setq d "")
   (setq n 1)
   (while (<= n (strlen a))
    (setq str (substr a n 1))
    (if (acet-str-is-upper str)
        (setq str (strcase str T))
        (setq str (xstrcase str))
    );if
    (setq d (strcat d str))
    (setq n (+ n 1));setq
   );while
   (setq a d)
  );cond #4
 
 );cond close
 
 a
);defun acet-tcase-change-string
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a string and returns T if the first character is in the alphabet and is upper case.
;
(defun acet-str-is-upper ( a / n flag )
 (if (> (strlen a) 0)
     (progn
      (setq a (substr a 1 1)
            n (ascii a)
      );setq
      (if (and (> n 64)
               (< n 91)
          );and
          (setq flag T)
          (setq flag nil)
      );if
     );progn then
     (setq flag nil)
 );if
 flag
);defun acet-str-is-upper
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a raw string string and list of format pairs of the form:
;  ((controlChar startposition)
;   (controlChar startposition)
;   (controlChar startposition)
;    ...
;  )
;
;returns a string with the formating applied in the proper locations.
;
;
(defun acet-mtext-format-apply ( str flst / n a b frmt j )
 
 (setq n 0)
 (repeat (length flst)
  (setq    a (nth n flst)
           j (cadr a)			;; the start position
        frmt (car a)			;; the formating string
           a (substr str 1 (- j 1))
           b (substr str j)
  );setq
  (if (and (or (= frmt "\\P")
               (= frmt "\\~")
           );or
           (= " " (substr b 1 1))
      );and
      (setq b (substr b 2))
  );if
  (setq str (strcat a frmt b))
  (setq n (+ n 1))
 );repeat
 str
);defun acet-mtext-format-apply
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a string from mtext and returns 
;a list of the form:
;( "RawTextString"
;  ((controlChar startposition)
;   (controlChar startposition)
;   (controlChar startposition)
;    ...
;  )
;)
;
(defun acet-mtext-format-extract ( str / lst raw len pos frmt flst a n j lst2 )
 
 (setq lst (list "{"	"}"	"\\P"	"\\~"
                 "\\{"	"\\}"	"\\O"	"\\L"
                 "\\S"	"\\A1"	"\\A2"	"\\A3"
                 "\\f"	"\\C"	"\\H"	"\\T"
                 "\\Q"	"\\W"
           );list
       raw ""
       len (strlen str)
       pos 0
 );setq
 
 (while (> (strlen str) 0)
  
  (setq lst2 (mapcar '(lambda (x) (acet-str-find x str)) lst)
        lst2 (mapcar '(lambda (x) (if x (list x) x)) lst2)
        lst2 (apply 'append lst2)
           j (apply 'min lst2)
  );setq 
  (if (/= j 0)
      (progn
        (setq  raw (strcat raw 
                           (substr str 1 (- j 1))
                   )
               str (substr str j)
                 a (acet-mtext-format-bite str) ;; (list format str offset)
              frmt (car a)
               str (cadr a)
                 n (+ pos j)
               pos (+ pos 
                      j 
                      (caddr a)
                      (- (strlen frmt) 1)
                   )
              frmt (list frmt n)
              flst (cons frmt flst)
        );setq
        (setq n (+ (length lst) 10));get out of inner loop
      );progn
      (setq raw (strcat raw str)
            str ""
      );setq then get out
  );if    
 
 );while
 
 (list raw (reverse flst))
);defun acet-mtext-format-extract
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Takes a string that begins with formating and returns the format string and
;the remainder of the string provided in a list
;  ("format" str)
;
(defun acet-mtext-format-bite ( str / a f1 n )
 
 (setq a (substr str 1 2)
       n 0
 )
 
 (cond 
  ((or (= "{" (substr str 1 1))
       (= "}" (substr str 1 1))
   );or
   (setq  f1 (substr str 1 1)
         str (substr str 2)
   );setq
  );cond #1
 
  ((or (= "\\P" a)
       (= "\\~" a)
   )
   (setq  f1 (substr str 1 2)
         str (strcat " " (substr str 3))
           n -1
   )
  );cond #2
 
  ((or (= "\\{" a)
       (= "\\}" a)
       (= "\\O" a)
       (= "\\L" a)
       (= "\\S" a)
       ;(= "\\\\" a)
   )
   (setq  f1 (substr str 1 2)
         str (substr str 3)
   )
  );cond #3
 
  ((or (= "\\A1" (substr str 1 3))
       (= "\\A2" (substr str 1 3))
       (= "\\A3" (substr str 1 3))
   );or
   (setq  f1 (substr str 1 3)
         str (substr str 4)
   );setq
  );cond #4
 
  ((or (= "\\f" a)
       (= "\\C" a)
       (= "\\H" a)
       (= "\\T" a)
       (= "\\Q" a)
       (= "\\W" a)
   )
   (setq   n (acet-str-find ";" str)
          f1 (substr str 1 n)
         str (substr str (+ n 1))
           n 0
   );setq
  );cond #6
 );cond close
 
 (list f1 str n)
);defun acet-mtext-format-bite


(princ)
