;;;
;;;    PLT2DWG.LSP - Written by Randy Kintzley
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
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Reads a plt file (HPGL format only) and creates polylines in the current drawing
;from the plot file information.
; Each pen number is directly related to the color of the polyline that will be created.
;i.e. pen 1 will create red polylines.
;
(defun c:plt2dwg ( / a n fna fh flag p1 pd ptcnt)
(acet-error-init
 (list (list   "cmdecho" 0
             "regenmode" 1
              "limcheck" 0
                "clayer" nil
       )
       0  ;do not undo everything if cancel is pressed
 );list
);acet-error-init
 
(setq fna (acet-ui-getfile "Enter the plot file" (acet-filename-ext-remove (getvar "dwgname")) "PLT" "Acet:plt2dwg" 1664));setq
(cond
 ((not fna) (princ))
 ((not (setq flag (acet-plt2dwg-file-is-hpgl fna)))
  (acet-alert "\nThe selected plt file is not the proper HPGL format.")
 )
 (flag                   ;(= flag 1)
  (acet-plt2dwg fna)
  (command "_.zoom" "_e")
 )
);cond close
 
(acet-error-restore)
);defun c:plt2dwg
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-plt2dwg ( fna / fh n a b pd p1 ptcnt lst n lst2 na ss )
 
    (setq fh (acet-file-open fna "r")
          na (entlast) 
    );setq
    (setq n 0);setq
    (while (setq a (acet-plt2dwg-plt-read fh));setq
      (setq    a (acet-str-replace "PU" ";PU;" a)
               a (acet-str-replace "PD" ";PD;" a)
            lst2 (acet-str-to-list ";" a)
      );setq
 
     (foreach a lst2
      (if (equal n (* 75 (/ n 75)))
          (acet-spinner)
      );if
      (setq b (substr a 3)
            a (substr a 1 2)
      );setq
 
      (if (= b "")
          (setq b nil)
      );if
      (cond
       ((= a "")(princ));cond #1
       ((acet-str-equal a "PD")
        (if (not (wcmatch (getvar "cmdnames") "*PLINE*"))
            (progn
             (acet-cmd-exit)
             (command "_.pline")
            );progn then
        );if
        (setq    pd T
              ptcnt 0
        );setq
       );cond #2
       ((and b
             (acet-str-equal a "SP") ;;;set pen
        );and
        (if (and pd
                 (equal ptcnt 0)
                 p1
            )
            (command p1 p1 "")
         );if
         (acet-cmd-exit)
         (if (not (tblobjname "layer" b))
             (command "_.-layer" "_make" b "_color" b b "") 
             (command "_.-layer" "_thaw" b "_on" b "_set" b "")
         );if
         (princ (acet-str-format "\rImporting pen number: %1" b))
         (setq pd nil)
       );cond #3
       ((and b
             (acet-str-equal a "CI")
        );and
         (if pd
             (progn
              (if (equal ptcnt 0)
                  (command p1 b)
                  (command b)
              );if
             );progn
         );if
         (command "_.circle" p1 b)
       );cond #4
       ((and b
             (acet-str-equal a "PA")
        );and
         (if pd
             (progn
              (if (equal ptcnt 0)
                  (command p1 b)
                  (command b)
              );if
             );progn
         );if
         (setq p1 b)
       );cond #5
       ((and b
             (acet-str-equal a "PR")
        )
        (if (and pd 
                 (= ptcnt 0)
            )
            (command p1) ;;drop the first point
        );if 
        (setq lst (acet-str-to-list "," b))
        (setq n 0)
        (repeat (/ (length lst) 2)
         (setq  a (list (atoi (nth n lst))
                        (atoi (nth (+ n 1) lst))
                  )
               p1 (mapcar 'atoi (acet-str-to-list "," p1))
               p1 (strcat (itoa (+ (car p1) (car a)))
                          ","
                          (itoa (+ (cadr p1) (cadr a)))
                  )
         );setq
         (if pd 
             (progn
              (command p1)
              (setq ptcnt (+ ptcnt 1))
             );progn then
         );if 
         (setq n (+ n 2))
        );repeat
       );cond #6
       ((acet-str-equal a "PU")
        (if (and pd
                 (equal ptcnt 0)
                 p1
            )
            (command p1 p1 "")
        );if
        (acet-cmd-exit)
        (setq pd nil)
       );cond #7
      );cond close
 
     );foreach
 
    );while reading the file
    (close fh)
    (acet-cmd-exit)
 
    (if (setq ss (acet-ss-new na))
        (command "_.scale" ss "" "0,0" (/ 1.0 1020.0))
    );if
 
    (princ "\r                                                                   ")
    (princ)
);defun acet-plt2dwg
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;takes a plt file name and returns 
;- 1 if the file is in ABSOLUTE HPGL format.
;- 2 if the file is in RELATIVE HPGL format.
;
(defun acet-plt2dwg-file-is-hpgl (fna / a b n fh flag aplt rplt )
 (setq aplt "\e.(;\e.I81;;17:\e.N;19:IN;SC;PU"
       rplt "\e.(;\e.I81;;17:\e.N;19:IN;SC;PU"
 );setq
 (if (and (setq fna (findfile fna))
          (setq fh (acet-file-open fna "r"))
     );and
     (progn
      (setq b "")
      (setq n 1)
      (while (and (<= n 29)
                  (setq a (read-char fh))
                  (or (acet-str-equal (chr a) (substr aplt n 1))
                      (acet-str-equal (chr a) (substr rplt n 1))
                  );or
             );and
       (setq b (strcat b (chr a)));setq
      (setq n (+ n 1))
      );while
      (cond
       ((acet-str-equal aplt b)
        (setq flag 1) ;absolute 
       )
       ((acet-str-equal rplt b)
        (setq flag 2) ;relative
       )
      ); 
      (close fh)
     );progn then
 );if
 flag
);defun acet-plt2dwg-file-is-hpgl
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun acet-plt2dwg-plt-read ( fh / a b c )
 
 (while (and (setq a (read-char fh));setq ;;read past any leading semi-colons.
             (setq a (chr a))
             (equal a (chr 59))
        );and
 );while
 (if a
     (setq b a)
     (setq b "")
 );if
 (while (and (setq a (read-char fh));setq
             (setq a (chr a))
             (not (equal a (chr 59)))
        );and
  
  (setq b (strcat b a));setq
 );while
 (if (equal b "")
     (setq b nil);setq
 );if
 b
);defun acet-plt2dwg-plt-read


(princ)