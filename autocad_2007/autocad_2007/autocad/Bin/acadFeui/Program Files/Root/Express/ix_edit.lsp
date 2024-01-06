;;
;;  ix_edit.lsp - IMAGEEDIT command
;;
;;  Copyright � 1999 by Autodesk, Inc.
;;
;;  Your use of this software is governed by the terms and conditions
;;  of the License Agreement you accepted prior to installation of this
;;  software.  Please note that pursuant to the License Agreement for this
;;  software, "[c]opying of this computer program or its documentation
;;  except as permitted by this License is copyright infringement under
;;  the laws of your country.  If you copy this computer program without
;;  permission of Autodesk, you are violating the law."
;;
;;  AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.  AUTODESK
;;  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF MERCHANTABILITY OR
;;  FITNESS FOR A PARTICULAR USE.  AUTODESK, INC. DOES NOT WARRANT THAT THE
;;  OPERATION OF THE PROGRAM WILL BE UNINTERRUPTED OR ERROR FREE.
;;
;;  Use, duplication, or disclosure by the U.S. Government is subject
;;  to restrictions set forth in FAR 52.227-19 (Commercial Computer
;;  Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
;;  (Rights in Technical Data and Computer Software), as applicable.
;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:imageedit ( / na na2 na3 na4 e1 e2 e3 e4 fna fna2 ina ina2)
 
(acet-error-init
 (list (list "cmdecho" 0)
       T
 );list
);acet-error-init
(princ "\nUse the IMAGEAPP command to specify a non-system-default editor.")
(if (setq na (acet-ui-single-select '((0 . "IMAGE")) nil))
    (progn
     (setq  e1 (entget na)                         ;the image
           na2 (cdr (assoc 340 e1))                ;the imagedef dict
            e2 (entget na2)
           fna (cdr (assoc 1 e2))                  ;the image filename
           na3 (cdr (assoc 330 e2))                ;the dictionary that holds the image name
            e3 (entget na3)                        ;
           na4 (cdr (assoc 330 (reverse e2)))      ;the imagedef_reactor
            e4 (entget na4)
            e3 (member (cons 350 na2) (reverse e3))
           ina (xstrcase (cdr (assoc 3 e3)))
     );setq
     (command "_.-image" "_unload" ina)
     (setq fna2 (ACET-FILE-WRITEDIALOG "IMAGE EDIT"
                             fna
                             (strcat "BMP;RLE;DIB;RST;GP4;MIL;CAL;CG4;"
                                     ";FLC;FLI;GIF;JPG;PCX;PCT;PNG;TGA;"
                                     "TIF"
                             );strcat
                             "Acet:ImageEdit"
                             1664
                );ACET-FILE-WRITEDIALOG
     );setq
     (if (and fna2
              (setq fna2 (acet-file-find-image fna2));setq
         );and
         (progn
          (start_image_editor ina fna2)
          (if (not (equal (acet-file-find-image fna)
                          (acet-file-find-image fna2)
                   )
              )
              (progn
               ;(entmod (subst (cons 1 fna2) (assoc 1 e2) e2))
               (command "_.image" "_path" ina fna2)
               (setq ina2 (xstrcase (acet-filename-path-remove (acet-filename-ext-remove fna2))));setq
                    (setq ina "")
                    (while (or (not (snvalid ina))
                           );or
                     (if (and (snvalid ina2)
                              (not (and (member (cons 3 ina2) (entget na3))
                                        (not (equal ina2 (xstrcase (cdr (assoc 3 e3)))))
                                        ;(princ "\n*Invalid* That name is already in use.")
                                   );and
                              );not
                         );and
                         (progn
                          (setq ina (getstring (strcat "\nImage name <"
                                                       ina2 ">: "
                                               );strcat
                                    );getstring
                          );setq
                          (if (equal ina "")
                              (setq ina ina2)
                          );if
                          (setq ina (xstrcase ina));setq
                          (if (not (snvalid ina))
                              (progn
                               (princ "\n*Invalid image name*")
 
                              );progn
                          );if
                         );progn then
                         (setq ina (getstring (strcat "\nImage name: "
                                              );strcat
                                   );getstring
                               ina (xstrcase ina)
                         );setq else
                     );if
                     (if (and (member (cons 3 ina) (entget na3))
                              (not (equal ina (xstrcase (cdr (assoc 3 e3)))))
                              (princ "\n*Invalid* That name is already in use.")
                         );and
                         (setq ina "")
                     );if
                    );while
                    (if (not (equal ina
                                    (xstrcase (cdr (assoc 3 e3)))
                             )
                        );not
                        (progn
                         (setq  e3 (entget na3)
                                e3 (member (cons 350 na2) (reverse e3))
                                e3 (cdr (cdr e3))
                                e3 (append (reverse e3)
                                           (list (cons 3 ina) (cons 350 na2) )
                                           (cdr (member (cons 350 na2) (entget na3)))
                                   );append
                         );setq
                         (entmod e3)
                        );progn then
                    );if
              );progn
          );if
         );progn then
     );if
     ;(alert (strcat "Pick OK when ready to re-load " ina))
     (command "_.-image" "_unload" ina)
     (command "_.-image" "_reload" ina)
    );progn then
);if
(acet-error-restore)
);defun c:imageedit
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Sets the default raster editing application to use in conjunction with the
;imageedit command.
(defun c:imageapp ( / a fna )
 
(acet-error-init (list nil nil))
(if (or (not (setq a (getenv "VIP_IMAGEEDIT_IMAGEAPP")))
        (equal a ".")
        (not (findfile a))
    );or
    (setq a ".")
);if
(while (or (not fna)
           (and (not (equal fna "."))
                (not (findfile fna))
                (princ "\nCannot find that file!")
           );and
       );or
(setq fna (getstring T
                     (strcat "\nRaster editing application or . for system default <"
                             a
                             ">: "
                     );strcat
          );getstring
      fna (acet-str-space-trim fna)
);setq
(cond
 ((equal fna "")
  (setq fna a)
 );cond #1
 ((equal fna "~")
  (setq fna (ACET-FILE-READDIALOG "Raster editing application" a "EXE" "Acet:Raster" 1664))
 );cond #2
);cond close
);while
(if (not (equal fna "."))
    (setq fna (findfile fna));setq
);if
(setenv "VIP_IMAGEEDIT_IMAGEAPP" fna)
 
(acet-error-restore)
);defun c:imageapp
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start_image_editor ( ina fna / fna2 a app msg flag)
 
(setq fna2 (acet-file-find-image fna))
(if (not fna2)
    (progn
     (alert (strcat "Cannot find image: " fna))
     (if (not #ihatch);global default file name.
         (setq #ihatch "")
     );if
     (setq fna (ACET-FILE-WRITEDIALOG "Image file name"
                         #ihatch
                         (strcat "bmp;rle;dib;rst;gp4;mil;cal;cg4;"
                           "flc;fli;gif;jpg;pcx;pct;png;tga;tif"
                         );strcat
                         "Acet:ImageEdit"
                         1664
               );ACET-FILE-WRITEDIALOG
     );setq
     (if (setq fna2 (acet-file-find-image fna))
         (progn
          (command "_.-image" "_path" ina fna)
          (command "_.-image" "_unload" ina)
         );progn
     );if
    );progn then
);if
(if fna2
    (setq fna fna2)
    (setq fna nil)
);if
(if (not (setq app (getenv "VIP_IMAGEEDIT_IMAGEAPP")))
    (progn
     (setq app ".")
     (setenv "VIP_IMAGEEDIT_IMAGEAPP" ".")
    );progn
);if
(if (equal app ".")
    (setq app (acet-filename-associated-app fna));setq
);if
(if (and fna
         app
         (or (setq a (findfile app))
             (and (getenv "PATH")
                  (setq a (acet-file-find-on-path app (getenv "PATH")))
             );and
         );or
    );and
    (progn
     (if (or (acet-str-m-find "IEXPLORE.EXE" (acet-filename-path-remove app))
             (acet-str-m-find "NETSCAPE.EXE" (acet-filename-path-remove app))
         );or
         (setq msg (strcat "\n   The associated application: "
                           "\n   \"" app "\""
                           "\n   is not suitable for editing the selected image."
                           "\n\n   You can use the IMAGEAPP command to specify a suitable "
                           "\n   application and override the system default."
                           "\n\n   Alternativley you can change the file association in"
                              "\n My Computer by clicking View and then clicking Options."
 
                   );strcat
         );setq then
     );if
 
     (if (not msg)
         (progn
 
          (setq app (acet-filename-ext-remove a)
                app (acet-str-replace "\"" "" app)
                fna (acet-str-replace "\"" "" fna)
          )
          (setq
                flag (acet-sys-spawn 1 app fna)
          );setq
          (if (< flag 0)
              (princ (strcat "\nError starting application \"" app "\"."))
          );if
         );progn then
         (alert msg);else
     );if
    );progn then
    (progn
    (if fna
         (progn
          (if (not app)
              (progn
               (alert (strcat "No associated application found for file extension: "
                              "\"" (vl-filename-extension fna) "\"."
                              "\n\n Create an association in My Computer by "
                              "clicking View and then clicking Options."
                      );strcat
               );alert
              );progn then association failed
              (progn
               (if (not a)
                   (alert "\nRaster editing application: \"" app "\" was not found.")
               );if
              );progn else just could not find the app.
          );if
         );progn then
     );if
    );progn else describe the error
);if
 
);defun start_image_editor


(princ)
