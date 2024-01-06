;;;                                                                    ;
;;;  MOUSE REACTOR.LSP                                                 ;
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
;;;
;;;
;;;	 Mouse Reactor and Dynamic Custom Popup
;;;
;;;
;;; Sample file supplied by: 	Perceptual Engineering Inc.
;;; web:			www.perceptual-eng.com
;;; Author:			Ralph Gimenez, Perceptual Engineering

;|
Overview:

	This file uses the Visual LISP ActiveX menu API and a
	mouse reactor that enables the creation of a custom
	popup associated with a mouse right click.

    
Specifications:

	Provide the end user a different method by which to change object's
	color.
	
	This must be performed without having to first select the object
	(displaying the object's grips) select an object grip, performing a
	right click to display the grips menu, select properties and lastly
	display the properties navigator.

	
	Example Code Goals:	


    	1. Provide a method using a right mouse click to display
    	   a custom menu with the use of a mouse reactor. 

    	2. Create a custom popup menu dynamically at runtime
    	   without the need to write a menu or mns file.
    	
    	3. Create a mouse reactor that displays its actions 
           for a double click and a right click. 


Running the Example:

	1. Open a new drawing
   
	2. Draw a few objects on the screen. The type of object is unimportant.
   
	3. Load Mouse-Reactor.lsp file into Visual LISP. 
   
	4. Once Mouse-Reactor.lsp is loaded, you will receive the message 'Mouse
	Reactor Example Loaded!'
      
	5. Execute the c:mouse-popup-on function at the command prompt to enable
	the mouse reactor and create a custom popup using Visual LISP new
	capability dynamic menu creation.
      
	You will see the menu items printed on the screen as they are created:
      
	Command: mouse-popup-on

	(#<VLA-OBJECT IAcadPopupMenu 022db528> 0 "&Enter" "\n")
	(#<VLA-OBJECT IAcadPopupMenu 022db528> 2 "&Move" "_Move ")
	(#<VLA-OBJECT IAcadPopupMenu 022db528> 3 "&Erase" "_Erase ")
	(#<VLA-OBJECT IAcadPopupMenu 022db528> 4 "&Cop&y" "_Copy ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 0 "&Bylayer" "(ac:Change-Color 256) ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 1 "&Red" "(ac:Change-Color 1) ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 2 "&Yellow" "(ac:Change-Color 2) ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 3 "&Green" "(ac:Change-Color 3) ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 4 "&Cyan" "(ac:Change-Color 4) ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 5 "&Blue" "(ac:Change-Color 5) ")
	(#<VLA-OBJECT IAcadPopupMenu 022e7c78> 6 "&Magenta" "(ac:Change-Color 6) ")
	Custom menu popup enabled!

	------------

	6. Once the mouse reactor is enabled place your crosshairs directly
	   above any object. Now perform a right click with the mouse. You will
	   notice a pop up menu which is different than the default popup displayed
	   by AutoCAD. Once the custom popup is displayed you may select any option
	   on the menu.

	7. To display the default mouse popup simply move the crosshairs away
	   from any object and perform a right click. 

	8. To temporarily disable the popup menu execute mouse-popup-off
	   at the command prompt.

	Command:  MOUSE-POPUP-OFF
	All menu items have been removed from:
	; menugroup = ACAD
	; Name = Custom-Menu

|;




;;; The premise of this example is to provide a mouse reactor that is triggered
;;;	when a right click is issued and an object is directly underneath
;;;	the crosshair's position. If the right click is produced and an object is not
;;; found, the original acad popup is displayed.



;;; This example utilizes the following Visual LISP pragma directives:
;;;
;;; pragma
;;;	unprotect-assign
;;;	protect-assign
;;;
;;;
;;; This example utilizes the following Visual LISP functions:
;;;	
;;;	vl-prin1-to-string 
;;;	vl-princ-to-string 
;;;	vla-add 
;;;	vla-addmenuitem 
;;;	vla-addseparator 
;;;	vla-addsubmenu 
;;;	vla-delete 
;;;	vla-get-color 
;;;	vla-get-menugroups 
;;;	vla-get-menus 
;;;	vla-get-name 
;;;	vla-get-namenomnemonic 
;;;	vla-get-objectname 
;;;	vla-get-parent 
;;;	vla-get-tagstring 
;;;	vla-insertinmenubar 
;;;	vla-put-check 
;;;	vla-put-color 
;;;	vla-saveas 
;;;	vlax-ename->vla-object 
;;;	vlax-for 
;;;	vlax-get-acad-object 
;;;	vlax-property-available-p 
;;;	vlax-variant-type 
;;;	vlaxx-integer-p 
;;;	vlaxx-real-p 
;;;	vlaxx-string-p 
;;;	vlr-mouse-reactor 
;;;	vlr-remove-all 
;;;

;;; to display the current color on the popup set the value of
;;;  *enableColor* to T
;;; --------------------------------------------------------------------------
;;; Start constants
;;; un-protect the global *MenuGroupname*
(pragma '((unprotect-assign *MenuGroupname*)))

;; now set the value of *MenuGroupname*
(setq *MenuGroupname*  "acad")

;; protect the global *MenuGroupname*
(pragma '((protect-assign *MenuGroupname*)))
(setq  *enableColor*  T)
(setvar "cmdecho" 0)

(vl-load-com) 	   ;; load vla ActiveX
(vl-load-reactors) ;; and reactors
;;; End constants
;;; --------------------------------------------------------------------------

;;; Globals
;;;	*Custom-Popup*		Variable that contains the Run-time Custom menu
;;; *MenuGroupname*		The menugroup name we will use to place the custom menu. 
;;; *mouse*				Mouse reactor.

;;; --------------------------------------------------------------------------
;;; clean function during debugging.
;;; Returns T
(defun ac:CleanMouseReactor	()
  (setq *mouse* nil)					; clear the variable
  (vlr-remove-all :VLR-Mouse-Reactor)
   T
  )
;;; --------------------------------------------------------------------------
;;; mouse reactor function
;;; Returns the newly created mouse reactor.
(defun ac:mouse	(data)
  ;; mouse reactors are document based
  (VLR-Mouse-Reactor
	data
	'(
	  (:VLR-beginDoubleClick . ac:beginDoubleClick)
	  (:VLR-beginRightClick . ac:beginRightClick)
	  )
	)
  )
;;; --------------------------------------------------------------------------
;;; reactor call back for a double click
;;; Returns nothing
(defun ac:beginDoubleClick (reactorObject Listofsomething)
  (princ "\nbeginDoubleClick Even Callback: ")
;;;  (princ (list reactorObject Listofsomething))

  (alert (strcat
		   "Begin Double Click"
		   "\nReactor Object: "
		   (vl-princ-to-string reactorObject)
		   "\nClick Point:"
		   (vl-princ-to-string Listofsomething)
		   )
		 )
  (princ)
  )

;;; --------------------------------------------------------------------------
;;; reactor call back for a right click
;;; Returns nothing
(defun ac:beginRightClick (reactorObject Listofsomething)
  (princ "\nbeginRightClick Even Callback: ")
;;;  (princ (list reactorObject Listofsomething))
  (setq	*objectUnderCursor*
		 (if (setq data (nentselp "" (car Listofsomething)))
		   (vlax-ename->vla-object (car data))
		   )
		)

  (if (and *objectUnderCursor*
		   (equal (getvar "cmdnames") "") ; no present command
		   )
	(progn
	  (princ (strcat "\nObject \""
					 (vla-get-ObjectName *objectUnderCursor*)
					 "\"\ under Cursor!\n"
					 )
			 )
;;; if there are too many calculations the original right click menu appears.
;;; To test this out set the variable *enableColor* to T if you want to
;;; peek and check the color of the object. Otherwise set it to nil. If the
;;; variable *enableColor* is T most likely there will be two
;;; popups displayed, one after the other.
	  
(if *enableColor*
  (progn
	  (foreach item *colors*
		   (vla-put-Check (cadr item) :vlax-false)
		)
	  (if (assoc (vla-get-color *objectUnderCursor*) *colors*)
		  (vla-put-check (cadr (assoc (vla-get-color *objectUnderCursor*) *colors*)) :vlax-true)
		)
	))
	  ;; --------

	  
	  (menucmd (strcat "p0=" *MenuGroupname* ".Custom-Menu"))
	  (menucmd "p0=*")
	  )
	
	(progn
	  (princ "\nNo Object under Cursor!\n")
	  ;; always return the pop0 to the acad system.
	  (menucmd "p0=acad.pop0")
	  )
	)
  (princ)
  )




;;; --------------------------------------------------------------------------
;;; enables the custom popup menu
;;; Returns nothing
(defun c:mouse-popup-on ()
  (if (not *mouse*)
	(progn
	  (setq *mouse* (ac:mouse nil))
	  (princ "\nMouse reactor enabled!\n")
	  )
	)
  (if (or (null *Custom-Popup*)
		  (null (ac:get-Menu-Items *Custom-Popup*))
		  )
	(progn
	  (ac:addCustomPopupMenu)
	  (princ "\nCustom menu popup enabled!\n")
	  )
	)
  (princ)
  )
;;; --------------------------------------------------------------------------
;;; disables the custom popup menu
(defun c:mouse-popup-off	()
  (if *Custom-Popup*
	(progn 
	(mapcar 'vla-delete (ac:get-Menu-Items *Custom-Popup*))
	
  ;; always return the pop0 to the acad system.
  (menucmd "p0=acad.pop0")
  (princ
	(strcat
	  "\nAll menu items have been removed from: \n; menugroup = "
	  (vla-get-name (vla-get-parent (vla-get-parent *Custom-Popup*)))
	  "\n; Name = "
	  (vla-get-name *Custom-Popup*)
	  "\n"
	  )
	)
	))
  (princ)
  )
;;; --------------------------------------------------------------------------
;;; returns a list of menuObjects currently loaded in acad.
;;; presently not used.
(defun ac:Menus	(/ acad menuGroups dl)
  (setq	acad			 (vlax-get-acad-object)
		*acadmenuGroups* (vla-get-MenuGroups acad)
		)
  (vlax-for Item *acadmenuGroups* (setq dl (cons item dl)))
  (reverse dl)
  )

;;; --------------------------------------------------------------------------
;;; returns a menuObject. The parameter must be a valid menugroup
(defun ac:ReturnMenuObject (MenuName / dl)
  (if (menugroup MenuName)
	(progn
	  (vlax-for	Item (vla-get-MenuGroups (vlax-get-acad-object))
		(setq dl (cons (list (strcase (vla-get-name item)) item) dl))
		)
	  (cadr (assoc (strcase MenuName) dl))
	  )
	)
  )
;;; --------------------------------------------------------------------------
;;; Predicate for a string
;;; Returns T if successfull nil otherwise.
(defun vlaxx-string-p (arg)
  (and (equal (type arg) 'str))
  )
;;; --------------------------------------------------------------------------
;;; Predicate for an integer 
;;; Returns T if successfull nil otherwise.
(defun vlaxx-integer-p (arg)
  (and (equal (type arg) 'int))
  )
;;; --------------------------------------------------------------------------
;;; Predicate for a real number
;;; Returns T if successfull nil otherwise.
(defun vlaxx-real-p	(arg)
  (and (equal (type arg) 'real))
  )
;;; --------------------------------------------------------------------------
;;; adds a menu item to a popupobject.
;;; This function is identical to vla-AddMenuItem except that
;;; error checking is performed. Returns the menuitem
;;; object if successful. If an error is encountered an error
;;; message is printed and the function returns nil.
(defun ac:Add-Menu-Item	(ParentMenuObject Index Label Macro / res)
  (print (list ParentMenuObject Index Label Macro))
  (if (and (vlaxx-string-p Label)
		   (or (vlaxx-integer-p Index)
			   (vlaxx-string-p Index)
			   (equal (vlax-variant-type Index) 2)
										; is it a variant integer?
			   (equal (vlax-variant-type Index) 8)
										; is it a variant String?
			   )
		    (vlaxx-string-p Macro)
		   )
	;; now check for pop menu Object
	(if
	  (and (equal (type ParentMenuObject) 'vla-object)
		   ;; Check if its a IAcadPopupMenu:
		   (vlax-property-available-p ParentMenuObject "ShortcutMenu")
		   )
	   (progn

		 (setq res (vla-AddMenuItem ParentMenuObject Index Label Macro))

		 )
	   (princ
		 "\nError: ParentMenuObject is not a valid pop up menu object"
		 )
	   )
	(princ "\nError: Index, Label or Macro are not correct")
	)
  res
  )
;;; --------------------------------------------------------------------------
;; Dumps a menuitem
;; use ac:get-Menu-Items to retrieve individual menu items. such as:
;;;	_$ (mapcar 'ac:dump-MenuItem (ac:get-Menu-Items *Custom-Popup*))
;;;
;;;	; APPLICATION = #<VLA-OBJECT IAcadApplication 00d935f0>
;;;	; CAPTION = "&Line"
;;;	; CHECK = :vlax-false
;;;	; ENABLE = :vlax-true
;;;	; HELPSTRING = ""
;;;	; INDEX = 0
;;;	; LABEL = "&Line"
;;;	; MACRO = "\003\003_Line\n"
;;;	; PARENT = #<VLA-OBJECT IAcadPopupMenu 014d4648>
;;;	; TAGSTRING = "ID_Line"
;;;	; TYPE = 0
;;; ...
;;;	(T T T T T)
;;;	_$ 
;;; Prints an object dump of a menuitem. Always returns T
;;; Similar to vlax-dump-Object but without the error
;;; encountered by querying the Submenu property if the object
;;; does not have a submenu.
(defun ac:dump-MenuItem	(item)
  (princ
	(apply 'strcat
		   (mapcar
			 (function
			   (lambda (x)
				 (strcat
				   "\n; "
				   (vl-princ-to-string x)
				   " = "
				   (vl-prin1-to-string
					 (eval
					   (list (read
							   (strcat "vla-get-" (vl-princ-to-string x))
							   )
							 item
							 )
					   )
					 )
				   )
				 )
			   )

			 '(Application			 Caption	Check	   Enable
			   HelpString Index		 Label		Macro	   Parent
										; Submenu ; this causes an error if the item is not a submenu
			   Tagstring  Type
			   )
			 )
		   )
	)
  (terpri)
  T
  )
;;; --------------------------------------------------------------------------
;;; searches for a popup label in a popupmenu object
;;; returns the popupmenu object is it exists.
(defun ac:MenuPopExist (MenuGroupObject popupLabel / dl)
  (vlax-for	item MenuGroupObject
	(setq dl (cons
			   (list (strcase (vla-get-NameNoMnemonic item))
					  item ) dl))
	)
   (cadr (assoc (strcase popuplabel)  dl))
  )
;;; --------------------------------------------------------------------------
;;; searches for an id string label in a popupmenuitem object
;;; returns the popupmenu object is it exists.
(defun ac:MenuItemLabelExist (MenuPopupObject itemLabel / dl)
  (vlax-for	item   MenuPopupObject 
	(setq dl (cons
			   (list (strcase (vla-get-TagString item))
					  item ) dl))
	)
   (cadr (assoc (strcase itemLabel)  dl))
  )
;;; --------------------------------------------------------------------------
;;; returns a list of all menuitem objects contained within a popupmenu object
(defun ac:get-Menu-Items (popup / dl)
  (if popup
	(progn
	  (vlax-for	MenuItem POpup
		(setq dl (cons MenuItem dl))
		)
	  (reverse dl)
	  )
	)
  )
;;; --------------------------------------------------------------------------
;;; Adds a specific custom popup menu
;;; to AutoCAD. Returns the newly created popupmenu Object.
(defun ac:addCustomPopupMenu ()
  (if (or (null *Custom-Popup*)
		  (null (ac:get-Menu-Items *Custom-Popup*))
		  )
	(progn
	  (setq	acadMenuObject	   (ac:ReturnMenuObject *MenuGroupname*)

			acadPopupMenuGroup (vla-get-Menus acadMenuObject)
			)
	  (if
		(not (ac:MenuPopExist acadPopupMenuGroup "Custom-Menu"))
		 (setq *Custom-Popup*
				(vla-add acadPopupMenuGroup "Custom-Menu")
			   )
		 )
	  )
	)
  (setq
	item0		 (ac:Add-Menu-Item *Custom-Popup* 0 "&Enter" "\n")
	item1		 (vla-AddSeparator *Custom-Popup* 1)
	item2		 (ac:Add-Menu-Item *Custom-Popup* 2 "&Move" "_Move ")
	item3		 (ac:Add-Menu-Item *Custom-Popup* 3 "&Erase" "_Erase ")
	item4		 (ac:Add-Menu-Item *Custom-Popup* 4 "&Cop&y" "_Copy ")
	;; add a separator 
	item5		 (vla-AddSeparator *Custom-Popup* 5)
	;; add a submenu
	*ColorSubMenu* (vla-AddSubMenu
				   *Custom-Popup*
				   8
				   "C&hange Color"
				   )
	*BylayerMenuItem*  (ac:Add-Menu-Item
				   *ColorSubMenu*
				   0
				   "&Bylayer"
				   "(ac:Change-Color 256) "
				   )
	*RedMenuItem*		 (ac:Add-Menu-Item
				   *ColorSubMenu*
				   1
				   "&Red"
				   "(ac:Change-Color 1) "
				   )
	*YellowMenuItem*		 (ac:Add-Menu-Item
				   *ColorSubMenu*
				   2
				   "&Yellow"
				   "(ac:Change-Color 2) "
				   )

	*GreenMenuItem*		 (ac:Add-Menu-Item
				   *ColorSubMenu*
				   3
				   "&Green"
				   "(ac:Change-Color 3) "
				   )
	*CyanMenuItem*		 (ac:Add-Menu-Item
				   *ColorSubMenu*
				   4
				   "&Cyan"
				   "(ac:Change-Color 4) "
				   )
	*BlueMenuItem*
				 (ac:Add-Menu-Item
				   *ColorSubMenu*
				   5
				   "&Blue"
				   "(ac:Change-Color 5) "
				   )
	*MagentaMenuItem*
				 (ac:Add-Menu-Item
				   *ColorSubMenu*
				   6
				   "&Magenta"
				   "(ac:Change-Color 6) "
				   )
	)

  (setq	*colors* (list (list 256 *BylayerMenuItem*)
					   (list 1 *RedMenuItem*)
					   (list 2 *YellowMenuItem*)
					   (list 3 *GreenMenuItem*)
					   (list 4 *CyanMenuItem*)
					   (list 5 *BlueMenuItem*)
					   (list 6 *MagentaMenuItem*)
					   )
		)
  
  *Custom-Popup*
  )


;;; --------------------------------------------------------------------------
;; Changes the color of the Object contained in the global variable
;; global variable *objectUnderCursor*. See function ac:beginRightClick
;; for its settings.
(defun ac:Change-Color ( whatColor )
	 (if *objectUnderCursor*
	     (vla-put-color *objectUnderCursor* whatColor)
	   )
  )

;;; --------------------------------------------------------------------------
;;; (vla-SaveAs acadMenuObject "c:/acad/mystuff.mns" acPartialMenuGroup)
;;; Once you save the menu look at ***CUSTOM-MENU

(defun ac:SavePOpup (Object Filename)
  (vla-SaveAs Object Filename acPartialMenuGroup))


(princ "Mouse Reactor Example Loaded!")
(princ)
