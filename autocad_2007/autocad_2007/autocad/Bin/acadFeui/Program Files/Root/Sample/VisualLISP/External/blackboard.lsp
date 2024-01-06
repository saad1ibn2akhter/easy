;;;                                                                    ;
;;;  BLACKBOARD.lsp                                                    ;
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
;;;	 Blackboard Sample
;;;
;;;
;;; Sample file supplied by: 	Perceptual Engineering Inc.
;;; web:			www.perceptual-eng.com
;;; Author:			Ralph Gimenez, Perceptual Engineering
;;;
;;;
;;; Functions Used:
;;;
;;; This example will demonstrate the use of the blackboard 
;;; functions vl-bb-set, vl-bb-ref, as well as vl-propagate and defun-q. 
;;;
;;;
;;;
;;;
;|

Running the Example:

   1. Open a new drawing
   2. Load blackboard.lsp file into Visual LISP. 
   3. Once blackboard.lsp is loaded, you will receive the message 
      'Blackboard Example Initialized'
   4. Execute the c:documents function at the command prompt.
      You will see a list printed on the screen: (("Drawing1.dwg"))
   5. Create a new drawing
   6. Execute the c:documents function again at the command prompt.
      You will see a list printed on the screen:
      (("Drawing2.dwg")("Drawing1.dwg"))

   You may repeat this as many times as desired including switching back
   and forth between documents and evaluating c:documents.


Overview:

    This file uses the blackboard feature as a communication pipeline
    between documents to:

    	1. Provide new and updated AutoLISP function definitions 
    	   to all currently displayed documents as well as any 
    	   future documents created by the new or open command 
    	   without using vl-propagate. 

    	2. Provide default initialization values to any new document without 
    	   redefining the s::startup function.

    	3. Use the blackboard as a initialization font.

    	4. Provide communications without the use of reactors.
	
    	5. Provide the developer an additional API that 
    	   will retrieve, evaluate and place values during an 
    	   initialization of a document (whether by new or open)
    	   without overburdening s::startup.

    	6. Enable any application to place any information that 
    	   is required to be evaluated without modifying 
    	   s::startup or ACAD2000doc.lsp.

	7. Display a list of drawings that have been opened or created during
           this example.

    Initial communication is accomplished using a redefinition of the 
    s::startup function (if it exists). Otherwise a new s::startup 
    function is created.




Specifications:

	What is the Blackboard?

		The Help file states... 

		VLISP provides a blackboard namespace for communicating the values of
		variables between namespaces. The blackboard is a namespace that is not
		attached to any document or VLX application. You can set and reference
		variables in the blackboard from any document or VLX. Use the vl-bb-set
		function to set a variable, and use vl-bb-ref to retrieve a variable's
		value. 

		Referencing Variables in Document Namespaces

		Variables defined in a separate-namespace VLX are not known to the
		document namespace associated with the VLX. However, a
		separate-namespace VLX can access variables defined in a document
		namespace using the vl-doc-ref and vl-doc-set functions. The vl-doc-ref
		function retrieves the value of a variable from a document namespace.
		The function requires a single argument, a symbol identifying the
		variable to be retrieved. For example, the following function call
		retrieves the value of a variable named aruhu:

		(setq var1 (vl-doc-ref 'aruhu))

		If executed within a document namespace, vl-doc-set is equivalent to the
		set function. The vl-doc-set function sets the value of a variable in a
		document namespace. The function requires two arguments: a symbol
		identifying the variable to be set, and the value to set the variable
		to. For example, the following function call sets the value of a variable
		named ulus:

		(vl-doc-set 'ulus "Go boldly to no one")

		If executed within a document namespace, vl-doc-ref is equivalent to the
		eval function. To set the value of a variable in all open document
		namespaces, use the vl-propagate function. For example, the following
		function call sets a variable named fooyall in all open document
		namespaces:

		(vl-propagate 'fooyall)

		This command not only copies fooyall into all currently open document
		namespaces, but also causes fooyall to be automatically copied to the
		namespace of any new drawings opened during the current AutoCAD session.

		Please Note:  
		       
		       Any function which has been "propagated" is available 
			   after AutoCAD has completed the initialization of a new 
			   or opened document. This limitation is evident when a
			   propagated function is used within the s::startup function.

			Example:

			   1. Define this code fragment in the Visual LISP console:

				(defun-q print-list (lst)
					 (foreach item lst
					    (prin1 item)
					   (terpri)
					   )
					 (princ)
					 )

				(vl-propagate 'print-list) ;; propagate the function to all documents

				(defun s::startup ()
				    (print-list '( 1 2 3 4 5 6 7 ))
				)
				(vl-propagate 's::startup) ;; propagate the function

			  2. Create a new drawing and watch the error produced by s::startup.

			     When s::startup is executed you will receive the error:

				; *** ERROR: no function definition: PRINT-LIST


	Sample Code Documentation:

	Mechanism and Flow:

	* Functions defined in this file 
	  and the mechanics behind the functions.
		
		The first function defined in this file is
        ac:register-function-to-blackboard.  This function is
        responsible for updating the value of the global list  
        *shared-functions* and placing its updated value to 
        the blackboard.

        The function ac:evaluate-Blackboard-items is called only 	
		within the body of the modified s::startup function. When 
		AutoCAD automatically calls the s::startup function whenever 
		a new drawing is created, s::startup calls 
		ac:evaluate-Blackboard-items, retrieving the current value 
		of the global list *shared-functions*.  Every item in this
		list is then evaluated within the context of the new 
		drawing.

 
        ac:register-mdi-drawing-names is a function that is placed 
		in the *shared-functions* variable. Moreover, this function 
		is called at each <drawing new> or <drawing open> command 
		because s::startup initializes the document with the
        current contents of the global variable *shared-functions*.


        This file (blackboard.lsp) when loaded attempts to find the 
		current definition of the s::startup function. If the 
		s::startup function is a list (defined by defun-q) the
		definition of the function named my-startup is appended to 
		the current definition of s::startup. The function 
		my-startup is the initializing function that is responsible 
		for evaluating every item in the blackboard variable named
        *shared-functions*.
		
	* Flow of the program
		
        During a <drawing new> or <drawing open> command, the 
		s::startup function is evaluated after AutoCAD has completed
		basic drawing initialization. The s::startup function then 
		inspects the current blackboard value of *shared-functions* 
		and if it is not nil, an eval is performed foreach item in 
		this list.

	* Potential Uses and Enhancements
	
        This example demonstrates just one of many designs for 
		creating an application that is able to share between 
		documents. It could be expanded to keep track of a bill
        of materials or large schedules that are generated on a
		separate drawing representing an entire collection or
		project of drawings. The options are endless.
				
    	General Comments:
		The functions s::startup and ac:register-function-to-
		blackboard mimic the vl-propagate function with the 
		exception that data passed to ac:register-function-to-
		blackboard is not immediately available to all documents.
		It is the responsibility of a document to retrieve the data
		when it is required by the application. In this manner the 
		application has the flexibility when to update its value 
		from the blackboard.
 		
|;

;;;
;;; ----------------------------------------------------------------------
;;;  This enables any application to "pre-populate"
;;;  definitions to the blackboard.
(setq regfunc 
'(defun ac:register-function-to-blackboard (funcAsList evaluate)
(if (equal (type funcAsList) 'list)
  (progn 
  (princ "\nRegistering ") (prin1 funcAsList)
  
  ;; Does the funcAsList need to be evaluated at this time?
  (if evaluate
  ;; if so, evaluate it.
     (eval funcAsList)
    )
    
;; Look at the value of *shared-functions*.
    
(if (vl-bb-ref '*shared-functions*)

;; If *shared-functions* is present, use the value from the
;; blackboard, cons the new item to the list ensuring that
;; it is added at the end of the list (note the double reverse).

  (setq	*shared-functions*
	 (reverse (cons funcAsList
	       (reverse (vl-bb-ref '*shared-functions*))
	       ))
	)
	;; otherwise, create the variable from scratch with the 
	;; desired initial value.
  (setq	*shared-functions*
	   (cons funcAsList
	         *shared-functions*)
	       )
	)
  )
)
;; Always update the blackboard value and return the value of
;; *shared-functions* as if setq was used.
  
(vl-bb-set '*shared-functions* *shared-functions*) 
  ))
  
  

;; evaluate the function ac:register-function-to-blackboard
;; so it can be used within this context.
(eval regfunc)

;; Now place it in the blackboard and do not evaluate it.
(ac:register-function-to-blackboard regfunc  nil )
;;; ----------------------------------------------------------------------


(setq regFunc
 '(defun ac:evaluate-Blackboard-items ()
;; automatically update any function in the blackboard.
;; variable *shared-functions*.
(if (vl-bb-ref '*shared-functions*)
  (progn
    (princ "\nEvaluating *shared-functions*")
    (foreach func (vl-bb-ref '*shared-functions*)
      (eval func)
      )
    )
  )
  ))

;;; register the function ac:evaluate-Blackboard-items
;;; and evaluate it now
(ac:register-function-to-blackboard regfunc  T )
;;; ----------------------------------------------------------------------


(setq regFunc 
  '(defun ac:register-mdi-drawing-names ()
  (if (null (member (getvar "dwgname") (mapcar 'car *ac:mdi-drawing-names*)))
    (progn
      (princ (strcat "\nadding " (getvar "dwgname") " to global list *ac:mdi-drawing-names*."))
      (setq
	;; retrieve the blackboard value for the
	;; variable *ac:mdi-drawing-names*
	*ac:mdi-drawing-names*
	 (vl-bb-ref '*ac:mdi-drawing-names*)
	;; change the value of *ac:mdi-drawing-names*
	*ac:mdi-drawing-names*
	 (cons (list (getvar "dwgname"))
	       *ac:mdi-drawing-names*
	       )
	)
      ;; place the value of *ac:mdi-drawing-names* into the blackboard.
      (VL-BB-SET '*ac:mdi-drawing-names* *ac:mdi-drawing-names*)
      ;; retrieve the blackboard value for the variable 
	;;*ac:mdi-drawing-names*
      (setq *ac:mdi-drawing-names* (vl-bb-ref '*ac:mdi-drawing-names*))

      )
    )
  )
      )

;;; register the function ac:register-mdi-drawing-names
;;; and evaluate it now
(ac:register-function-to-blackboard regfunc  T )
;;; ----------------------------------------------------------------------
      
(setq regfunc
;;; c:documents returns a history list of all opened
;;; or created documents during an AutoCAD session.
'(defun c:documents ()
(princ (vl-bb-ref '*ac:mdi-drawing-names*))
  (princ)
  )
)

(ac:register-function-to-blackboard regfunc T)
;;; ----------------------------------------------------------------------

;;; register the function call to ac:register-mdi-drawing-names
;;; and evaluate it now
(ac:register-function-to-blackboard '(ac:register-mdi-drawing-names) T)
;;; ----------------------------------------------------------------------


;;; make s::startup a native AutoLISP compatible defun 
(defun-q my-startup ()
    (princ "\nStartup Created in blackboard.lsp")
	;; automatically update any function in the blackboard.
	;; variable *shared-functions*.
	(if (vl-bb-ref '*shared-functions*)
	  (progn
	    (princ "\nEvaluating *shared-functions*")
	    (foreach func (vl-bb-ref '*shared-functions*)
	      (eval func)
	      )
	    )
	   (progn
	     (princ "\nThe Blackboard Variable *shared-functions* is nil")
	     (princ)	     
	     )
	  )
    (princ)
  )

;; we assume s::startup has been defined using defun-q
;;
(if (listp s::startup)
  ;; A response of T indicates that it is a list
     (setq s::startup (append s::startup
			      	(cdr my-startup) ; remove the nil
			      ))
  ;; s::startup is not a list so we create our own
  ;; s::startup function using my-startup 
     (setq s::startup my-startup)
  )

(princ "\nValue of s::startup")
(prin1 s::startup)

;;; s::startup is automatically evaluated by AutoCAD.
;;; All we need to do is propagate the function to all
;;; new documents.
;;;
;;; Note: By default it will be present
;;; in all existing documents.

(vl-propagate 's::startup)

;;;(princ "\nThe value of *shared-functions* (within this document) is: ") (prin1 *shared-functions*)
;;;(terpri)
;;;(princ "\nThe value of *shared-functions* ( as defined in the blackboard) is: ")
;;;(prin1 (vl-bb-ref '*shared-functions*))

(princ "\nBlackboard Example Initialized")
(princ)
