// Next available MSG number is     7 
// MODULE_ID VIEWSLID_DCL_
/* Next available MSG number is  24 */

//----------------------------------------------------------------------------
//
//   VIEWDLID.DCL   Version 1.0
//
//     Copyright (C) 1991-1994,1997 by Autodesk, Inc.
//
//     Permission to use, copy, modify, and distribute this software
//     for any purpose and without fee is hereby granted, provided
//     that the above copyright notice appears in all copies and
//     that both that copyright notice and the limited warranty and
//     restricted rights notice below appear in all supporting
//     documentation.
//
//     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
//     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
//     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
//     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
//     UNINTERRUPTED OR ERROR FREE.
//
//     Use, duplication, or disclosure by the U.S. Government is subject to
//     restrictions set forth in FAR 52.227-19 (Commercial Computer
//     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
//     (Rights in Technical Data and Computer Software), as applicable.
//
//.     
//   
//----------------------------------------------------------------------------
//
//  Viewslid.dcl - For use with dlgtest.cc
//   (Test Program for User Programmable Dialog Boxes)
//
//----------------------------------------------------------------------------

//dcl_settings : default_dcl_settings { audit_level = 3; }

viewslide : dialog {
    label = "View Color Wheel Slide";
    : column {
        : image {
            key = "color_whl";
            color = graphics_background;
            aspect_ratio = 1.33;
            height = 7;
    	    width = 10;
        }
        spacer_1;
        : row {
            fixed_width = true;
            alignment = centered;
            ok_button;
            : spacer { width = 2; }
            : button {
                key = "subdlgtest";
                label = "Test subdlg...";
            }
        }
    }
}

subdlg : dialog {
    label = "Test Nested Dialogs";
    : column {
	    spacer_1;
        : text {
            label = "Nested Sub-dialog Test OK";
        }
	    spacer_1;
        : toggle {
            label = "Allow terminate";
            alignment = left;
            key = "term_on_off";
        }
	    spacer_1;
        : row {
            fixed_width = true;
            alignment = centered;
            ok_button;
            : spacer { width = 2; }
            : button {
                key = "terminate";
                label = "Term all";
            }    
        }
    }
} 

