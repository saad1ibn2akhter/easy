//
//    OVERKILL.DCL
//    Created by Randy Kintzley 7/19/99
//
//    Copyright © 1999 by Autodesk, Inc.
//
//    Your use of this software is governed by the terms and conditions of the
//    License Agreement you accepted prior to installation of this software.
//    Please note that pursuant to the License Agreement for this software,
//    "[c]opying of this computer program or its documentation except as
//    permitted by this License is copyright infringement under the laws of
//    your country.  If you copy this computer program without permission of
//    Autodesk, you are violating the law."
//
//    AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
//    AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
//    MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
//    DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
//    UNINTERRUPTED OR ERROR FREE.
//
//    Use, duplication, or disclosure by the U.S. Government is subject to
//    restrictions set forth in FAR 52.227-19 (Commercial Computer
//    Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
//    (Rights in Technical Data and Computer Software), as applicable.
//
//  ----------------------------------------------------------------

overkill : dialog {
  label = "OVERKILL";
  : boxed_row {
    label="Object comparison settings";
     : column {
      : toggle {
        label = "Ignore LAYERS";
        key = "layer";
        mnemonic = "L";
        fixed_width = true;
      }
      : toggle {
        label = "Ignore LINETYPE";
        key = "linetype";
        mnemonic = "T";
        fixed_width = true;
      }
      : toggle {
        label = "Ignore COLOR";
        key = "color";
        mnemonic = "C";
        fixed_width = true;
      }
      : toggle {
        label = "Ignore LINEWEIGHT";
        key = "lineweight";
        mnemonic = "W";
        fixed_width = true;
      }
      : toggle {
        label = "Ignore PLOTSTYLE";
        key = "plotstyle";
        mnemonic = "S";
        fixed_width = true;
      }
     } // column
      spacer;
      : edit_box {
      label="Numeric fuzz";
      fixed_width=true;
      key="fuz";
      width=15;
      edit_width=15;
      alignment=top;
      }
   } // boxed row
//   spacer;
   : boxed_column {
    label="Lines, Arcs and Plines";
      : toggle {
        label = "PLINES - Optimize segments within plines";
        key = "plines";
        mnemonic = "P";
      }
      : toggle {
        label = "OVERLAP - Combine co-linear objects that partially overlap";
        key = "partial";
        mnemonic = "V";
      }
      : toggle {
        label = "END to END - Combine co-linear objects when aligned end to end";
        key = "endtoend";
        mnemonic = "E";
      }
   } // boxed column
  spacer;
  : row
  {
    spacer;
    spacer;
    spacer;
    spacer;
    spacer;
    spacer;
    spacer;
    spacer;
//    spacer;
//    spacer;
//    spacer;
    : button {
      fixed_width=true;
      width=13;
      key="accept";
      is_default=true;
      label= "OK";
    }
    : button {
      fixed_width=true;
      width=13;
      is_cancel=true;
      key="cancel";
      label= "Cancel";
    }
    : button {
      fixed_width=true;
      width=13;
      key="help";
      label= "&Help";
      is_help=true;
    }
//    spacer;
  } // row
  : text_part
  {
    key="error";
    label="";
  }
}
