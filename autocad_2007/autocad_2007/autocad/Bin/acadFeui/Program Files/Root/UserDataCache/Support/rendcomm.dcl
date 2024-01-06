// Next available MSG number is   341 
// MODULE_ID RENDCOMM_DCL_

//     Copyright (C) 1991-1997 by Autodesk, Inc.
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

//***************************************************************************
//
// Common Render Dialogue Control Language (DCL) -- Version 1.0
//
//***************************************************************************

//**************************************************************************
// Support dialog box code

var_text : text_part { label = ""; }

//**************************************************************************
// Information dialog box

pref_info : dialog {
    label = "Render Information";

    : text_part {
        alignment = centered;
        label = "AutoCAD Render";
    }
    : var_text {
        alignment = centered;
        key = "prodname";
    }
    : var_text {
        alignment = centered;
        key = "cyears";
    }
    : text_part {
        alignment = centered;
        label = "by Autodesk, Inc.  All Rights Reserved.";
    }
    : var_text {
        alignment = centered;
        key = "release";
    }
    spacer_1;
    : text_part {
        alignment = left;
        label = "Current Configuration:";
    }
    : concatenation {
        : text_part {label = "Rendering: "; }
        : var_text {
            key = "rendering";
            width = 60;
        }
    }
    : concatenation {
        : text_part { label = "           "; }
        : var_text {
            key = "rendname";
            width = 60;
        }
    }
    : concatenation {
        : text_part { label = "Hardcopy: "; }
        : var_text {
            key = "hardcopy";
            width = 60;
        }
    }
    : concatenation {
        : text_part { label = "           "; }
        : var_text {
            key = "hardname";
            width = 60;
        }
    }
    : list_box {
        key = "extras";
        height = 5;
        width = 60;
        multiple_select = false;
        allow_accept = false;
    }
    spacer_1_ok_only;
}


//***************************************************************************
// For showing real numbers.
 
text_part_12 : text_part { width = 12; }
 
//***************************************************************************
// Standard size list_box
 
list_box_8x8 : list_box {
    height = 8;          // (10x10) width includes scrollbar.
}
 
//***************************************************************************
// For File name, where max filename = 100.
 
edit_box_100 : edit_box {
    edit_width = 14;
    edit_limit = 100;
}
 
//***************************************************************************
// For Directory paths , where max pathname = 256.
 
edit_box_256 : edit_box {
    edit_width = 14;
    edit_limit = 256;
}
 
//***************************************************************************
// For File names and the like.
 
edit_box_14 : edit_box {
    edit_width = 14;
    edit_limit = 14;
}
 
//***************************************************************************
// For names that are <= 8 characters long.
 
edit_box_8 : edit_box {
//  edit_width = 8;
    edit_width = 15;  // used since Windows has variable width fonts and
                      // proteus uses the wrong width
    edit_limit = 8;
}
 
//***************************************************************************
// For names that are <= 16 characters long.
 
edit_box_16 : edit_box {
    edit_width = 16;
    edit_limit = 16;
}
 
//***************************************************************************
// For real numbers 0.00->1.00.
 
edit_box_4 : edit_box {
    edit_width = 4;
    edit_limit = 4;
}
 
//***************************************************************************
// For real numbers 0.000->1.000 (or nE-mm)
 
edit_box_6 : edit_box {
    edit_width = 6;
    edit_limit = 6;
}
 
//***************************************************************************
// For real numbers 0.00->1.00.
 
slider_0_1 : slider {
    min_value = 0;
    max_value = 100;
    small_increment = 1;
    big_increment = 10;
    is_tab_stop = false;        //  We have edit_boxes for all
}
 
//***************************************************************************
// Fixed slider for real numbers 0.00->1.00.
 
slider_0_1_fixed : slider {
    min_value = 0;
    max_value = 100;
    width = 16;
    fixed_width = true;
    alignment = centered;
    small_increment = 1;
    big_increment = 10;
    is_tab_stop = false;        //  We have edit_boxes for all
}
 
//***************************************************************************
//  Spacer tiles
 
spacer_0_1: spacer {
    height = 0.1;
}
 
spacer_0_25: spacer {
    height = 0.25;
}
 
spacer_0_5: spacer {
    height = 0.5;
}
 
spacer_1_25 : spacer {
    height = 1.25;
}
 
spacer_1_5 : spacer {
    height = 1.5;
}
 
//***************************************************************************
//  Dialogs with Cancel as the default.
 
cancel_ok_48 : dialog {
    width = 48;
    key = "dialog";
    initial_focus = "cancel"; // Doesn't work allways
    children_alignment = centered;
    spacer_1;
    : var_text { key = "line1"; }
    : var_text { key = "line2"; }
    cancel_ok;                          // Makes "Cancel" the default.
}

cancel_ok_32 : dialog {
    width = 32 ;
    key = "dialog";
    initial_focus = "cancel"; // Doesn't work allways
    children_alignment = centered;
    spacer_1;
    : var_text { key = "line1"; }
    : var_text { key = "line2"; }
    cancel_ok;                          // Makes "Cancel" the default.
}

cancel_ok : column {        // Makes "Cancel" the default.
    spacer_1;
    : row {
        fixed_width = true;
        alignment = centered;
        : ok_button { is_default = false; }
        : spacer { width = 2; }
        : cancel_button { is_default = true; }
    }
}


//***************************************************************************
//  Generic botton-line button combinations
 
spacer_0_1_ok_cancel_help_errtile : column {
    spacer_0_1;
    ok_cancel_help_errtile;
}
 
spacer_1_ok_cancel_help_errtile : column {
    spacer_1;
    ok_cancel_help_errtile;
}
 
spacer_1_ok_cancel_help : column {
    spacer_1;
    ok_cancel_help;
}
 
spacer_1_ok_help : column {
    spacer_1;
    : row {
        fixed_width = true;
        alignment = centered;
        : ok_button { is_cancel = true; }
        : spacer { width = 2; }
        help_button;
    }
}
 
spacer_1_ok_only : column {
    spacer_1;
    ok_only;
}

 
//***************************************************************************
// Define common widgets
 
button_new : button {
    key = "new";
    label = "New...   ";
    mnemonic = "N";
}
 
button_mod : button {
    key = "modify";
    label = "Modify...";
    mnemonic = "M";
    is_enabled = false;     // Enable when Items are selected.
}
 
button_dup : button {
    key = "duplicate";
    label = "Duplicate...";
    mnemonic = "u";
    is_enabled = false;     // Enable when Items are selected.
}
 
button_del : button {
    key = "delete";
    label = "Delete   ";
    mnemonic = "D";
    is_enabled = false;     // Enable when Items are selected.
}
 
button_imp : button {
    key = "import";
    label = "Materials Library...";
    mnemonic = "L";
}
 
button_exp : button {
    key = "export";
    label = "Export   ";
    mnemonic = "x";
}
 
button_pkt : button {
    key = "pickit";
    label = "Select <   ";
    mnemonic = "S";
}

/*****************************************************************************/
/********************** GENERIC COLORSYSTEM SLIDER SET ***********************/
/*****************************************************************************/
color_system_set : row {
    key = "color_system_set";
    : column {
        : text {
            key = "Red_txt";
            label = "Hue:";
        }
        : text {
            key = "Green_txt";
            label = "Lightness:";
        }
        : text {
            key = "Blue_txt";
            label = "Saturation:";
        }
    }
    : column {
        : edit_box_4 {
            key = "red_edit";
        }
        : edit_box_4 {
            key = "green_edit";
        }
        : edit_box_4 {
            key = "blue_edit";
        }
    }
    : column {
        : slider_0_1_fixed { key = "red_slider"; }
        : slider_0_1_fixed { key = "green_slider"; }
        : slider_0_1_fixed { key = "blue_slider"; }
    }
}


//***************************************************************************
// Black Cat dialog boxes
//***************************************************************************
//3DS input
object_list : list_box
{
    label   = "Object Name:          Type:";
    tabs    = "18";
    height  = 7;
    width   = 28;
    multiple_select = true;
}


//***************************************************************************
//
//***************************************************************************
// this is a clone of ok_cancel_help, without a default button
okNoDef_cancel_help : column 
{
    : row 
    {
        fixed_width = true;
        alignment = centered;
        : ok_button 
        {
            is_default      = false;
        }
        : spacer { width = 2; }
        cancel_button;
        : spacer { width = 2; }
        help_button;
    }
}
//***************************************************************************
//
//***************************************************************************
// 
bc3dsin : dialog
{
    label   = "3D Studio File Import Options";

    : column {
        : row {
            : column {
                : boxed_column {
                    label           = "Available Objects";
                    : object_list {
                        key     = "available";
                    }
                    : row {
                        children_fixed_width    = true;

                        spacer_1;
                        : button {
                            key     = "selall";
                            label   = "Add All";
                            mnemonic = "A";
                            /*is_default = true;*/
                        }
                        : button {
                            key     = "select";
                            label   = "Add";
                            mnemonic = "d";
                        }
                        spacer_1;
                    }
                }
                spacer_1;
                : boxed_radio_column
                {
                    label           = "Save to Layers:";
                    : radio_button
                    {
                        key     = "byobject";
                        label   = "By Object";
                        mnemonic = "O";
                    }
                    : radio_button
                    {
                        key     = "bymaterial";
                        label   = "By Material";
                        mnemonic = "M";
                    }
                    : radio_button
                    {
                        key     = "bycolor";
                        label   = "By Object Color";
                        mnemonic = "B";
                    }
                    : radio_button
                    {
                        key     = "onelayer";
                        label   = "Single Layer";
                        mnemonic = "L";
                    }
                }
            }
            spacer_1;
            : column
            {
                : boxed_column {       
                    label           = "Selected Objects";
                    : object_list {
                        key     = "selected";
                    }
                    : row
                    {
                        children_fixed_width    = true;
                
                        spacer_1;
                        : button
                        {
                            key     = "remove";
                            label   = "Remove";
                            mnemonic = "R";
                        }
                        : button
                        {
                            key     = "rmvall";
                            label   = "Remove All";
                            mnemonic = "v";
                        }
                        spacer_1;
                    }
                }
                spacer_1;
                : boxed_radio_column
                {
                    label           = "Multiple Material Objects:";
                    : radio_button
                    {
                        key     = "prompt";
                        label   = "Always Prompt";
                        mnemonic = "P";
                    }
                    : radio_button
                    {
                        key     = "break";
                        label   = "Split by Material";
                        mnemonic = "S";
                    }
                    : radio_button
                    {
                        key     = "first";
                        label   = "Assign First Material";
                        mnemonic = "F";
                    }
                    : radio_button
                    {
                        key     = "none";
                        label   = "Don't Assign a Material";
                        mnemonic = "n";
                    }
                }
            }
        }
        spacer_1;
        okNoDef_cancel_help;
    }
}


//***************************************************************************
//
//***************************************************************************
bcmatls : dialog
{
    label   = "Material Assignment Alert";
    
    : text {
        key     = "objname";
        width   = 50; /* "Object1234567890 has multiple materials assigned" */
    }
    : boxed_column {
        label                   = "";
        children_alignment      = centered;
        children_fixed_width    = true;

        : radio_column {
            : radio_button {
                key     = "breakapart";
                label   = "Split Object By Material";
                mnemonic = "S";
            }
            : radio_button {
                key     = "applyfirst";
                label   = "Assign First Material";
                mnemonic = "A";
            }
            : radio_button {
                key     = "applyone";
                label   = "Select a Material:";
                mnemonic = "M";
            }
        }
        : row {
            : popup_list {
                key     = "materials";
                value   = "0";
                edit_width  = 25;
            }
        }
    }
    spacer_1;
    ok_cancel_help;
}


//***************************************************************************
//
//***************************************************************************
// this is a clone of ok_cancel_help 
rename_cancel_replace : column 
{
    : row 
    {
        fixed_width = true;
        alignment = centered;
        : retirement_button 
        {
            label           = " Rename ";
            key             = "rename";
            is_default      = true;
        }
        : spacer { width = 2; }
        cancel_button;
        : spacer { width = 2; }
        : retirement_button 
        {
            label           = " Replace ";
            key             = "replace";
            mnemonic        = "R";
        }
    }
}


//***************************************************************************
//
//***************************************************************************
bcrenmat : dialog
{
    label   = "Mordred Message";

    : text 
    {
        key     = "prompt";
        width   = 40;
    }
    : text 
    {
        label = "Do you want to replace it?";
    }
    spacer_1;
    rename_cancel_replace;
}

//***************************************************************************
// 3DS Output
//***************************************************************************
outOptions : dialog
{
  label = "3D Studio File Export Options";

  :boxed_radio_column
  {
    label = "Derive 3D Studio Objects From";
    key = "method";
    :radio_button
    {
      label = "Layer";
      key = "methLay";
      mnemonic = "L";
    }
    :radio_button
    {
      label = "AutoCAD Color Index (ACI)";
      key = "methCol";
      mnemonic = "A";
    }
    :radio_button
    {
      label = "AutoCAD Object Type";
      key = "methType";
      mnemonic = "O";
    }
  }
  :boxed_row
  {
    label = "AutoCAD Blocks";
    :toggle
    {
      label = "Override (Each block is one object)";
      key = "mode";
      mnemonic = "v";
    }
  }
  :boxed_row
  {
    height = 2;
    label = "Smoothing";
    :toggle
    {
      label = "Auto-Smoothing";
      key = "sEn";
      mnemonic = "S";
    }
    :edit_box
    {
      label = " ";
      key = "smooth";
      edit_width = 3;
      edit_limit = 3;
      allow_accept = true;
    }
    :text
    {
      label = "Degrees";
      key = "sText";
    }
  }
  :boxed_row
  {
    height = 2;
    label = "Welding";
    :toggle
    {
      label = "Auto-Welding";
      key = "wEn";
      mnemonic = "W";
    }
    spacer_1;
    :edit_box
    {
      key = "weld";
      label = "&Threshold";
      edit_width = 8;
      edit_limit = 8;
      allow_accept = true;
    }
    :text
    {
      label = " ";
    }
  }
  spacer_1;
  errtile;
  ok_cancel_help;
}
//***************************************************************************
// VL conversion
//***************************************************************************
presOptions : dialog
{
  label = "Visual Link Data Conversion";
  initial_focus = "overwt";
  :toggle
  {
    key = "overwt";
    mnemonic = "";
    label = "Overwrite AutoVision Material and Mapping Assignments";
  }
  spacer_1;
  ok_cancel_help;
}

//***************************************************************************
// 
//***************************************************************************
warning : dialog
{
  label = "Visual Link Conversion Alert";
  :text
  {
    label = "Some Visual Link material assignments";
  }
  :text
  {
    label = "could not be represented by AutoVision";
  }
  :text
  {
    label = "and therefore were not converted";
  }
  spacer_1;
  ok_only;
}

//***************************************************************************
// RHEXPORT support
//***************************************************************************
rhexport : dialog
{
  label = "File Output Configuration";

  : row {
      : column {
          : boxed_column {
              label = "File Type";
              mnemonic = "F";
              : popup_list { 
                  key = "f_types";
                  // CHANGING THE ORDER OF THE LIST OR WHAT IS ON IT
                  // REQUIRES A CHANGE IN dlg_rhex.c !!!!!
                  list = "BMP\nPCX\nPostScript\nTGA\nTIFF";
                  mnemonic = "F";
                  value = "4";
              }
              : popup_list { 
                  key = "resol";
                  // CHANGING THE ORDER OF THE LIST OR WHAT IS ON IT
                  // REQUIRES A CHANGE IN dlg_rhex.c !!!!!
                  // Just changing the strings is OK.
                  width = 30;
list = "320 x 200 (CGA/MCGA color)\n640 x 200 (CGA Monochrome)\n640 x 350 (EGA)\n640 x 400\n640 x 480 (VGA)\n720 x 540\n800 x 600\n1024 x 768\n1152 x 900 (Sun Standard)\n1600 x 1280 (Sun hi-res)\n2048 x 1366\n2048 x 1536\n4096 x 2736\n4096 x 3072\nUser Defined";
                  mnemonic = "F";
                  value = "4";
              }
              : row {
                  : edit_box {
                      label = "X:";
                      key   = "RES_X";
                      value = "640";
                      mnemonic = "X";
                      edit_limit = 5;
                  }
                  : edit_box {
                      label = "Y:";
                      key   = "RES_Y";
                      value = "480";
                      mnemonic = "Y";
                      edit_limit = 5;
                  }
              }
              : edit_box {
                  label = "Aspect Ratio:";
                  key   = "ASPECT";
                  value = "1.0";
                  mnemonic = "R";
                  edit_limit = 5;
              }
          }
          : boxed_radio_column {
              label = "Colors";
              key = "colortype";
              : radio_button {
                  key = "b_1";
                  label = "Monochrome";
                  mnemonic = "M";
              }
              : radio_button {
                  key = "b_8";
                  label = "8 Bits (256 Grayscale)";
                  mnemonic = "G";
              }
              : radio_button {
                  key = "b_8c";
                  label = "8 Bits (256 Color)";
                  mnemonic = "C";
              }
              : radio_button {
                  key = "b_16";
                  label = "16 Bits";
                  mnemonic = "B";
              }
              : radio_button {
                  key = "b_24";
                  label = "24 Bits";
                  mnemonic = "i";
                  value = "1";
              }
              : radio_button {
                  key = "b_32";
                  label = "32 Bits";
                  mnemonic = "t";
              }
          }
      }
      : column {
          : boxed_column {
              label = "TGA Options";
              key   = "tga_opts";
              : column {
                  : row {
                      : toggle {
                          key = "compress";
                          label = "Compressed";
                          value = "1";
                          mnemonic = "d";
                      }
                      : toggle {
                          key = "up_down";
                          label = "Bottom Up";
                          mnemonic = "U";
                      }
                  }
                  : boxed_radio_row {
                      label = "Interlace";
                      key = "inter_a";
                      : radio_button {
                          key = "inter_1";
                          label = "None";
                          mnemonic = "N";
                      }
                      : radio_button {
                          key = "inter_2";
                          label = "2 to 1";
                          mnemonic = "2";
                      }
                      : radio_button {
                          key = "inter_4";
                          label = "4 to 1";
                          mnemonic = "4";
                      }
                  }
              }
          }
          : boxed_column {
              label = "PostScript Options";
              key   = "post_opts";
              : radio_row {
                  key = "ps_port_land";
                  : radio_button {
                      key = "ps_land";
                      label = "Landscape";
                      mnemonic = "L";
                  }
                  : radio_button {
                      key = "ps_port";
                      label = "Portrait";
                      mnemonic = "P";
                  }
              }
              : column {
                  : radio_column {
                      key = "ps_imgsize";
                      : radio_button {
                          key = "ps_auto";
                          label = "Auto";
                          mnemonic = "A";
                      }
                      : radio_button {
                          key = "ps_img";
                          label = "Image Size";
                          mnemonic = "S";
                      }
                      : radio_button {
                          key = "ps_cust";
                          label = "Custom";
                          mnemonic = "o";
                      }
                  }
                  : edit_box {
                      label = "Image Size";
                      key   = "img_size";
                      value = "640";
                      mnemonic = "z";
                      edit_limit = 5;
                  }
              }
          }
      }
  }

  ok_cancel_help_errtile;
}

//*****************************************************************************
//
other_options : boxed_column {
    label = "Face Controls";
    : toggle {
        key = "discard_bf";
        label = "Discard back faces";
        mnemonic = "D";
    }
    : toggle {
        key = "neg_normal";
        label = "Back face normal is negative";
        mnemonic = "N";
    }
}


//*****************************************************************************
//
crender_options : dialog {
    label = "Render Options";
    : row {
        : boxed_radio_column {
            label = "Render Quality";
            key = "pf_crend_qual";
            : radio_button {
                key = "pf_crend_qual_10";
                label = "Gouraud";
                mnemonic = "G";
            }
            : radio_button {
                key = "pf_crend_qual_20";
                label = "Phong";
                mnemonic = "P";
            }
        }
        spacer_1;
        other_options;
    }
    spacer_1_ok_cancel_help_errtile;
}


//*****************************************************************************
// Render and Preferences widgets

render_scene_list : list_box {
    width = 18;
    height = 9;
    key = "pf_scene";
    label = "Scene to Render";
    mnemonic = "S";
}
 

//***************************************************************************

components : row {
    : column {
        : color_name       { key = "top_name"; }
        : color_name       { key = "middle_name"; }
        : color_name       { key = "bottom_name"; }
    }
    : column {
        : edit_box_4       { key = "top_edit"; }
        : edit_box_4       { key = "middle_edit"; }
        : edit_box_4       { key = "bottom_edit"; }
    }
    : column {
        : slider_0_1_fixed { key = "top_slider"; }
        : slider_0_1_fixed { key = "middle_slider"; }
        : slider_0_1_fixed { key = "bottom_slider"; }
    }
    : column {
        : color_image      { key = "top_image"; }
        : color_image      { key = "middle_image"; }
        : color_image      { key = "bottom_image"; }
    }
}



color_name : text_part {
    width = 11;
    label = "";
}
 
color_image : image {
    height = 1;
    aspect_ratio = 1;
    fixed_height = true;
    fixed_width = true;
    color = -15;         /* -15 = dialog background color */
}
 
//***************************************************************************
// Replay

replay : dialog {
    label = "Image Specifications";
    width = 22;
    alignment = centered;
    spacer_1;
    : column {
        : row {
            : column {
                fixed_width = true;
                width = 20;
                : text_part {
                    alignment = centered;
                    key = "imgsz";
                    width = 16;
                }
                : image_button {              // Virtual image window
                    alignment = centered;
                    key = "vimg";
                    color = 0;
                    aspect_ratio = 1;
                    height = 6;
                    width = 16;
                    fixed_width = true;
                    fixed_height = true;
                    is_tab_stop = false;
                    image_flush = true;
                }
                spacer;
                : text_part {
                    alignment = centered;
                    label = " Image Offset";
                    width = 16;
                }
                : row {
                    : edit_box_4 { 
                        label = "X:";
                        key = "imgoffx"; 
                    }
                    : edit_box_4 { 
                        label = "Y:";
                        key = "imgoffy"; 
                    }
                }
            }
            spacer;
            : column {
                fixed_width = true;
                width = 20;
                : text_part {
                    alignment = centered;
                    key = "winsz";
                    width = 16;
                }
                : image_button {              // Virtual window window
                    alignment = centered;
                    key = "vwin";
                    color = 0;
                    aspect_ratio = 1;
                    height = 6;
                    width = 16;
                    fixed_width = true;
                    fixed_height = true;
                    is_tab_stop = false;
                }
                spacer;
                : text_part {
                    alignment = centered;
                    label = " Screen Offset";
                    width = 16;
                }
                : row {
                    : edit_box_4 { 
                        label = " X:";
                        key = "winoffx"; 
                    }
                    : edit_box_4 { 
                        label = "Y:";
                        key = "winoffy"; 
                    }
                }
             }
          }
          spacer_1;
          : row {
            : column {
                fixed_width = true;
                width = 20;
                : text_part {
                    alignment = centered;
                    label = " Image Size";
                    width = 16;
                }
                : row {
                    : edit_box_4 { 
                        label = "X:";
                        key = "imgsizex"; 
                    }
                    : edit_box_4 { 
                        label = "Y:";
                        key = "imgsizey"; 
                    }
                }
            }
            : column {
                fixed_width = true;
                width = 20;
                : text_part {
                    alignment = centered;
                    label = " Screen Size";
                    width = 16;
                }
                : row {
                    : concatenation {
                        : text_part { 
                            label = " X:  ";
                        }
                        : text_part { 
                            key = "winsizex"; 
                            width = 4; 
                        }
                    }
                    : concatenation {
                        : text_part { 
                            label = "Y:  ";
                        }
                        : text_part { 
                            key = "winsizey"; 
                            width = 4; 
                        }
                    }
                }
            }
        }
    }
    spacer_1;
    : button {
        key = "rst";
        mnemonic = "R";
        label = "Reset";
        fixed_width = true;
        alignment = centered;
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
//  saveimg dialog

saveimg : dialog {
    label = "Save Image";
    : row {
        : boxed_radio_column {
            label = "Format";
            key = "frmt";
            : radio_button {
                label = "BMP";
                key = "bmp";
                mnemonic = "B";
            }
            : radio_button {
                label = "TGA";
                key = "tga";
                mnemonic = "T";
            }
            : radio_button {
                label = "TIFF";
                key = "tif";
                mnemonic = "F";
            }
            : button {
                key = "opt";
                label = "Options...";
                mnemonic = "p";
            }
        }
        : boxed_row {
            label = "Portion";
            spacer;
            : column {
                : text_part {
                    label = "Active viewport";
                    fixed_width = true;
                    alignment = centered;
                }
                : image_button {              // Virtual window window
                    key = "vwin";
                    color = 0;
                    alignment = centered;
                    aspect_ratio = 1;
                    height = 6;
                    width = 16;
                    fixed_width = true;
                    fixed_height = true;
                    image_flush = true;
                }
                : button {
                    key = "rst";
                    label = "Reset";
                    mnemonic = "R";
                }
            }
            : column {
                : row {
                    : text_part {
                        label = "      Size : ";
                        mnemonic = "S";
                      }
                    : text_part {
                         key = "vportsz";
                         width = 12;
                    }
                }
                : row {
                    : text_part {
                        label = "      Offset ";
                        mnemonic = "O";
                      }
                    : text_part {
                        label = "      Size ";
                        mnemonic = "S";
                      }
                }
                : row {
                    fixed_width = true;
                    alignment = centered;
                    : edit_box_4 {
                        label = "X:";
                        key = "winoffx";
                    }
                    : edit_box_4 {
                        label = " X:";
                        key = "winsizex";
                    }
                }
                : row {
                    fixed_width = true;
                    alignment = centered;
                    : edit_box_4 {
                        label = "Y:";
                        key = "winoffy";
                    }
                    : edit_box_4 {
                        label = " Y:";
                        key = "winsizey";
                    }
                }
            }
        }
    }
    spacer_1_ok_cancel_help_errtile;
}


//***************************************************************************
//

tga_options : dialog {
    label = "TGA Options";
    spacer;
    : boxed_radio_row {
        label = "Compression";
        key = "comp";
        : radio_button {
            label = "None";
            mnemonic = "N";
            key = "none";       // returned as "value" of radio_cluster
        }
        : radio_button {
            label = "RLE";
            mnemonic = "R";
            key = "rle";
        }
    }
    spacer_1_ok_cancel_help;
}

//***************************************************************************
//

tif_options : dialog {
    label = "TIFF Options";
    spacer;
    : boxed_radio_row {
        label = "Compression";
        key = "comp";
        : radio_button {
            label = "None";
            mnemonic = "N";
            key = "none";       // returned as "value" of radio_cluster
        }
        : radio_button {
            label = "PACK";
            mnemonic = "P";
            key = "pack";
        }
    }
    spacer_1_ok_cancel_help;
}

//*****************************************************************************
//*****************************************************************************
// Lights Common widgets

distant_light_parameters_panel : column {
    fixed_height = true;
    : edit_box_8 {
        label = "Light Name:";
        key = "name";
        mnemonic = "N";
    }
    : row {
        : edit_box_8 {
            label = "Intensity: ";
            key = "inten_t";
            edit_width = 8;
            mnemonic = "I";
        }
        : slider {
            key = "inten_s";
            min_value = 0;
            max_value = 10000;
            small_increment = 100;
            big_increment = 1000;
        }
    }
    : boxed_column {
        label = "Color";
        rgb_edit_slider;
        light_color_panel;
    }
}

light_color_panel : row {
    : image_button {
        key = "color";
        color = -15;             /* background color */
        height = 1;
        width = 8;
    }
    spacer_1;
    : column {
        width = 14;
        : button {
            label = "Select Color...";
            key = "mod_color";
            mnemonic = "C";
            alignment = centered;
        }
        : button {
            label = "Select Indexed...";
            key = "select_aci";
            mnemonic = "x";
            alignment = centered;
        }
    }
}


azimuth_altitude_panel : boxed_row {
    : column {
        : edit_box {
            label = "Azimuth:";
            key = "azimuth_t";
            mnemonic = "u";
            edit_width = 6;
            edit_limit = 6;
        }
        : image_button {
            key = "azimuth_image";
            color = dialog_background;
            height = 7;
            aspect_ratio = 1;
            fixed_height = true;
            fixed_width = true;
            alignment = centered;
        }
        : slider  {
            key = "azimuth_slider";
            min_value = -180;
            max_value = 180;
            small_increment = 1;
            big_increment = 10;
        }
    }
    : column {
        : edit_box {
            label = "Altitude:";
            key = "altitude_t";
            mnemonic = "d";
            edit_width = 6;
            edit_limit = 6;
        }
        : image_button {
            key = "altitude_image";
            color = dialog_background;
            height = 7;
            aspect_ratio = 1;
            fixed_height = true;
            fixed_width = true;
            alignment = centered;
        }
        : slider  {
            key = "altitude_slider";
            min_value = 0;
            max_value = 90;
            small_increment = 1;
            big_increment = 10;
        }
    }
}



light_source_panel : column {
    : boxed_column {
        label = "Light Source Vector";
        : row {
            : edit_box_4 {
                label = "X:";
                key = "x";
                mnemonic = "X";
            }
            spacer;
            : edit_box_4 {
                label = "Y:";
                key = "y";
                mnemonic = "Y";
            }
            spacer;
            : edit_box_4 {
                label = "Z:";
                key = "z";
                mnemonic = "Z";
            }
        }
        : button {
            alignment = centered;
            fixed_width = true;
            key = "mod_points";
            label = "Modify <";
            mnemonic = "M";
        }
    }
}


//***************************************************************************
// Tile used by all three light types.
 
ave_basic_lights : column {
    : edit_box_8 {
        label = "Light Name:";
        key = "name";
        mnemonic = "N";
    }
    : edit_box_8 {
        label = "Intensity:";
        key = "inten_t";
        mnemonic = "I";
    }
    : slider  {
        key = "inten_s";
        min_value = 0;
        max_value = 10000;
        small_increment = 100;
        big_increment = 1000;
    }
    position_panel;
    : boxed_column {
        label = "Color";
        rgb_edit_slider;
        light_color_panel;
    }
}

position_panel : boxed_row {
    label = "Position";
    : button {
        label = "Modify <";
        key = "mod_points";
        mnemonic = "M";
    }
    : button {
        label = "Show...";
        key = "show_points";
        mnemonic = "S";
        other = "1";
    }
 }

attenuation_panel : boxed_radio_column {
    label = "Attenuation";
    key = "falloff";
    fixed_height = true;
    : radio_button {
        label = "None";
        mnemonic = "o";
        key = "0";      // returned as "value" of radio_cluster
    }
    : radio_button {
        label = "Inverse Linear";
        mnemonic = "L";
        key = "1";
    }
    : radio_button {
        label = "Inverse Square";
        mnemonic = "q";
        key = "2";
    }
}

//***************************************************************************
//  Display the Location and Target points of lights.  Target should be
//  disabled when displaying a Point Light.
 
show_points : dialog {
    label = "Show Light Position";
    : row {
        : boxed_row {
            key = "location";
            label = "Location";
            : paragraph {
                : text_part { label = "X ="; }
                : text_part { label = "Y ="; }
                : text_part { label = "Z ="; }
            }
            : paragraph {
                : text_part_12 { key = "lfx"; }
                : text_part_12 { key = "lfy"; }
                : text_part_12 { key = "lfz"; }
            }
        }
        : boxed_row    {
            key = "target";     //  Disabled for Point Lights
            label = "Target";
            : paragraph {
                : text_part { label = "X ="; }
                : text_part { label = "Y ="; }
                : text_part { label = "Z ="; }
            }
            : paragraph {
                : text_part_12 { key = "lax"; }
                : text_part_12 { key = "lay"; }
                : text_part_12 { key = "laz"; }
            }
        }
    }
    spacer_1_ok_help;
}

//***************************************************************************
// Main scene dialog
 
ave_scene : dialog {
    label = "Scenes";
    : row {
        : list_box_8x8 {
            key = "scen";
            label = "Scenes:";
            mnemonic = "S";
        }
        : column {
            spacer_0;
            button_new;
            button_mod;
            button_del;
            spacer_0;
        }
    }
    spacer_1_ok_cancel_help;
}
 
//***************************************************************************
// Dialog to modify a scene.
 
ave_scene_mod : dialog {
    key = "dialog";
    : edit_box_8 {
        label = "Scene Name:";
        key = "name";
        mnemonic = "N";
    }
    spacer;
    : row {
        : column {
            : text {
                label = "Views";
                mnemonic = "V";
            }
            : list_box_8x8 {
                key = "view";
            }
        }
        : column {
            : text {
                label = "Lights";
                mnemonic = "L";
            }
            : list_box_8x8 {
                key = "lght";
                multiple_select = true;
            }
        }
    }
    spacer_1_ok_cancel_help_errtile;
}



//***************************************************************************
//Materials list dialog -- common components
material_column : boxed_column {
    label = "Current Drawing";
    mnemonic = "C";
    : list_box {
        height = 15;
        width = 24;
        key = "materials_list";
        multiple_select = true;
    }
    : row {
        : button {
            key = "cleanup";
            label = "Purge";
            mnemonic = "u";
        }
        : button {
            key = "saveAsList";
            label = "Save As...";
            mnemonic = "S";
        }
    }
}

import_export_delete : column {
    : button {
        key = "import";
        label = "<-Import";
        mnemonic = "I";
        is_enabled = false;
    }
    : button {
        key = "export";
        label = "Export->";
        mnemonic = "E";
        is_enabled = false;
    }
    : button {
        key = "delete";
        label = "Delete";
        mnemonic = "D";
        is_enabled = false;
    }
}

library_column : boxed_column {
    label = "Current Library";
    mnemonic = "L";
    : popup_list {
        key = "libraryTitle";
    }
    spacer_1;
    : list_box {
        height = 12;
        width = 24;
        key = "library_list";
        multiple_select = true;
    }
    : row {
        : button {
            key = "openLibrary";
            label = "Open...";
            mnemonic = "O";
        }
        : button {
            key = "saveLibrary";
            label = "Save";
            mnemonic = "a";
        }
    }
    : button {
        key = "saveAsLibrary";
        label = "Save As...";
        mnemonic = "v";
    }
}

spacer_1_ok_cancel_help_custom : column {
    spacer_1;
    ok_cancel_help_custom;
}
 
ok_cancel_help_custom : column {
    :column {
        : row {
            fixed_width = true;
            alignment = centered;
            ok_button;
            : spacer { width = 2; }
            cancel_button_custom;
            : spacer { width = 2; }
            help_button;
        }
        errtile;
    }
}

cancel_button_custom : retirement_button {
        label           = "Cancel";
        key             = "cancel";
        is_cancel       = true;            // reason for custom
}

ave_confirm_library_save : dialog {
    label = "Library Modification";
    initial_focus = "save";
    : column {
        : text {
            label = "The current materials library has been changed.";
            alignment = centered;
        }
        : row {
            : button {
                label = "Save Changes...";
                is_default = true;
                mnemonic = "S";
                key = "save";
            }
            : button {
                label = "Discard Changes";
                mnemonic = "D";
                key = "discard";
            }
            : button {
                label = "Cancel Command";
                is_cancel = true;
                mnemonic = "C";
                key = "cancel";
            }
        }
    }
}

//***************************************************************************
//Reconcile Imported Material Names dialog
 
ave_reconcile_import : dialog {
    label = "Reconcile Imported Material Names";
    initial_focus = "new_name";
    spacer_1;
    : row {
        children_alignment = top;
        : column {
            : boxed_column {
                fixed_height = true;
                label = "Options";
                key = "options";
                dialog_kind = "import";
                : toggle {
                    label = "Overwrite Existing Material";
                    value = "1";
                    key = "overwrite";
                    mnemonic = "O";
                }
                : toggle {
                    label = "Transfer Attachments";
                    key = "transfer";
                    value = "1";
                    mnemonic = "T";
                }
            }
            : boxed_column {
                fixed_height = true;
                label = "Material Names";
                key = "names";
                : edit_box_16 {
                    label = "Old Material in List:";
                    key = "old_name";
                    mnemonic = "l";
                }
                : edit_box_16 {
                    label = "New Material from Library:";
                    key = "new_name";
                    mnemonic = "N";
                }
            }
            spacer_0;
        }
    }
    spacer_1_ok_ok_all_cancel_help;
}
//***************************************************************************
//Reconcile Exported Material Names dialog
 
ave_reconcile_export : dialog {
    label = "Reconcile Exported Material Names";
    initial_focus = "new_name";
    spacer_1;
    : row {
        children_alignment = top;
        : column {
            : boxed_column {
                label = "Options";
                key = "options";
                dialog_kind = "export";
                fixed_height = true;
                : toggle {
                    label = "Overwrite Existing Material";
                    value = "1";
                    key = "overwrite";
                    mnemonic = "O";
                }
            }
            : boxed_column {
                label = "Material Names";
                key = "names";
                fixed_height = true;
                : edit_box_16 {
                    label = "Old Material in Library:";
                    key = "old_name";
                    mnemonic = "l";
                }
                : edit_box_16 {
                    label = "New Material from List:";
                    key = "new_name";
                    mnemonic = "N";
                }
            }
            spacer_0;
        }
    }
    spacer_1_ok_ok_all_cancel_help;
}
 
spacer_1_ok_ok_all_cancel_help : column {
    spacer_1;
    ok_ok_all_cancel_help;
}
 
ok_ok_all_cancel_help : column {
    : row {
        fixed_width = true;
        alignment = centered;
        ok_button;
        ok_all_button;
        : spacer { width = 2; }
        cancel_button;
        : spacer { width = 2; }
        help_button;
    }
}
 
ok_all_button : retirement_button {
        label           = "OK to All";
        key             = "okAll";
        mnemonic        = "A";
}


//***************************************************************************
// Main material dialog -- common components

material_list : list_box {
    key = "list";
    label = "Materials:";
    mnemonic = "t";
    width = 18;
}

material_attach : column {
    : button {
        label = "Attach <";
        key = "entities";
        mnemonic = "A";
    }
    : button {
        label = "Detach < ";
        key = "detach";
        mnemonic = "D";
    }
    : button {
        label = "By ACI... ";
        key = "ACI";
        mnemonic = "B";
    }
    : button {
        label = "By Layer... ";
        key = "layer";
        mnemonic = "y";
    }
}

material_name : row {
    : edit_box_16 {
        label = "Material Name:";
        key = "name";
        mnemonic = "M";
        fixed_width = true;
    }
}

value_edit_slider : boxed_row {
    : edit_box_4 {
        label = "Value:";
        key = "value_edit";
        mnemonic = "V";
    }
    : slider_0_1_fixed { key = "value_slider"; }
}


//*****************************************************************************
//*****************************************************************************

//**RGB edit sliders
rgb_edit_slider : row {
    : column {
        : edit_box_4 {
            label = "Red:";
            key = "red_edit";
            mnemonic = "R";
        }
        : edit_box_4 {
            label = "Green:";
            key = "green_edit";
            mnemonic = "G";
        }
        : edit_box_4 {
            label = "Blue:";
            key = "blue_edit";
            mnemonic = "B";
        }
    }
    : column {
        : slider_0_1_fixed { key = "red_slider"; }
        : slider_0_1_fixed { key = "green_slider"; }
        : slider_0_1_fixed { key = "blue_slider"; }
    }
}
 
lock : toggle {
    label = "Lock";
    key = "lock";
    mnemonic = "k";
}
 
 
color_system : popup_list {
    label = "Color System:";
    mnemonic = "C";
    key = "color_system";
    list = "RGB\nHLS";
    width = 5;
}
 
 
 
//***************************************************************************
// Finish Preview tile.
 
finish_preview : boxed_column {
    children_alignment = centered;
    : image_button {
        key = "image";
        color = graphics_background;
        height = 8;
        aspect_ratio = 1;
        fixed_height = true;
        fixed_width = true;
        mnemonic = "P";     //  FIXME - Can we ALT to an image???
        is_tab_stop = false;
        image_flush = true;
    }
    : button {
        key = "object";
        label = "Preview";
        mnemonic = "P";
    }
    spacer_0;
}
