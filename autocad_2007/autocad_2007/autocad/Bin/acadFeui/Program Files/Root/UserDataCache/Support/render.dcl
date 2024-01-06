// Next available MSG number is   485 
// MODULE_ID RENDER_DCL_

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
// Render Dialogue Control Language (DCL)
//
//***************************************************************************

// Change level to 3 for new DCL auditing.
dcl_settings : default_dcl_settings { }

@include "rendcomm.dcl"

//***************************************************************************
//

// Sub-assemblies common to the render and preferences dialog

render_quality : popup_list {
    label = "Rendering Type:";
    key = "pf_st";
    mnemonic = "R";
    list = "Render\nPhoto Real\nPhoto Raytrace";
    render_types = "crender\nautovis\nraytrace";
    edit_width = 20;
    fixed_width = true;
}

render_query : toggle {
    key = "pf_rp";
    label = "Query for Selections";
    mnemonic = "Q";
}

render_crop : toggle {
    key = "pf_cropwin";
    label = "Crop Window";
    mnemonic = "W";
}

render_procedure : column {
    : boxed_column {
        label = "Rendering Procedure";
        render_query;
        render_crop;
        : toggle {
            key =  "pf_sd";
            //  FIXME
            label = "Skip Render Dialog";
            mnemonic = "k";
        }
    }
    : edit_box_4 {
        label = "Light Icon Scale:";
        key = "pf_ic_tx";
        mnemonic = "L";
    }
    : edit_box_4 {
        label = "Smoothing Angle:";
        key = "pf_sa";
        mnemonic = "g";
    }
}

render_options : boxed_column {
    label = "Rendering Options";
    : toggle {
        key = "pf_ss";
        label = "Smooth Shade";
        mnemonic = "m";
    }
    : toggle {
        key = "pf_af";
        label = "Apply Materials";
        mnemonic = "A";
    }
    : toggle {
        key = "pf_sh";
        label = "Shadows";
        mnemonic = "d";
    }
    : toggle {
        key = "pf_ca";
        label = "Render Cache";
        mnemonic = "C";
    }
    : button {
        key = "Options";
        label = "More Options...";
        mnemonic = "O";
    }
}

sub_sample : boxed_column {
    label = "Sub Sampling";
    mnemonic = "u";
    : popup_list {
        key = "pf_subs";
        mnemonic = "u";
        list = "1:1 (Best)\n2:1\n3:1\n4:1\n5:1\n6:1\n7:1\n8:1 (Fastest)";
        subsample_types = "1\n2\n3\n4\n5\n6\n7\n8";
        edit_width = 15;
    }
}

render_env : column {
    sub_sample;
    : button {
        key = "pf_env_back";
        label = "Background...";
        mnemonic = "B";
    }
    : button {
        key = "pf_env_fog";
        label = "Fog/Depth Cue...";
        mnemonic = "F";
    }
}

render_destination : boxed_column {
    label = "Destination";
    mnemonic = "n";
    : popup_list {
        key = "pf_ds";
        list = "Viewport\nRender Window\nFile";
        mnemonic = "n";

    }
    spacer;
    : column {
        : var_text {
            key = "pf_width";
            alignment = left;
            label = "   Width   :  640";
        }
        : var_text {
            key = "pf_height";
            alignment = left;
            label = "   Height  :  480";
        }
        : var_text {
            key = "pf_colors";
            alignment = left;
            label = "   Colors    :  8-bit";
        }
    }
    spacer;
    : button {
        key = "FOptions";
        label = "More Options...";
        mnemonic = "p";
    }
}

pref : dialog {
    label = "Rendering Preferences";
    render_quality;
    spacer_1;
    : row {
        render_scene_list;
        render_procedure;
    }
    spacer_1;
    : row {
        render_options;
        render_destination;
        render_env;
    }
    spacer_1_ok_cancel_help_errtile;
}


render : dialog {
    label = "Render";
    render_quality;
    spacer_1;
    : row {
        render_scene_list;
        render_procedure;
    }
    spacer_1;
    : row {
        render_options;
        render_destination;
        render_env;
    }
    spacer_1;
    : row {
        fixed_width = true;
        alignment = centered;
        : button {
            key = "pf_re_scene";
            label = "Render";
            is_default = true;
        }
        : spacer { width = 2; }
        cancel_button;
        : spacer { width = 2; }
        help_button;
    }
    errtile;
}

//**************************************************************************
// Rendering Options

rp_alias_mode : boxed_radio_column {
    label = "Anti-Aliasing";
    key = "rpaliasmode";
    fixed_height = true;
    : radio_button {
        key = "ahnv";
        label = "Minimal";
        mnemonic = "i";
    }
    : radio_button {
        key = "ahsv";
        label = "L&ow";
    }
    : radio_button {
        key = "ahsv3";
        label = "M&edium";
    }
    : radio_button {
        key = "ahsv4";
        label = "Hi&gh";
    }
}

rp_adaptive_toggle : toggle {
    label = "Enable";
    key = "adaptive_enable";
    mnemonic = "E";
}

rp_ray_adaptive : boxed_column {
    key = "adapt_samp_box";
    label = "Adaptive Sampling";
    fixed_height = true;
    rp_adaptive_toggle;
    : edit_box_6 {
        label = "Contrast Threshold:";
        key = "contrast_threshold";
        mnemonic = "C";
    }

}


rp_raydepth_mode : boxed_column {
    label = "Ray Tree Depth";
    fixed_height = true;
    : edit_box_4 {
        label = "Maximum Depth:";
        key = "ray_depth";
        mnemonic = "D";
    }
    : edit_box_6 {
        label = "Cutoff Threshold:";
        key = "ray_threshold";
        mnemonic = "T";
    }
}


rp_shadow_mode : boxed_column {
    label = "Depth Map Shadow Controls";
    key = "rpshadowmode";
    : edit_box_8  {
        key = "minbias";
        label = "Minimum Bias: ";
        mnemonic = "B";
    }
    : edit_box_8  {
        key = "maxbias";
        label = "Maximum Bias: ";
        mnemonic = "x";
    }
}

rp_texmap_mode : boxed_radio_column {
    label = "Texture Map Sampling";
    key = "rptexmapmode";
    : radio_button  {
        key = "txpoint";
        label = "Point Sample";
        mnemonic = "P";
    }
    : radio_button  {
        key = "txlinterp";
        label = "Linear Sample";
        mnemonic = "L";
    }
    : radio_button  {
        key = "txmip";
        label = "Mip Map Sample";
        mnemonic = "M";
    }
}

scanline_options : dialog {
    label = "Photo Real Render Options";
    spacer_1;
    : row {
        : column {
            fixed_height = true;
            alignment = top;
            rp_alias_mode;
        }
        spacer_1;
        : column {
            fixed_height = true;
            alignment = top;
            other_options;
            spacer;
            rp_shadow_mode;
            spacer;
            spacer;
            rp_texmap_mode;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

raytrace_options : dialog {
    label = "Photo Raytrace Render Options";
    spacer_1;
    : row {
        : column {
            rp_alias_mode;
            rp_ray_adaptive;
            rp_raydepth_mode;
        }
        spacer_1;
        : column {
            other_options;
            spacer;
            rp_shadow_mode;
            spacer;
            spacer;
            rp_texmap_mode;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}


//**************************************************************************
// Statistics dialog box

avis_stats : dialog {
    label = "Statistics";
    width = 50;
    : list_box { 
        key = "stats_list";
        height = 16;
        tabs = "24";
    }
    : row {
        alignment = centered;
        fixed_width = true;
        width = 39;
        : toggle {
            label = "Save Statistics to File:";
            mnemonic = "S";
            key = "save_stats";
            height = 1;
        }
        : edit_box {
            label = "";
            key = "stats_name";
            edit_width = 14;
            edit_limit = 132;
        }
        : button {
            label = "Find File...";
            mnemonic = "F";
            key = "findfile";
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Main light dialog.

avis_light : dialog {                       // identical to ave_light, plus North Location button
    label = "Lights";
    key = "dialog";
    dialog_kind = "main";
    : row {
        : column {
            : row {
                : list_box_8x8 {
                    key = "list";
                    label = "Lights:";
                    mnemonic = "L";
                }
                : column {
                    spacer_0;
                    button_mod;
                    button_del;
                    button_pkt;
                    spacer_0;
                }
            }
            : row {
                button_new;
                : popup_list { 
                    key = "light_type_popup";
                    edit_width = 15;
                    mnemonic = "T";
                    list = "Point Light\nDistant Light\nSpotlight";
                    light_types = "overhead\ndirect\nsh_spot";
                }
            }
            : button {
                key = "northloc";
                label = "North Location...";
                mnemonic = "o";
                fixed_width = true;
                alignment = centered;
            }
        }
        spacer;
        : column {
            children_alignment = centered;
            : boxed_column {
                label = "Ambient Light";
                : edit_box_4 {
                    label = "Intensity:";
                    key = "ambient_t";
                    mnemonic = "I";
                }
                : slider_0_1 { key = "ambient_s"; }
                : boxed_column {
                    label = "Color";
                    rgb_edit_slider;
                    light_color_panel;
                }
            }
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Dialog to edit the currently selected Point light definition.

avis_point_light : dialog {
    key = "dialog";
    dialog_kind = "point";
    : row {
        ave_basic_lights;
        : column {
            alignment = top;
            fixed_height = true;
            attenuation_panel;
            shadow_panel;
            spacer_0;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Dialog to edit the currently selected Spot light definition.

avis_spotlight : dialog {
    key = "dialog";
    dialog_kind = "spot";
    : row {
        ave_basic_lights;
        : column {
            alignment = top;
            fixed_height = true;
            ave_spot_lights;
            attenuation_panel;
            shadow_panel;
            spacer_0;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Dialog to edit distant lights (sun).

avis_distant_light : dialog {
    key = "dialog";
    dialog_kind = "distant";
    : row {
        : column {
            distant_light_parameters_panel;
            shadow_panel;
            : button {
                key = "sa_calc";
                label = "Sun Angle Calculator...";
                mnemonic = "S";
                fixed_width = true;
                alignment = centered;
            }
        }
        : column {
            azimuth_altitude_panel;
            light_source_panel;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

sun_angle_calculator_panel : column {
    : boxed_column {
        : row {
        : column {
                : edit_box {
                    label = "Date:      ";
                    key = "date_edit";
                    mnemonic = "D";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : edit_box {
                    label = "Clock Time:";
                    key = "time_edit";
                    mnemonic = "C";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : popup_list { 
                    key = "timezone_popup";
                    list = "PST:8\nMST:7\nCST:6\nEST:5\nNewfoundland:4\nYukon:9";
                }
                : edit_box {
                    label = "Latitude:  ";
                    key = "latitude_edit";
                    mnemonic = "L";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : edit_box {
                    label = "Longitude: ";
                    key = "longitude_edit";
                    mnemonic = "o";
                    edit_width = 6;
                    edit_limit = 6;
                }
        }
        : column {
                : slider {
                    key = "date_slider";
                    min_value = 1;
                    max_value = 365;
                    small_increment = 1;
                    big_increment = 31;
                    width = 10;
                }
                : slider {
                    key = "time_slider";
                    min_value = 0;
                    max_value = 1440;
                    small_increment = 10;
                    big_increment = 60;
                    width = 10;
                }
                : toggle {
                    label = "Daylight Savings";
                    key = "daylight_toggle";
                    mnemonic = "S";
                }
                : slider {
                    key = "latitude_slider";
                    min_value = 0;
                    max_value = 90;
                    small_increment = 1;
                    big_increment = 10;
                    width = 10;
                }
                : slider {
                    key = "longitude_slider";
                    min_value = 0;
                    max_value = 180;
                    small_increment = 1;
                    big_increment = 10;
                    width = 10;
                }
        }
        }
        : row {
            : popup_list { 
                key = "latitude_popup";
                list = "North\nSouth";
            }
            : popup_list { 
                key = "longitude_popup";
                list = "West\nEast";
            }
        }
        : button {
            alignment = centered;
            fixed_width = true;
            key = "location_button";
            label = "Geographic Location...";
            mnemonic = "G";
        }
    }
}
        

azimuth_altitude_panel_write_only : boxed_column {
    : row {
        : column {
            : row {
                : text {
                    label = "Azimuth:";
                }
                : text {
                    label = "      ";
                    key = "azimuth_t";
                    width = 6;
                }
            }
            : image_button {
                key = "azimuth_image";
                color = dialog_background;
                height = 7;
                aspect_ratio = 1;
                fixed_height = true;
                fixed_width = true;
                alignment = centered;
                is_enabled = false;
            }
        }
        : column {
            : row {
                : text {
                    label = "Altitude:";
                }
                : text {
                    label = "      ";
                    key = "altitude_t";
                    width = 6;
                }
            }
            : image_button {
                key = "altitude_image";
                color = dialog_background;
                height = 7;
                aspect_ratio = 1;
                fixed_height = true;
                fixed_width = true;
                alignment = centered;
                is_enabled = false;
            }
        }
    }
    : row {
        : text {
            label = "Solar Time:";
        }
        : text {
            label = "     ";
            key = "solar_time";
        }
    }
}

ave_sun_angle_calc : dialog {
    label = "Sun Angle Calculator";
    initial_focus = "accept";
    : row {
        sun_angle_calculator_panel;
        azimuth_altitude_panel_write_only;
    }
    spacer_1_ok_cancel_help_errtile;
}

shadow_panel : boxed_column {
    label = "Shadows:";
    : toggle {
        alignment = left;
        label = "Shadow On";
        key = "shadow_on";
        mnemonic = "w";
    }
    : button {
        key = "shadow_dialog";
        label = "Shadow Options...";
        mnemonic = "p";
        fixed_width = true;
    }
}


//***************************************************************************
// Tile used only by Spotlights.

ave_spot_lights : column {
    fixed_height = true;
    : edit_box_8 {
        label = "Hotspot:";
        key = "conea_t";
        mnemonic = "t";
    }
    : slider {
        key = "conea_s";
        min_value = 0;
        max_value = 160;
        small_increment = 1;
        big_increment = 10;
    }
    : edit_box_8 {
        label = "Falloff:";
        key = "coned_t";
        mnemonic = "F";
    }
    : slider {
        key = "coned_s";
        min_value = 0;
        max_value = 160;
        small_increment = 1;
        big_increment = 10;
    }
}

//***************************************************************************
// geographic location dialog

ave_geographic_location : dialog {
    key = "dialog";
    initial_focus = "accept";
    label = "Geographic Location";
    : column { 
        key = "top_key";
        : row {
            : column {
                fixed_width = true;
                : list_box {
                    key = "cities";
                    height = 8;
                    width = 21;
                    label = "City:";
                    mnemonic = "C";
                }
                : boxed_column {
                    : text {
                        label = " ";
                        key = "city";
                        width = 21;
                    }
                    : edit_box_8 {
                        label = "Latitude: ";
                        value = "0.";
                        edit_width = 8;
                        key = "latitude";
                        mnemonic = "a";
                    }
                    : edit_box_8 {
                        label = "Longitude: ";
                        value = "0.";
                        edit_width = 8;
                        key = "longitude";
                        mnemonic = "o";
                    }
                    spacer_0;
                }
            }
            : boxed_column {
                : row {
                    : popup_list { 
                        key = "map_name";
                        list = 
"North America\nCanada\nEurope\nSouth America\nAsia\nAustralia\nAsian Subcontinent\nAfrica";
                        files = 
"namer.map\ncanada.map\neurope.map\nsamer.map\nasia.map\naust.map\nindia.map\nafrica.map";
                        width = 18;
                    }
                    : toggle {
                        label = "Nearest Big City";
                        value = "1";
                        key = "nearest";
                        mnemonic = "N";
                    }
                }
                : image_button {
                    key = "map_image";
                    color = -15;             /* background color */
                    height = 15;
                    aspect_ratio = 1.5;
                }
            }
        }
//      spacer_1_ok_cancel_help_errtile;
        ok_cancel_help_errtile;
    }
}

northLocator : dialog {
    label = "North Location";
    spacer;
    spacer;
    : row {
        : boxed_column {
            label = "X/Y Plane:";
            fixed_width = true;
            : image_button {
                key = "northImage";
                color = dialog_background;             /* background color */
                width = 20;
                aspect_ratio = 1;
//              is_enabled = true;
                fixed_width = true;
            }
            : column {
                : edit_box {
                    label = "Angle:";
                    key = "northEdit";
                    mnemonic = "A";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : slider {
                    key = "northSlider";
                    min_value = 0;
                    max_value = 360;
                    small_increment = 1;
                    big_increment = 10;
                    width = 20;
                }
            }
        }
        : list_box {
            width = 20;
            fixed_width = true;
            key = "useUCS";
            label = "Use UCS:";
            mnemonic = "U";
//          list = "Shadow Softness:";
        }
    }
    ok_cancel_help_errtile;
}

shadowMapOptions : dialog {
    label = "Shadow Options";
    spacer;
    spacer;
    : toggle {
        label = "Shadow Volumes/Ray Traced Shadows";
        key = "shadowVolumes";
        mnemonic = "S";
    }
    : popup_list { 
        key = "shadowMapSize";
        fixed_width = true;
        label = "Shadow Map Size";
        list = "64\n128\n256\n512\n1024\n2048\n4096";
    mnemonic = "M";
    }
    : row {
        : edit_box {
            label = "Shadow Softness:";
            edit_width = 6;
            edit_limit = 6;
            key = "beamd_t";
            mnemonic = "o";
        }
        : slider {
            key = "beamd_s";
            min_value = 1;
            max_value = 10;
            small_increment = 1;
            big_increment = 2;
            width = 20;
        }
    }
    : button {
        key = "pick";
        label = "Shadow Bounding Objects <";
        alignment = centered;
        fixed_width = true;
        width = 3;
        mnemonic = "B";
    }
    spacer;
    spacer;
    ok_cancel_help_errtile;
}

ave_make_map : dialog {
    key = "dialog";
    label = "Make Map";
    : row {
        : column {
            : edit_box_8 {
                label = "Longitude: ";
                value = "-99.";
                key = "longitude";
                mnemonic = "X";
            }
            : edit_box_8 {
                label = "Latitude: ";
                value = "38.";
                key = "latitude";
                mnemonic = "Y";
            }
            : edit_box_8 {
                label = "Width (deg): ";
                value = "70.";
                key = "width";
                mnemonic = "S";
            }
            : edit_box_8 {
                label = "Rank: ";
                value = "1";
                key = "rank";
                mnemonic = "R";
            }
            : edit_box_8 {
                label = "File: ";
                value = "bdy.cbd";
                key = "file";
                mnemonic = "F";
            }
            : edit_box {
                label = "Path: ";
                value = "/files1/map/namer.map/";
//              value = "e:\\map\\namer.map\\";
                edit_width = 16;
                edit_limit = 30;
                key = "path";
                mnemonic = "t";
            }
            : edit_box {
                label = "Output: ";
                value = "namer.map";
                edit_width = 12;
                edit_limit = 12;
                key = "save";
                mnemonic = "O";
            }
            : row {
                : button {
                    key = "plot_button";
                    label = "Plot";
                    mnemonic = "P";
                    fixed_width = true;
                }
                : button {
                    key = "header_button";
                    label = "Header";
                    mnemonic = "H";
                    fixed_width = true;
                }
                : button {
                    key = "save_button";
                    label = "Save";
                    mnemonic = "S";
                    fixed_width = true;
                }
                : button {
                    key = "erase_button";
                    label = "Erase";
                    mnemonic = "E";
                    fixed_width = true;
                }
            }
        }
        : boxed_column {
            : image_button {
                key = "map_image";
                color = -15;             /* background color */
                height = 13;
                aspect_ratio = 1.5;
            }
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
//Materials list dialog

avis_list : dialog {
    label = "Materials Library";
    : column { 
        key = "top_key";
        spacer_1;
        : row {
            material_column;
            : column {
                avis_mat_preview;
                spacer_0;
                import_export_delete;
                spacer_0;
            }
            library_column;
        }
        spacer_0_5;
        ok_cancel_help_custom;
    }
}


//***************************************************************************
// Main material dialog


ave_material : dialog {
  label = "Materials";
  : column { 
    key = "top_key";
    : row {
        material_list;
        avis_material_preview_and_select;
        : column {
            width = 18;
            spacer_0;
            button_mod;
            button_dup;
            : boxed_column {
                button_new;
                : popup_list { 
                    key = "template";
                    list = "Standard\nGranite\nMarble\nWood";
                }
            }
            spacer_1;
            material_attach;
        }
    }
    spacer_1_ok_cancel_help_errtile;
  }
}


avis_material_preview_and_select : column {
    avis_mat_preview;
    spacer_0_5;
    button_imp;
    button_pkt;
    spacer_0_5;
}


//
// Dialogs to edit a minimal, standard, granite, marble, or wood material.
//

ave_standard_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        standard_attributes;
        standard_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


ave_granite_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        granite_attributes;
        template_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


ave_marble_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        marble_attributes;
        template_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


ave_wood_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        wood_attributes;
        template_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


standard_attributes : boxed_column {
    label = "Attributes";
    : row {
        : radio_column {
            key = "attribute";
            alignment = top;
            : radio_button {
                label = "Color/Pattern";
                key = "diffuse";
                mnemonic = "C";
            }
            : radio_button {
                label = "Ambient";
                key = "ambient";
                mnemonic = "A";
            }
            : radio_button {
                label = "Reflection";
                key = "reflection";
                mnemonic = "e";
            }
            : radio_button {
                label = "Roughness";
                key = "roughness";
                mnemonic = "o";
            }
            : radio_button {
                label = "Transparency";
                key = "transparency";
                mnemonic = "T";
            }
            : radio_button {
                label = "Refraction";
                key = "refraction";
                mnemonic = "n";
            }
            : radio_button {
                label = "Bump Map";
                key = "bump";
                mnemonic = "u";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


granite_attributes : boxed_column {
    label = "Attributes";
    : row {
        : radio_column {
            alignment = top;
            key = "attribute";
            : radio_button {
                label = "First Color";
                key = "first_color";
                mnemonic = "C";
            }
            : radio_button {
                label = "Second Color";
                key = "second_color";
                mnemonic = "n";
            }
            : radio_button {
                label = "Third Color";
                key = "third_color";
                mnemonic = "T";
            }
            : radio_button {
                label = "Fourth Color";
                key = "fourth_color";
                mnemonic = "F";
            }
            : radio_button {
                label = "Reflection";
                key = "reflection";
                mnemonic = "e";
            }
            : radio_button {
                label = "Roughness";
                key = "roughness";
                mnemonic = "o";
            }
            : radio_button {
                label = "Sharpness";
                key = "sharpness";
                mnemonic = "r";
            }
            : radio_button {
                label = "Scale";
                key = "scale";
                mnemonic = "S";
            }
            : radio_button {
                label = "Bump Map";
                key = "bump";
                mnemonic = "u";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            : avis_sample_image { key = "sample_3"; }
            : avis_sample_image { key = "sample_4"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


marble_attributes : boxed_column {
    label = "Attributes";
    : row {
        : radio_column {
            alignment = top;
            key = "attribute";
            : radio_button {
                label = "Stone Color";
                key = "stone_color";
                mnemonic = "C";
            }
            : radio_button {
                label = "Vein Color";
                key = "vein_color";
                mnemonic = "n";
            }
            : radio_button {
                label = "Reflection";
                key = "reflection";
                mnemonic = "e";
            }
            : radio_button {
                label = "Roughness";
                key = "roughness";
                mnemonic = "o";
            }
            : radio_button {
                label = "Turbulence";
                key = "turbulence";
                mnemonic = "T";
            }
            : radio_button {
                label = "Sharpness";
                key = "sharpness";
                mnemonic = "r";
            }
            : radio_button {
                label = "Scale";
                key = "scale";
                mnemonic = "S";
            }
            : radio_button {
                label = "Bump Map";
                key = "bump";
                mnemonic = "u";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


wood_attributes : boxed_column {
    label = "Attributes";
    : row {
        : radio_column {
            alignment = top;
            key = "attribute";
            : radio_button {
                label = "Light Color";
                key = "light_color";
                mnemonic = "C";
            }
            : radio_button {
                label = "Dark Color";
                key = "dark_color";
                mnemonic = "k";
            }
            : radio_button {
                label = "Reflection";
                key = "reflection";
                mnemonic = "e";
            }
            : radio_button {
                label = "Roughness";
                key = "roughness";
                mnemonic = "o";
            }
            : radio_button {
                label = "Light/Dark";
                key = "light_dark_ratio";
                mnemonic = "t";
            }
            : radio_button {
                label = "Ring Density";
                key = "ring_density";
                mnemonic = "n";
            }
            : radio_button {
                label = "Ring Width";
                key = "ring_width";
                mnemonic = "h";
            }
            : radio_button {
                label = "Ring Shape";
                key = "ring_shape";
                mnemonic = "a";
            }
            : radio_button {
                label = "Scale";
                key = "scale";
                mnemonic = "S";
            }
            : radio_button {
                label = "Bump Map";
                key = "bump";
                mnemonic = "u";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


avis_sample_image : image {
    color = -15;
    height = 1.25;
    aspect_ratio = 1;
    fixed_height = true;
    fixed_width = true;
    is_tab_stop = false;
}


standard_value_color_bitmap_preview : column {
    : row {
        : column {
            value_edit_slider;
            standard_color_widget;
        }
        avis_mat_preview;
    }
    spacer_0_1;
    bitmap_widget;
}


template_value_color_bitmap_preview : column {
    : row {
        : column {
            value_edit_slider;
            template_color_widget;
        }
        avis_mat_preview;
    }
    spacer_0_1;
    bitmap_widget;
}


standard_color_widget : boxed_column {
    label = "Color";
    : row {
        : toggle {
            label = "By ACI";
            key = "by_aci";
            mnemonic = "y";
        }
        lock; 
        raytrace_mirror_toggle;
    }
    avis_variable_color_model;
}


template_color_widget : boxed_column {
    label = "Color";
    : row {
        : toggle {
            label = "By ACI";
            key = "by_aci";
            mnemonic = "y";
        }
        raytrace_mirror_toggle;
    }
    avis_variable_color_model;
}


avis_variable_color_model : row { 
    : column {
        : row {
            : column {
                : text {
                    key = "top_text";
                    width = 10;
                    value = "Red:";
                }
                : text {
                    key = "middle_text";
                    width = 10;
                    value = "Green:";
                }
                : text {
                    key = "bottom_text";
                    width = 10;
                    value = "Blue:";
                }
            }
            : column {
                : edit_box_4 {
                    label = "";
                    key = "top_edit";
                } 
                : edit_box_4 {
                    label = "";
                    key = "middle_edit";
                } 
                : edit_box_4 {
                    label = "";
                    key = "bottom_edit";
                }
            }
            : column {
                : slider_0_1_fixed { key = "top_slider"; }
                : slider_0_1_fixed { key = "middle_slider"; }
                : slider_0_1_fixed { key = "bottom_slider"; }
            }
        }
        : row {
            color_system;
            : image_button {
                key = "set_color_image";
                color = -15;             /* background color */
                height = 1;
                aspect_ratio = 1;
            }
        }
    }
}


raytrace_mirror_toggle : toggle {
    label = "Mirror";
    key = "raytrace_mirror";
    mnemonic = "o";
}


bitmap_widget : row {
    : column {
        : row {
            : edit_box_4 {
                label = "Bitmap Blend:";
                key = "blend_edit";
                mnemonic = "d";
                fixed_width = true;
            } 
            : slider_0_1 { 
                key = "blend_slider"; 
                width = 16;
            }
        }
        : edit_box {
            label = "File Name:";
            key = "bitmap_edit";
            mnemonic = "l";
        }
    }
    : column {
        fixed_width = true;
        : button {
            label = "Adjust Bitmap...";
            key = "adjust_map";
            mnemonic = "j";
            fixed_width = true;
        }
        : button {
            label = "Find File...";
            key = "find_bitmap";
            mnemonic = "i";
        }
    }
}



//
// Material attach by ACI dialogue.
//

avis_material_aci : dialog {
  label = "Attach by AutoCAD Color Index";
  : column {
    key = "top_key";
    : row {
        : list_box {
            key = "list";
            label = "Select a Material:";
            mnemonic = "M";
            width = 18;
        }
        : column {
            avis_mat_preview;
            : button {
                label = "Attach ->";
                key = "attach";
                mnemonic = "t";
            }
            : button {
                label = "Detach";
                key = "detach";
                mnemonic = "D";
            }
        }
        : list_box {
            key = "index";
            label = "Select ACI:";
            mnemonic = "A";
            width = 31;
            tabs = "4 12";
            multiple_select = true;
        }
    }
    spacer_1_ok_cancel_help_errtile;
  }
}



//
// Material attach by layer dialogue.
//

avis_material_layer : dialog {
  label = "Attach by Layer";
  : column {
    key = "top_key";
    : row {
        : list_box {
            key = "list";
            label = "Select a Material:";
            mnemonic = "M";
            width = 18;
        }
        : column {
            avis_mat_preview;
            : button {
                label = "Attach ->";
                key = "attach";
                mnemonic = "t";
            }
            : button {
                label = "Detach";
                key = "detach";
                mnemonic = "D";
            }
        }
        : list_box {
            key = "layer";
            label = "Select Layer:";
            mnemonic = "L";
            width = 38;
            tabs = "22";
            multiple_select = true;
        }
    }
    spacer_1_ok_cancel_help_errtile;
  }
}



avis_mat_preview : boxed_column {
    children_alignment = centered;
    : image_button {
        key = "image";
        color = graphics_background;
        height = 8;
        aspect_ratio = 1;
        fixed_height = true;
        fixed_width = true;
        is_tab_stop = false;
    }
    : button {
        key = "object";
        label = "Preview";
        mnemonic = "P";
    }
    : popup_list {
        key = "geometry";
//        list = "Adjust Bitmap...";
        list = "Sphere\nCube";
    }
    spacer_0;
}

avis_mat_preview_LBitmap : boxed_column {
    children_alignment = centered;
    : image_button {
        key = "image";
        color = graphics_background;
        height = 8;
        aspect_ratio = 1;
        fixed_height = true;
        fixed_width = true;
        is_tab_stop = false;
    }
    : button {
        key = "object";
        label = "Preview";
        mnemonic = "P";
    }
    : edit_box {
            label = "Object Size:";
            key   = "fixedScalingPreviewSize";
            value = "1";
            edit_limit = 8;
    }
    : popup_list {
        key = "geometry";
        list = "Sphere\nCube";
    }
    spacer_0;
}

//***************************************************************************
//
// Morderd UV mapper dialog
//
//***************************************************************************
//***************************************************************************
//
//***************************************************************************
OffsetArea : boxed_column {
    label = "Offsets and Rotations";
    : row {
        : edit_box {
            label = "X &Offset:";
            key   = "Xoff";
            value = "0";
            edit_limit = 8;
        }
        : edit_box {
            label = "Y &Offset:";
            key   = "Yoff";
            value = "0";
            edit_limit = 8;
        }
    }
    : row {
        : edit_box {
            label = "&Rotation:";
            key   = "Rot";
            value = "0";
            edit_limit = 5;
        }
        : slider {
            key             = "RotSl";
            min_value       = -180;
            max_value       = 180;
            small_increment = 1;
            big_increment   = 10;
            value = "0";
            is_tab_stop = false;
        }
    }
}


//***************************************************************************
//
//***************************************************************************
PickAdjustGroup : row {
    : button {
        label = "Adjust Bitmap...";
        mnemonic = "B";
        key   = "AdjMap";
    }
    spacer_1;
    : button {
        label = "Pick Points <";
        mnemonic = "i";
        key   = "PickPts";
    }
}

//***************************************************************************
//
//
//***************************************************************************
uv : dialog {
  label = "Mapping";
  : column { 
    key = "top_key";
    : row {
        : column {
            : boxed_radio_column {
                label = "Projection";
                key   = "MapTypes";
                : radio_button {
                    label = "Planar";
                    mnemonic = "l";
                    key   = "Pl";
                }
                : radio_button {
                    label = "Cylindrical";
                    mnemonic = "C";
                    key   = "Cyl";
                }
                : radio_button {
                    label = "Spherical";
                    mnemonic = "S";
                    key   = "Sph";
                }
                : radio_button {
                    label = "Solid";
                    mnemonic = "o";
                    key   = "Sol";
                }
            }

            spacer_1;

            : button {
                label = "Adjust Coordinates...";
                mnemonic = "A";
                key   = "AdjCoor";
            }

            spacer_1;

            : button {
                label = "Acquire From <";
                mnemonic = "F";
                key   = "Acquire";
            }
            : button {
                label = "Copy To <";
                mnemonic = "T";
                key   = "Copy";
            }
        }

        : spacer { width = 5; }

        finish_preview;
    }
    spacer_1_ok_cancel_help_errtile;
  }
}





//***************************************************************************
//
//***************************************************************************
AdjustPlanar : dialog {
  label = "Adjust Planar Coordinates";
  : column { 
    key = "top_key";
    : column {
        : row {
            children_alignment = top;
            : boxed_radio_column {
                label = "Parallel Plane";
                key   = "Orient";
                : radio_button {
                    label = "WCS XY Plane";
                    mnemonic = "X";
                    key   = "WCS_Z";
                }
                : radio_button {
                    label = "WCS XZ Plane";
                    mnemonic = "Z";
                    key   = "WCS_Y";
                }
                : radio_button {
                    label = "WCS YZ Plane";
                    mnemonic = "Y";
                    key   = "WCS_X";
                }
                : radio_button {
                    label = "Picked Plane";
                    mnemonic = "a";
                    key   = "UCS_Z";
                }
            }
            spacer_1;
            : column {
                : boxed_column {
                    label = "Center Position";
                    fixed_height = true;
                    : row {
                        fixed_width = true;
                        : column {
                            fixed_height = true;
                            : image_button {
                                key = "CenterImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "XSlide";
                                min_value = 0;
                                max_value = 10000;
                                is_tab_stop = false;
                            }
                        }
                        : slider {
                            layout = vertical;
                            key = "YSlide";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                }
            }

            spacer_1;

            finish_preview;
        }

        PickAdjustGroup;

        OffsetArea;
    }

    spacer_1_ok_cancel_help_errtile;
  }
}




//***************************************************************************
//
//***************************************************************************
AdjustCyl : dialog {
  label = "Adjust Cylindrical Coordinates";
  : column { 
    key = "top_key";
    : column {
        : row {
            children_alignment = top;
            : boxed_radio_column {
                label = "Parallel Axis";
                key   = "Orient";
                : radio_button {
                    label = "WCS Z Axis";
                    mnemonic = "Z";
                    key   = "WCS_Z";
                }
                : radio_button {
                    label = "WCS Y Axis";
                    mnemonic = "Y";
                    key   = "WCS_Y";
                }
                : radio_button {
                    label = "WCS X Axis";
                    mnemonic = "X";
                    key   = "WCS_X";
                }
                : radio_button {
                    label = "Picked Axis";
                    mnemonic = "A";
                    key   = "UCS_Z";
                }
            }
            spacer_1;
            : column {
                : boxed_column {
                    label = "Central Axis Position";
                    fixed_height = true;
                    : row {
                        fixed_width = true;
                        : column {
                            fixed_height = true;
                            : image_button {
                                key = "CenterImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "XSlide";
                                min_value = 0;
                                max_value = 10000;
                                is_tab_stop = false;
                            }
                        }
                        : slider {
                            layout = vertical;
                            key = "YSlide";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                }
            }

            spacer_1;

            finish_preview;
        }

        PickAdjustGroup;

        OffsetArea;
    }
    spacer_1_ok_cancel_help_errtile;
  }
}




//***************************************************************************
//
//***************************************************************************
AdjustSph : dialog {
    label = "Adjust Spherical Coordinates";
  : column { 
    key = "top_key";
    : column {
        : row {
            children_alignment = top;
            : boxed_radio_column {
                label = "Parallel Axis";
                key   = "Orient";
                : radio_button {
                    label = "WCS Z Axis";
                    mnemonic = "Z";
                    key   = "WCS_Z";
                }
                : radio_button {
                    label = "WCS Y Axis";
                    mnemonic = "Y";
                    key   = "WCS_Y";
                }
                : radio_button {
                    label = "WCS X Axis";
                    mnemonic = "X";
                    key   = "WCS_X";
                }
                : radio_button {
                    label = "Picked Axis";
                    mnemonic = "A";
                    key   = "UCS_Z";
                }
            }
            spacer_1;
            : column {
                : boxed_column {
                    label = "Polar Axis Position";
                    fixed_height = true;
                    : row {
                        fixed_width = true;
                        : column {
                            fixed_height = true;
                            : image_button {
                                key = "CenterImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "XSlide";
                                min_value = 0;
                                max_value = 10000;
                                is_tab_stop = false;
                            }
                        }
                        : slider {
                            layout = vertical;
                            key = "YSlide";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                }
            }

            spacer_1;

            finish_preview;
        }

        PickAdjustGroup;

        OffsetArea;
    }
    spacer_1_ok_cancel_help_errtile;
  }
}




//***************************************************************************
//
//***************************************************************************

BitMapAdjustE : column {
    : row {
        bitmap_image;
        finish_preview;
    } /* end of row */
    offset_scale;
} /* end column */

BitMapAdjustL : column {
    : row {
        bitmap_image;
        avis_mat_preview_LBitmap;
    } /* end of row */
    offset_scale;
} /* end column */

bitmap_image : boxed_column {
    label = "Offset";
    fixed_height = true;
    : row {
        children_alignment = top;
        fixed_width = true;
        : boxed_column {
            fixed_height = true;
            label = "Scale";
            : slider {
        key = "X_SIZE";
        min_value = 0;
        max_value = 102;
        is_tab_stop = false;
            }
            : row {
        fixed_width = true;
        : slider {
            layout = vertical;
            key = "Y_SIZE";
            fixed_width = true;
            min_value = 0;
            max_value = 102;
            is_tab_stop = false;
        }
        : image {
            key   = "BitImage";
            color = graphics_background;
            height = 8;
            aspect_ratio = 1;
            fixed_width = true;
            fixed_height = true;
            is_tab_stop = false;
        }
            } /* end row */
        }
        : column {
            : slider {
        layout = vertical;
        key = "Y_OFFSET";
        min_value = 0;
        max_value = 10000;
        is_tab_stop = false;
            }
        }
    } /* end of row */
    : row {
        : slider {
            key = "X_OFFSET";
            min_value = 0;
            max_value = 10000;
            is_tab_stop = false;
        }
    } /* end of row */
} /* end column */


offset_scale : column {
    : row {
        : column {
            : text {
                label = "Offset:";
            }
            : text {
                label = "Scale:";
            }
        }
        : column {
            : edit_box {
                key   = "OU";
                edit_limit = 8;
            }
            : edit_box {
                key   = "SU";
                edit_limit = 8;
            }
        }
        : column {
            : text {
                label = "U";
            }
            : text {
                label = "U";
            }
        }
        : column {
            : edit_box {
                key   = "OV";
                edit_limit = 8;
            }
            : edit_box {
                key   = "SV";
                edit_limit = 8;
            }
        }
            : column {
            : text {
                label = "V";
            }
            : text {
                label = "V";
            }
        }
    } /* end row */

    : toggle {
        key   = "Aspect";
        label = "&Maintain Aspect Ratio";
    }
} /* end column */


//***************************************************************************
//
//***************************************************************************
AdjustEBitmap : dialog {
  label = "Adjust Object Bitmap Placement";
  : column { 
    key = "top_key";

    : row {
        BitMapAdjustE;

        : column {
            : boxed_radio_column {
                key   = "Tiling";
                label = "Tiling";
                spacer_1;
                spacer_1;
                : radio_button {
                    key   = "T_DEF";
                    label = "&DEFAULT";
                }
                : radio_button {
                    key   = "T_TILE";
                    label = "&TILE";
                }
                : radio_button {
                    key   = "T_CROP";
                    label = "&CROP";
                }
                spacer_1;
                spacer_1;
            }
            spacer;
        }
    }
    ok_cancel_help_errtile;
  }
}

//***************************************************************************
//
//***************************************************************************
AdjustLBitmap : dialog {
  label = "Adjust Material Bitmap Placement";
  : column { 
    key = "top_key";

    : row {
        BitMapAdjustL;

        : column {
            : boxed_radio_column {
                spacer_1;
                spacer_1;
                key   = "Tiling";
                label = "Tiling";
                : radio_button {
                    key   = "T_TILE";
                    label = "&TILE";
                }
                : radio_button {
                    key   = "T_CROP";
                    label = "&CROP";
                }
                spacer_1;
                spacer_1;
            }
            : boxed_radio_column {
                key   = "MaterialMapping";
                label = "Map Style";
                spacer_1;
                : radio_button {
                    key   = "FixedScaling";
                    label = "&Fixed Scale";
                }
                : radio_button {
                    key   = "FitToEntity";
                    label = "Fit To &Object";
                }
                spacer_1;
            }
            : toggle {
                key = "AutoAxis";
                label = "Use &Auto Axis";
            }
            spacer_1;
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
  }
}

//***************************************************************************
//
//***************************************************************************
AdjustRST : dialog {
  label = "Adjust UVW Coordinates";
  : column { 
    key = "top_key";
    : row {
        : column {
            : row {
                : edit_box {
                    label = "&U Scale:";
                    key   = "RS";
                    value = "1.0";
                    edit_limit = 10;
                }
                : slider {
                    key = "R_SLIDE";
                    min_value = 0;
                    max_value = 100;
                    width = 15;
                    is_tab_stop = false;
                }
            }
            : row {
                : edit_box {
                    label = "&V Scale:";
                    key   = "SS";
                    value = "1.0";
                    edit_limit = 10;
                }
                : slider {
                    key = "S_SLIDE";
                    min_value = 0;
                    max_value = 100;
                    width = 15;
                    is_tab_stop = false;
                }
            }
            : row {
                : edit_box {
                    label = "&W Scale:";
                    key   = "TS";
                    value = "1.0";
                    edit_limit = 10;
                }
                : slider {
                    key = "T_SLIDE";
                    min_value = 0;
                    max_value = 100;
                    width = 15;
                    is_tab_stop = false;
                }
            }
            : row {
                : button {
                    label = "Pick Points <";
                    mnemonic = "i";
                    key   = "Pick4Pts";
                }
                : toggle {
                    key   = "ARatio";
                    label = "&Maintain Aspect Ratio";
                }
            }
        }

        finish_preview;

    }
 
    spacer_1_ok_cancel_help_errtile;
  }
}

//***************************************************************************
// Support for Background image dialog
//***************************************************************************
background : dialog {
    label = "Background";
    : row { 
        key = "top_key";
        : column {
            : radio_row {
                key = "bg_type";
                : radio_button {
                    key = "solid";
                    label = "Solid";
                    mnemonic = "S";
                }
                : radio_button {
                    key = "gradient";
                    label = "Gradient";
                    mnemonic = "G";
                }
                : radio_button {
                    key = "image";
                    label = "Image";
                    mnemonic = "a";
                }
                : radio_button {
                    key = "merge";
                    label = "Merge";
                    mnemonic = "M";
                }
            }
            : row {
                : boxed_column {
                    key = "top_colors";
                    label = "Colors";
                    : row {
                        : column {
                            : row {
                                : column {
                                    : text {
                                        key = "color1_txt";
                                        label = "Top";
                                    }
                                    spacer;
                                    : text {
                                        key = "color2_txt";
                                        label = "Middle";
                                    }
                                    spacer;
                                    : text {
                                        key = "color3_txt";
                                        label = "Bottom";
                                    }
                                } /* column */
                                spacer;
                                : column {
                                    alignment = top;
                                    : image_button {
                                        key = "color1";
                                        color = graphics_background;
                                        fixed_width = true;
                                        aspect_ratio = 1;
                                        width = 2;
                                    }
                                    spacer;
                                    : image_button {
                                        key = "color2";
                                        color = graphics_background;
                                        fixed_width = true;
                                        aspect_ratio = 1;
                                        width = 2;
                                    }
                                    spacer;
                                    : image_button {
                                        key = "color3";
                                        color = graphics_background;
                                        fixed_width = true;
                                        aspect_ratio = 1;
                                        width = 2;
                                    }
                                } /* column */
                            } /* row */
                        } /* column */
                        spacer;
                        : column {
                            color_system;
                            color_system_set;
                        } /* column */
                    } /* row */
                    : column {
                        : row {
                            : toggle {
                                key = "use_acad";
                                label = "AutoCAD Background";
                                mnemonic = "D";
                            }
                            : button {
                               label = "Select Color";
                               key = "set_color";
                               fixed_width = true;
                               mnemonic = "C";
                            }
                        }
                    }
                } /* Color row */
                : boxed_column {
                    key = "top_preview";
                    children_alignment = centered;
                    : image_button {
                        key = "preview_img";
                        color = graphics_background;
                        width = 12;
                        height = 6;
                        fixed_height = true;
                        fixed_width = true;
                        is_tab_stop = false;
                    }
                    : button {
                        key = "bg_p_button";
                        label = "Preview";
                        mnemonic = "P";
                    }
                    spacer_0;
                }
            } /* row */
            : row {
                : boxed_column {
                    key = "top_image";
                    label = "Image";
                    : edit_box {
                        label = "Name:";
                        key = "bg_filename";
                        edit_width = 12;
                        mnemonic = "N";
                    }
                    : button {
                        label = "Find File...";
                        key = "bg_find_bitmap";
                        mnemonic = "F";
                    }
                    : button {
                        label = "Adjust Bitmap...";
                        key = "adjust_map";
                        mnemonic = "B";
                    }
                }
                : boxed_column {
                    key = "top_ray";
                    label = "Environment";
                    : edit_box {
                        label = "Name:";
                        key = "env_filename";
                        edit_width = 12;
                        mnemonic = "N";
                    }
                    : button {
                        label = "Find File...";
                        key = "env_find_bitmap";
                        mnemonic = "i";
                    }
                    : toggle {
                        label = "Use Background";
                        key = "lock_env_to_bg";
                        mnemonic = "U";
                    }
                }
                : boxed_column {
                    key = "top_sliders";
                    : row {
                        : column {
                            : edit_box_4 {
                                label = "Horizon:";
                                key = "horizon_edt";
                                min_value = 0;
                                max_value = 100;
                                value = "0.5";
                                mnemonic = "H";
                            }
                            : edit_box_4 {
                                label = "Height:";
                                key = "height_edt";
                                min_value = 0;
                                max_value = 100;
                                value = "0.5";
                                mnemonic = "e";
                            }
                            : edit_box_4 {
                                label = "Rotation:";
                                key = "angle_edt";
                                value = "90";
                                mnemonic = "R";
                            }
                        } /* edit column */
                        : column {
                            : slider {
                                key = "horizon_sld";
                                min_value = 0;
                                max_value = 100;
                                value = "50";
                                width = 14;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "height_sld";
                                min_value = 0;
                                max_value = 100;
                                value = "33";
                                width = 14;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "angle_sld";
                                min_value = -90;
                                max_value = 90;
                                value = "0";
                                width = 14;
                                is_tab_stop = false;
                            }
                        } /* slider column */
                   } /* row */
                } /* boxed column */
            } /* bottom row */
        ok_cancel_help_errtile;
        }
    }
}


//***************************************************************************
// Support for adjusting the Background image
//***************************************************************************

AdjustBGBitmap : dialog {
  label = "Adjust Background Bitmap Placement";
  : column { 
    key = "top_key";
    : column {
        : row {
            : boxed_column {
                key = "Offset_box";
                label = "Offset";
                fixed_height = true;
                : row {
                    children_alignment = top;
                    fixed_width = true;
                    : boxed_column {
                        fixed_height = true;
                        label = "Scale";
                        : slider {
                            key = "X_SIZE";
                            min_value = 0;
                            max_value = 102;
                            is_tab_stop = false;
                        }
                        : row {
                            fixed_width = true;
                            : slider {
                                layout = vertical;
                                key = "Y_SIZE";
                                fixed_width = true;
                                min_value = 0;
                                max_value = 102;
                                is_tab_stop = false;
                            }
                            : image {
                                key   = "BitImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                        } /* end row */
                    }
                    : column {
                        : slider {
                            layout = vertical;
                            key = "Y_OFFSET";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                } /* end of row */
                : row {
                    : slider {
                        key = "X_OFFSET";
                        min_value = 0;
                        max_value = 10000;
                        is_tab_stop = false;
                    }
                } /* end of row */
            } /* end column */
    
            : column {
                : toggle {
                    label = "Fit To Screen";
                    mnemonic = "F";
                    key = "fit_to_screen";
                    value = "1";
                }
    
                : toggle {
                    key   = "BGAspect";
                    label = "Use Image Aspect Ratio";
                    mnemonic = "U";
                }
    
                spacer_1;
                : boxed_radio_column {
                    alignment = centered;
                    key   = "Tiling";
                    label = "Tiling";
                    : radio_button {
                        key   = "T_TILE";
                        label = "TILE";
                        mnemonic = "T";
                    }
                    : radio_button {
                        key   = "T_CROP";
                        label = "CROP";
                        mnemonic = "C";
                    }
                }
                spacer_1;
                : button {
                    alignment = centered;
                    label = "Center";
                    mnemonic = "e";
                    key = "center";
                }
            }
        } /* end of row */
    
        : column {
            : row {
                key = "Edit_Boxes";
                : column {
                    : text {
                        label = "Offset:";
                    }
                    : text {
                        label = "Scale:";
                    }
                }
                : column {
                    : edit_box {
                        label = "X:";
                        mnemonic = "X";
                        key   = "OX";
                        edit_limit = 8;
                    }
                    : edit_box {
                        label = "X:";
                        mnemonic = "X";
                        key   = "SX";
                        edit_limit = 8;
                    }
                }
                : column {
                    : edit_box {
                        label = "Y:";
                        mnemonic = "Y";
                        key   = "OY";
                        edit_limit = 8;
                    }
                    : edit_box {
                        label = "Y:";
                        mnemonic = "Y";
                        key   = "SY";
                        edit_limit = 8;
                    }
                }
            } /* end row */
            : toggle {
                key   = "Aspect";
                label = "Maintain Apect Ratio";
                mnemonic = "M";
            }
        } /* end of boxed column */
    } /* end column */
    ok_cancel_help_errtile;
  }
}

//***************************************************************************
//***************************************************************************
//***************************************************************************
//***************************************************************************
// Support for Landscape objects
//***************************************************************************
//***************************************************************************
landscape_library : dialog {
    label = "Landscape Library";
    : row {
        : column {
            : text {
                key = "lib_name";
                width = 25;
                fixed_width = true;
            }
            : list_box {
                key = "plant_list";
                width = 25;
            }
        }
        : column {
            spacer_1;
            : button {
                label = "&Modify...";
                key = "plant_lib_modify";
            }
            : button {
                label = "&New...";
                key = "plant_lib_new";
            }
            : button {
                label = "&Delete";
                key = "plant_lib_delete";
            }
            : button {
                label = "&Open...";
                key = "plant_lib_open";
            }
            : button {
                label = "&Save...";
                key = "plant_lib_save";
            }
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
}

landscape_lib_delete : dialog {
    label = "Landscape Library Delete";
    : column {
        : text {
             key = "name";
             width = 30;
             alignment = centered;
        }
        : text {
            label = "Are you sure?";
            alignment = centered;
        }
    }
    ok_cancel;
}

landscape_lib_mod : dialog {
    key = "dialog";
    : column {
        key = "top_key";
        : row {
            : boxed_column {
                label = "Default Geometry";
                alignment = top;
                fixed_height = true;
                : radio_column {
                    key = "align_radio";
                    fixed_height = true;
                    : radio_button {
                        key = "align_single";
                        label = "Single Face";
                        mnemonic = "S";
                    }
                    : radio_button {
                        key = "align_cross";
                        label = "Crossing Faces";
                        mnemonic = "C";
                    }
                }
                spacer_1;
                : toggle {
                    key = "align_toggle";
                    label = "&View Aligned";
                }
           }  
           finish_preview;
        }
        : row {
            : column {
                :  text {
                    label = "Name:";
                }
                : text {
                    label = "Image File:";
                }
                : text {
                    label = "Opacity Map File:";
                }
            }
            : column {
                :  edit_box {
                    key = "plant_name";
                    edit_width = 14;
                }
                : edit_box {
                    key = "plant_image";
                    edit_width = 14;
                }
                : edit_box {
                    key = "plant_opac";
                    edit_width = 14;
                }
            }
            : column {
                spacer_1;
                spacer_1;
                : button {
                    label = "Find File...";
                    key = "ld_find_image";
                    mnemonic = "F";
                }
                : button {
                    label = "Find File...";
                    key = "ld_find_opac";
                    mnemonic = "n";
                }
            }
        }
        spacer_1;
        ok_cancel_help_errtile;
    }
}

landscape_lib_confirm : dialog {
    label = "Landscape Library Modification";
    initial_focus = "save";
    : column {
        : text {
            label = "The current landscape library has been changed.";
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

landscape_edit : dialog {
    key = "dialog";
    : column {
        key = "topkey";
        : row {
            : column {
                : text {
                    key = "lib_name";
                    width = 25;
                    fixed_width = true;
                }
                : list_box {
                    key = "plant_list";
                    width = 25;
                }
            }
            : column {
                finish_preview;
             }
        }
        : row {
            : boxed_column {
                label = "Geometry";
                : radio_column {
                    key = "align_radio";
                    fixed_height = true;
                    : radio_button {
                        key = "align_single";
                        label = "Single Face";
                        mnemonic = "S";
                    }
                    : radio_button {
                        key = "align_cross";
                        label = "Crossing Faces";
                        mnemonic = "C";
                    }
               }
               spacer_1;
               : toggle {
                 key = "align_toggle";
                 label = "&View Aligned";
               }
            }
            : column {
                spacer_0;
                : edit_box_4 {
                    label = "&Height:";
                    key = "height_txt";
                    fixed_width = true;
                    value = "30";
                }
                : slider {
                    label = "Height";
                    key = "height_sld";
                    min_value = 1;
                    max_value = 100;
                    value = "30";
                }
                : button {
                    label = "&Position <";
                    key = "plant_placement";
                }
            }
        }
    ok_cancel_help_errtile;
    }
}

//***************************************************************************
// Support for Fog/Depth cue
//***************************************************************************
fog : dialog {
    label = "Fog / Depth Cue";
    : row {
        : toggle {
           label = "&Enable Fog";
           key = "fog_enable";
        }
        : toggle {
           label = "Fog &Background";
           key = "fog_background";
        }
    }
    : column {
        key = "values_to_edit";
        : row {
            : boxed_column {
                color_system;
                color_system_set;
                light_color_panel;
            }
        }
        : boxed_row {
            : column {
                : text {
                    label = "Near Distance:      ";
                    key = "near_dist_txt";
                    width = 22;
                    fixed_width = true;
                }
                : text {
                    label = "Far Distance:";
                    key = "far_dist_txt";
                }
            }
            : column {
                : edit_box_4 {
                    key = "near_dist_txt_ed";
                    value = "0";
                }
                : edit_box_4 {
                    key = "far_dist_txt_ed";
                    value = "100";
                }
            }
            : column {
                : slider_0_1_fixed { key = "near_dist_sld"; }
                : slider_0_1_fixed { key = "far_dist_sld"; }
            }
        }
        : boxed_row {
            : column {
                : text {
                    label = "Near Fog Percentage:";
                    key = "near_pct_txt";
                    width = 22;
                    fixed_width = true;
                }
                : text {
                    label = "Far Fog Percentage:";
                    key = "far_pct_txt";
               }
            }
            : column {
                : edit_box_4 {
                    key = "near_pct_txt_ed";
                    value = "0";
                }
                : edit_box_4 {
                    key = "far_pct_txt_ed";
                    value = "100";
                }
            }
            : column {
                : slider_0_1_fixed { key = "near_pct_sld"; }
                : slider_0_1_fixed { key = "far_pct_sld"; }
            }
        }
    }
    spacer_1;
    ok_cancel_help_errtile;
}
