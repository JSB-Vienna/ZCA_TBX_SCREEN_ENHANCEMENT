"! <p class="shorttext synchronized" lang="en">CA-TBX: Enhance screen/dynpro by docking container</p>
"!
"! <p>This class serves as a basis to enhance any screen by a docking container to provide additional
"! information to the user. For the consumer this class should handle the buffering of the controls except
"! hiding and displaying the controls when already created. This class can not be used by itself and is
"! therefore abstract.</p>
"! <p>Use this class <strong>ONLY at event PBO of a main screen</strong>. It must not be used in subscreens
"! or popups.</p>
"! <p>There are two possible options how to use this class. It is controlled via the parameter
"! IV_CREATE_DC_PER_SCREEN of the constructor.
"! <ol>
"! <li>ABAP_FALSE (= Default): A transaction has several screens and the docking container should display
"! always the same content, e. g. like transaction FB02 (Change FI Document) displaying an archived document.</li>
"! <br>
"! <li>ABAP_TRUE: One transaction is the starting point for several dialogs and the docking container contains
"! different views per dialog, e. g. like transaction SBWP (Business Workplace) where one-screen-dialogs
"! display an approval history and the archived document at the same time.</li>
"! </ol>
"! <p>The necessary steps are:
"! <ol>
"! <li>Export a unique number into the memory in your program that call the enhanced screen. The key should
"! be of the structured type SIBFLPORB. Pass at least in TYPEID the class or object name and in INSTID the
"! unique number, e. g. the concatenated key values of the object. It is strongly recommended to use as
"! MEMORY ID a constant, which ease the search for it.</li>
"! <br>
"! <li>Create a class that inherits from this class and create or redefine the following methods:</li>
"! <ul>
"! <li>Create the <strong>CONSTRUCTOR</strong> and fill the ranges for program name and screen number as
"! filter for method IS_RELEVANT_TO_DISPLAY if the docking container should appear or not.</li>
"! <br>
"! <li><strong>Redefine method IS_RELEVANT_TO_DISPLAY</strong> to check the filter created in the CONSTRUCTOR
"! and import your unique number, which is also an indicator if the docking container should be displayed.<br>
"! If this is not precise enough, than this is the place to check additional values. Either you create an
"! an instance of the calling program (typically where the EXPORT comes from) and/or you pass values from the
"! transaction via parameter IT_PARAMS into this method.</li>
"! <br>
"! <li><strong>Redefine method CREATE_N_DISPLAY_DOCKING</strong> and call the SUPER->METHOD. This will create
"! a docking container instance provided in structure MS_DOCIING_CNT_BUFFER. After that you fill the container
"! as it is required.</li>
"! </ul>
"! <li>Find a spot for an implicit enhancement in the wished main screen. Important is only that the
"! subroutine is not left before the end. So a preferred spot should be the beginning of a subroutine.</li>
"! <br>
"! <li>In the enhancement you implement a pattern like this:<br>
"! TRY.<br>
"!     DATA(lo_enh_by_docking) = NEW yourclassname( ).<br>
"!     IF lo_enh_by_docking->is_relevant_to_display( ).<br>
"!       lo_enh_by_docking->create_n_display_docking( ... ).<br>
"!     ENDIF.<br>
"! <br>
"!   CATCH zcx_ca_scr_enhancement_by_dc INTO DATA(lx_catched).<br>
"!     MESSAGE lx_catched TYPE 'S' DISPLAY LIKE lx_catched->mv_msgty.<br>
"! ENDCATCH.<br>
"! </ol>
CLASS zcl_ca_scr_enhancement_by_dc DEFINITION PUBLIC
                                              CREATE PUBLIC
                                              ABSTRACT.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Usage of docking container</p>
      BEGIN OF cs_usage,
        "! <p class="shorttext synchronized" lang="en">Use the same docking container for each following screen</p>
        "! <p>The docking container is used concurrently to a stream of several screens during one transaction,
        "! like e. g. FI transactions.</p>
        "! <p>
        same_dc_for_each_screen TYPE abap_boolean VALUE abap_false,
        "! <p class="shorttext synchronized" lang="en">Create a docking container for each screen</p>
        "! <p>The docking container is used for one screen but different objects / keys, like e. g. most individual
        "! custom transactions starting out of the Business Workplace (transaction SBWP).</p>
        new_dc_for_each_screen  TYPE abap_boolean VALUE abap_true,
      END OF cs_usage.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Initialize buffers</p>
      "!
      "! <p>Use this method if buffering is a problem or not worthy.</p>
      initialize_buffers.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor - use to set filter</p>
      "!
      "! @parameter is_actual_screen        | <p class="shorttext synchronized" lang="en">Actual screen name - only if not correct by Constructor</p>
      "! @parameter iv_create_dc_per_screen | <p class="shorttext synchronized" lang="en">X = Create a new container for each screen -> see docu.</p>
      "! <ul><li>Use ABAP_FALSE if your docking container should be visible in a dialog with several different screen. The container is then created only once
      "! and will be link to the next screen. This should avoid reloads of data which can be costly</li>
      "! <br>
      "! <li>Use ABAP_TRUE if the docking container is used in dialogs that have always the same caller, e. g. the business workplace. Then ...</li>
      "! </ul>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      constructor
        IMPORTING
          is_actual_screen        TYPE /aif/extdynpro OPTIONAL
          iv_create_dc_per_screen TYPE abap_boolean DEFAULT cs_usage-same_dc_for_each_screen
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Create a dialog box (instead of docking) container + display</p>
      "!
      "! @parameter iv_width     | <p class="shorttext synchronized" lang="en">Width of the dialog box</p>
      "! @parameter iv_height    | <p class="shorttext synchronized" lang="en">Height of the dialog box</p>
      "! @parameter iv_top       | <p class="shorttext synchronized" lang="en">Position of the dialog box - Top edge</p>
      "! @parameter iv_left      | <p class="shorttext synchronized" lang="en">Position of the dialog box - Left-hand edge</p>
      "! @parameter iv_style     | <p class="shorttext synchronized" lang="en">Control appearance+behavior (use const CL_GUI_CONTROL=>WS_*)</p>
      "! @parameter iv_lifetime  | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! <ul>
      "! <li>Using value IMODE the control remains alive as long as the internal session is alive, that is, e. g. until a statement such
      "! as LEAVE PROGRAM or LEAVE TO TRANSACTION is executed.</li>
      "! <li>Using value DYNPRO the control remains alive for the lifetime of the screen. That is, while it remains in the screen stack. It is
      "! not destroyed, e. g. by a CALL SCREEN or CALL TRANSACTION statement.</li>
      "! </ul>
      "! @parameter iv_caption  | <p class="shorttext synchronized" lang="en">Dialog box caption</p>
      "! @parameter iv_cnt_name | <p class="shorttext synchronized" lang="en">Container name</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_n_display_dialogbox
        IMPORTING
          iv_width     TYPE i         DEFAULT 30
          iv_height    TYPE i         default 30
          iv_top       TYPE i         default 0
          iv_left      TYPE i         default 0
          iv_style     TYPE i         OPTIONAL
          iv_lifetime  TYPE i         DEFAULT cl_gui_control=>lifetime_imode
          iv_caption   TYPE csequence OPTIONAL
          iv_cnt_name  TYPE csequence OPTIONAL
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Create the docking container and display</p>
      "!
      "! @parameter iv_side      | <p class="shorttext synchronized" lang="en">Where to dock at the screen</p>
      "! @parameter iv_extension | <p class="shorttext synchronized" lang="en">Extension of the control in mm or pixel (see METRIC)</p>
      "! @parameter iv_metric    | <p class="shorttext synchronized" lang="en">How to interpret EXTENSION, use const CL_GUI_CO...=>METRIC_*</p>
      "! @parameter iv_ratio     | <p class="shorttext synchronized" lang="en">Part of the screen in percent; takes priority over EXTENSION</p>
      "! @parameter iv_style     | <p class="shorttext synchronized" lang="en">Control appearance+behavior (use const CL_GUI_CONTROL=>WS_*)</p>
      "! @parameter iv_lifetime  | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=>LIFETIME_*)</p>
      "! <ul>
      "! <li>Using value IMODE the control remains alive as long as the internal session is alive, that is, e. g. until a statement such
      "! as LEAVE PROGRAM or LEAVE TO TRANSACTION is executed.</li>
      "! <li>Using value DYNPRO the control remains alive for the lifetime of the screen. That is, while it remains in the screen stack. It is
      "! not destroyed, e. g. by a CALL SCREEN or CALL TRANSACTION statement.</li>
      "! </ul>
      "! @parameter iv_caption  | <p class="shorttext synchronized" lang="en">Caption of the docking container</p>
      "! @parameter iv_cnt_name | <p class="shorttext synchronized" lang="en">Container name</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_n_display_docking
        IMPORTING
          iv_side      TYPE i         DEFAULT cl_gui_docking_container=>dock_at_right
          iv_extension TYPE i         OPTIONAL
          iv_metric    TYPE i         DEFAULT cl_gui_control=>metric_default
          iv_ratio     TYPE i         OPTIONAL
          iv_style     TYPE i         OPTIONAL
          iv_lifetime  TYPE i         DEFAULT cl_gui_control=>lifetime_imode
          iv_caption   TYPE csequence OPTIONAL
          iv_cnt_name  TYPE csequence OPTIONAL
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Check, if it is currently relevant to enhance the screen</p>
      "!
      "! @parameter is_lporb       | <p class="shorttext synchronized" lang="en">BO key - for buffering purposes</p>
      "! @parameter it_params      | <p class="shorttext synchronized" lang="en">Parameters and there value references</p>
      "! @parameter rv_is_relevant | <p class="shorttext synchronized" lang="en">X = Is relevant to display</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      is_relevant_to_display
        IMPORTING
          is_lporb              TYPE sibflporb     OPTIONAL
          it_params             TYPE zca_tt_params OPTIONAL
        RETURNING
          VALUE(rv_is_relevant) TYPE abap_boolean
        RAISING
          zcx_ca_scr_enhancement_by_dc.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Workarea for buffered controls / containers</p>
      BEGIN OF ty_s_control_buffer,
        "! <p class="shorttext synchronized" lang="en">Free key or that of the instance in O_OBJECT</p>
        s_lporb            TYPE sibflporb,
        "! <p class="shorttext synchronized" lang="en">Control/container to be able to control the visibility of it</p>
        "!
        "! E. g. if you switch between different objects displayed in the docking container that you have
        "! access to the corresponding child to e. g. control the visibility.
        o_control          TYPE REF TO cl_gui_control,
        "! <p class="shorttext synchronized" lang="en">Assigned parent container -> O_CONTROL is child of this</p>
        "! The parent can be all childs of CL_GUI_CONTAINER
        o_parent_container TYPE REF TO cl_gui_container,
        "! <p class="shorttext synchronized" lang="en">Creating and controlling class of O_CONTROL</p>
        o_object           TYPE REF TO object,
      END   OF ty_s_control_buffer,
      "! <p class="shorttext synchronized" lang="en">Buffer for used controls / containers</p>
      ty_t_control_buffer TYPE SORTED TABLE OF ty_s_control_buffer
                                               WITH UNIQUE KEY s_lporb
                                               WITH NON-UNIQUE SORTED KEY by_control
                                                                      COMPONENTS o_control,

      "! <p class="shorttext synchronized" lang="en">Workarea for buffered docking containers</p>
      BEGIN OF ty_s_container_buffer.
        INCLUDE TYPE /aif/extdynpro AS s_screen_name.
    TYPES:
        o_container TYPE REF TO cl_gui_container,
      END   OF ty_s_container_buffer,
      "! <p class="shorttext synchronized" lang="en">Buffer for used docking containers</p>
      ty_t_container_buffer TYPE SORTED TABLE OF ty_s_container_buffer
                                                 WITH UNIQUE KEY s_screen_name.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Buffer for used controls / containers</p>
      mt_control_buffer   TYPE ty_t_control_buffer,
      "! <p class="shorttext synchronized" lang="en">Buffer for used docking containers</p>
      mt_container_buffer TYPE ty_t_container_buffer,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Last called program and screen name</p>
      ms_last_screen      TYPE /aif/extdynpro.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for select options / range tables</p>
      mo_sel_options              TYPE REF TO zcl_ca_c_sel_options,
      "! <p class="shorttext synchronized" lang="en">Screen field attributes (usage with table SCREEN)</p>
      mo_scr_fld_attr             TYPE REF TO zcl_ca_c_screen_field_attr,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Relevant dynpro numbers</p>
      mra_dynnr                   TYPE RANGE OF syst_dynnr,
      "! <p class="shorttext synchronized" lang="en">Relevant program names</p>
      mra_program                 TYPE RANGE OF syrepid,

*     d a t a   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Workarea for buffered docking containers</p>
      mr_container_buffer         TYPE REF TO ty_s_container_buffer,
      "! <p class="shorttext synchronized" lang="en">Workarea for buffered controls / containers</p>
      mr_control_buffer           TYPE REF TO ty_s_control_buffer,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Actual program and screen name</p>
      ms_actual_screen            TYPE /aif/extdynpro,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Docking container is new; ' ' = already buffered</p>
      mv_docking_cnt_is_new       TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X = Screen has changed; ' ' = Same screen</p>
      mv_screen_has_changed       TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X = Create a new container for each screen</p>
      mv_create_dc_per_screen     TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">Number of used lines as defined</p>
      mv_used_scr_rows            TYPE syst_srows,
      "! <p class="shorttext synchronized" lang="en">Number of used columns as defined</p>
      mv_used_scr_columns         TYPE syst_scols,
      "! <p class="shorttext synchronized" lang="en">Calculated ratio of free space to the right side</p>
      mv_calc_ratio_on_the_right  TYPE i,
      "! <p class="shorttext synchronized" lang="en">Calculated ratio of free space at the bottom</p>
      mv_calc_ratio_at_the_bottom TYPE i,
      "! <p class="shorttext synchronized" lang="en">Width of the dialog box</p>
      mv_width                    TYPE i,
      "! <p class="shorttext synchronized" lang="en">Height of the dialog box</p>
      mv_height                   TYPE i,
      "! <p class="shorttext synchronized" lang="en">Position of the dialog box - Top edge</p>
      mv_top                      TYPE i,
      "! <p class="shorttext synchronized" lang="en">Position of the dialog box - Left edge</p>
      mv_left                     TYPE i,
      "! <p class="shorttext synchronized" lang="en">Docking side</p>
      mv_side                     TYPE i,
      "! <p class="shorttext synchronized" lang="en">DC extension (used free space; unit depends on METRIC value)</p>
      mv_extension                TYPE i,
      "! <p class="shorttext synchronized" lang="en">Screen metric, Millimeter or Pixel (see CL_GUI_C.=>METRIC_*)</p>
      mv_metric                   TYPE i,
      "! <p class="shorttext synchronized" lang="en">Ratio the screen is used in percent; wins over EXTENSION</p>
      mv_ratio                    TYPE i,
      "! <p class="shorttext synchronized" lang="en">A combination of CL_GUI_CONTROL=>WS_* by addition</p>
      mv_style                    TYPE i,
      "! <p class="shorttext synchronized" lang="en">Lifetime of control</p>
      mv_lifetime                 TYPE i,
      "! <p class="shorttext synchronized" lang="en">Caption of control</p>
      mv_caption                  TYPE string,
      "! <p class="shorttext synchronized" lang="en">Container name, eventually important for CFW method LINK</p>
      mv_cnt_name                 TYPE string.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add docking container to buffer for the current screen</p>
      "!
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      add_docking_cnt_to_buffer
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Calculate ratio of free space as proposal</p>
      calculate_ratio_of_free_space,

      "! <p class="shorttext synchronized" lang="en">Connect existing docking container to a different screen</p>
      "!
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      connect_docking_cnt_to_screen
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Create docking container (redefine, if constr param too less</p>
      "!
      "! @parameter ro_dialogbox_cnt             | <p class="shorttext synchronized" lang="en">Dialogbox container instance</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_dialogbox_container
        RETURNING
          VALUE(ro_dialogbox_cnt) TYPE REF TO cl_gui_dialogbox_container
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Create docking container (redefine, if constr param too less</p>
      "!
      "! @parameter ro_docking_cnt               | <p class="shorttext synchronized" lang="en">Docking container instance</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_docking_container
        RETURNING
          VALUE(ro_docking_cnt) TYPE REF TO cl_gui_docking_container
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Lookup for an active workitem and get object</p>
      "!
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      create_docking_cnt_per_screen
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Lookup for an active workitem and get object</p>
      "!
      "! @parameter ro_control                   | <p class="shorttext synchronized" lang="en">Determined control / container bound to DC</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      determine_ctrl_from_dc_childs
        RETURNING
          VALUE(ro_control) TYPE REF TO cl_gui_control
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Get control from buffer (check for workitem, if requested)</p>
      "!
      "! @parameter is_lporb                     | <p class="shorttext synchronized" lang="en">BO key - for buffering purposes</p>
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      get_control_from_buffer
        IMPORTING
          is_lporb                 TYPE sibflporb
        RETURNING
          VALUE(rr_control_buffer) TYPE REF TO ty_s_control_buffer
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Get docking from buffer (creates + insert DC if not exist)</p>
      "!
      "! @raising   zcx_ca_scr_enhancement_by_dc | <p class="shorttext synchronized" lang="en">Common exception: Enhancing screen/dynpro by docking contain</p>
      get_docking_cnt_from_buffer
        RAISING
          zcx_ca_scr_enhancement_by_dc,

      "! <p class="shorttext synchronized" lang="en">Hide other/recently displayed docking containers</p>
      hide_other_docking_cnts,

      "! <p class="shorttext synchronized" lang="en">Recreate connection to screen if it is lost</p>
      reconnect_docking_cnt_2_screen,

      "! <p class="shorttext synchronized" lang="en">Set all included controls / containers in- / visible</p>
      "!
      "! @parameter io_control     | <p class="shorttext synchronized" lang="en">Control / Container</p>
      "! @parameter iv_is_visible  | <p class="shorttext synchronized" lang="en">X = Elements are visible</p>
      "! @parameter iv_except_ctrl | <p class="shorttext synchronized" lang="en">X = Except control itself from hiding</p>
      set_control_n_childs_visibilty
        IMPORTING
          io_control     TYPE REF TO cl_gui_control
          iv_is_visible  TYPE abap_boolean DEFAULT abap_true
          iv_except_ctrl TYPE abap_boolean DEFAULT abap_false.

ENDCLASS.



CLASS ZCL_CA_SCR_ENHANCEMENT_BY_DC IMPLEMENTATION.


  METHOD set_control_n_childs_visibilty.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    TRY.
        IF io_control IS NOT BOUND.
          RETURN.
        ENDIF.

        "Since this method calls itself recursively this CAST here works like a CONTINUE,
        "when it fails.
        DATA(lo_container) = CAST cl_gui_container( io_control ).

        LOOP AT lo_container->children INTO DATA(lo_child).
          set_control_n_childs_visibilty( io_control    = lo_child
                                          iv_is_visible = iv_is_visible ).
        ENDLOOP.

        IF iv_except_ctrl EQ abap_false.
          zcl_ca_cfw_util=>set_visible( io_control = io_control
                                        iv_visible = iv_is_visible ).
        ENDIF.

      CATCH cx_sy_move_cast_error.
        "Only containers have children
    ENDTRY.
  ENDMETHOD.                    "set_control_n_childs_visibilty


  METHOD create_docking_container.
    "---------------------------------------------------------------------*
    "     Creates the docking container. If the parameters of the
    "     constructor are not enough redefine this method and provide
    "     the values for your needs.
    "---------------------------------------------------------------------*
    TRY.
        IF mv_extension IS INITIAL AND
           mv_ratio     IS INITIAL.
          "calculate RATIO using class CL_DYNPRO or objects in package SBAC
          "or use may be the class that returns the program name and screen number
          "as used in class ZCL_CA_C_SCREEN_FIELD_ATTR.
          calculate_ratio_of_free_space( ).

          mv_ratio = SWITCH #( mv_side WHEN cl_gui_docking_container=>dock_at_right OR
                                            cl_gui_docking_container=>dock_at_left
                                         THEN mv_calc_ratio_on_the_right

                                       WHEN cl_gui_docking_container=>dock_at_bottom OR
                                            cl_gui_docking_container=>dock_at_top
                                         THEN mv_calc_ratio_at_the_bottom ).
        ENDIF.

        ro_docking_cnt = zcl_ca_cfw_util=>create_docking_container(
                                                            iv_repid     = ms_actual_screen-progname
                                                            iv_dynnr     = CONV #( ms_actual_screen-dynpro )
                                                            iv_side      = mv_side
                                                            iv_ratio     = mv_ratio
                                                            iv_extension = mv_extension
                                                            iv_metric    = mv_metric
                                                            iv_caption   = mv_caption
                                                            iv_cnt_name  = mv_cnt_name
                                                            iv_style     = mv_style
                                                            iv_lifetime  = mv_lifetime ).

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_catched).
        DATA(lx_error) =
              CAST zcx_ca_scr_enhancement_by_dc(
                       zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_scr_enhancement_by_dc=>c_zcx_ca_scr_enhancement_by_dc
                                      iv_class    = 'ZCL_CA_CFW_UTIL'
                                      iv_method   = 'CREATE_DOCKING_CONTAINER'
                                      ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_docking_container


  METHOD create_n_display_docking ##needed.
    "-----------------------------------------------------------------*
    "   Create the docking container and display. Call BEFORE the
    "   SUPER->METHOD in any case!!
    "-----------------------------------------------------------------*
    mv_side      = iv_side.
    mv_ratio     = iv_ratio.
    mv_extension = iv_extension.
    mv_metric    = iv_metric.
    mv_style     = iv_style.
    mv_lifetime  = iv_lifetime.
    mv_caption   = iv_caption.
    mv_cnt_name  = iv_cnt_name.

    get_docking_cnt_from_buffer( ).

    "h a s   t o   b e   r e d e f i n e d   f r o m   h  e r e
  ENDMETHOD.                    "create_n_display_docking


  METHOD determine_ctrl_from_dc_childs.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    LOOP AT mr_container_buffer->o_container->children INTO ro_control.
      IF NOT line_exists( mt_control_buffer[ KEY by_control
                                                 COMPONENTS o_control = ro_control ] ).
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "determine_ctrl_from_dc_childs


  METHOD calculate_ratio_of_free_space.
    "-----------------------------------------------------------------*
    "   Calculate ratio of free space as proposal
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_scr_field_list    TYPE STANDARD TABLE OF d021s.

    "This FM provides also the values of the fields 'occupied rows/columns' of the screen painter
    CALL FUNCTION 'RS_SCRP_GET_SCREEN_INFOS'
      EXPORTING
        dynnr                 = CONV sychar04( ms_actual_screen-dynpro )
        progname              = ms_actual_screen-progname
      TABLES
        fieldlist             = lt_scr_field_list
      EXCEPTIONS
        dynpro_does_not_exist = 1
        no_field_list         = 2
        cancelled             = 3
        OTHERS                = 4.
    IF sy-subrc NE 0.
      IF sy-scols IS NOT INITIAL.
        mv_used_scr_columns = sy-scols / 2.
        mv_used_scr_rows   = sy-srows / 2.
      ENDIF.

    ELSE.
      IF sy-scols IS INITIAL.
        mv_calc_ratio_at_the_bottom = 50.
        mv_calc_ratio_on_the_right  = 50.

      ELSE.
        LOOP AT lt_scr_field_list REFERENCE INTO DATA(lr_scr_field)
                                  WHERE ltyp NE 'O' ##no_text.    "except OK code field
          DATA(lv_farest_right) = lr_scr_field->coln + lr_scr_field->leng.
          IF mv_used_scr_columns LT lv_farest_right.
            mv_used_scr_columns = lv_farest_right.
          ENDIF.

          "Column LINE is only the start line, while DIDX is the height (!! as a HEX value !!) of a subscreen
          CASE lr_scr_field->fill.
            WHEN 'B' ##no_text.     "Subscreen
              mv_used_scr_rows += lr_scr_field->didx.

            WHEN ' '.
              IF mv_used_scr_rows LT lr_scr_field->line.
                mv_used_scr_rows = lr_scr_field->line.
              ENDIF.
          ENDCASE.
        ENDLOOP.
      ENDIF.

      DATA(lv_possible_rows) = sy-srows - 5.
      IF lv_possible_rows LE mv_used_scr_rows.
        mv_used_scr_rows = lv_possible_rows.
      ENDIF.
      "                                           Plus one line buffer for the sash bar
      mv_calc_ratio_at_the_bottom = 100 - ( ( ( mv_used_scr_rows + 1 ) * 100 ) / sy-srows ).
      "Use at least 15 percent
      IF mv_calc_ratio_at_the_bottom NOT BETWEEN 15 AND 85.
        mv_calc_ratio_at_the_bottom = 15.
      ENDIF.

      "                                           Plus one line buffer for the sash bar
      mv_calc_ratio_on_the_right  = 100 - ( ( ( mv_used_scr_columns + 1 ) * 100 ) / sy-scols ).
      "Use at least 15 percent
      IF mv_calc_ratio_on_the_right NOT BETWEEN 15 AND 85.
        mv_calc_ratio_on_the_right = 15.
      ENDIF.
    ENDIF.

*    IF cl_gui_control=>gui_is_running EQ abap_false OR
*       cl_gui_control=>www_active     EQ abap_true.
*      mv_calc_ratio_at_the_bottom = mv_calc_ratio_at_the_bottom * 2.
*      mv_calc_ratio_on_the_right  = mv_calc_ratio_on_the_right  * 2.
*    ENDIF.
  ENDMETHOD.                    "calculate_ratio_of_free_space


  METHOD initialize_buffers.
    "-----------------------------------------------------------------*
    "   Initialize buffers
    "-----------------------------------------------------------------*
    LOOP AT zcl_ca_scr_enhancement_by_dc=>mt_container_buffer REFERENCE INTO DATA(lr_docking_cnt_buffer).
      lr_docking_cnt_buffer->o_container->free( ).
    ENDLOOP.

    CLEAR: zcl_ca_scr_enhancement_by_dc=>mt_container_buffer,
           zcl_ca_scr_enhancement_by_dc=>mt_control_buffer.
  ENDMETHOD.                    "initialize_buffers


  METHOD add_docking_cnt_to_buffer.
    "-----------------------------------------------------------------*
    "   Add docking container to buffer for the current screen
    "-----------------------------------------------------------------*
    "Screen not found -> create a new entry
    INSERT VALUE #( s_screen_name = ms_actual_screen )
                                        INTO TABLE zcl_ca_scr_enhancement_by_dc=>mt_container_buffer
                                        REFERENCE INTO mr_container_buffer.

    IF mv_create_dc_per_screen EQ cs_usage-same_dc_for_each_screen.
      "Lookup for an existing docking container instance. Since the new screen number can be the first entry,
      "but has still no container, search per LOOP and provide any screen with the same docking container.
      LOOP AT zcl_ca_scr_enhancement_by_dc=>mt_container_buffer REFERENCE INTO DATA(lo_docking_cnt_buffer)
                                                                  WHERE o_container IS BOUND.
        mr_container_buffer->o_container = lo_docking_cnt_buffer->o_container.
        EXIT.
      ENDLOOP.
    ENDIF.

    "If still no docking container is available, create it
    IF mr_container_buffer->o_container IS INITIAL.
      "Create a new docking container
      mv_docking_cnt_is_new = abap_true.
      mr_container_buffer->o_container = create_docking_container( ).
    ENDIF.
  ENDMETHOD.                    "add_docking_cnt_to_buffer


  METHOD create_n_display_dialogbox ##needed.
    "-----------------------------------------------------------------*
    "   Create the docking container and display. Call BEFORE the
    "   SUPER->METHOD in any case!!
    "-----------------------------------------------------------------*
    mv_width     = iv_width.
    mv_height    = iv_height.
    mv_top       = iv_top.
    mv_left      = iv_left.
    mv_style     = iv_style.
    mv_lifetime  = iv_lifetime.
    mv_caption   = iv_caption.
    mv_cnt_name  = iv_cnt_name.

    get_docking_cnt_from_buffer( ).

    "h a s   t o   b e   r e d e f i n e d   f r o m   h  e r e
  ENDMETHOD.                    "create_n_display_dialogbox


  METHOD connect_docking_cnt_to_screen.
    "-----------------------------------------------------------------*
    "   In the case of using one docking container for several screens
    "   it is enough to create one entry in the buffer.
    "-----------------------------------------------------------------*
    "Create only at the first time a docking container
    create_docking_cnt_per_screen( ).

    IF mv_screen_has_changed EQ abap_true.
      "Connect the already existing docking container to the new/other screen.
      zcl_ca_cfw_util=>link( io_from_container = mr_container_buffer->o_container
                             iv_prog           = ms_actual_screen-progname
                             iv_dynnr          = CONV #( ms_actual_screen-dynpro ) ).
    ENDIF.
  ENDMETHOD.                    "connect_docking_cnt_to_screen


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    IF cl_gui_object=>gui_is_running EQ abap_false OR
       cl_gui_object=>activex        EQ abap_false OR
       cl_gui_object=>catt_activ     EQ abap_true  OR
       sy-batch                      EQ abap_true  OR
       sy-binpt                      EQ abap_true.
      "Environment or current process type is not proper for use of controls
      RAISE EXCEPTION TYPE zcx_ca_scr_enhancement_by_dc
        EXPORTING
          textid = zcx_ca_scr_enhancement_by_dc=>no_proper_environment.
    ENDIF.

    mo_sel_options  = zcl_ca_c_sel_options=>get_instance( ).
    mo_scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).

    mv_create_dc_per_screen = iv_create_dc_per_screen.

    ms_actual_screen = is_actual_screen.
    IF ms_actual_screen IS INITIAL.
      ms_actual_screen = mo_scr_fld_attr->get_actual_screen( ).
    ENDIF.

    mv_screen_has_changed = abap_false.
    IF zcl_ca_scr_enhancement_by_dc=>ms_last_screen NE ms_actual_screen AND
       zcl_ca_scr_enhancement_by_dc=>ms_last_screen IS NOT INITIAL.
      mv_screen_has_changed = abap_true.
    ENDIF.
    zcl_ca_scr_enhancement_by_dc=>ms_last_screen = ms_actual_screen.
  ENDMETHOD.                    "constructor


  METHOD create_dialogbox_container.
    "---------------------------------------------------------------------*
    "     Creates the docking container. If the parameters of the
    "     constructor are not enough redefine this method and provide
    "     the values for your needs.
    "---------------------------------------------------------------------*
    TRY.
        ro_dialogbox_cnt = zcl_ca_cfw_util=>create_dialogbox_container(
*                           io_parent   =
                                                            iv_repid     = ms_actual_screen-progname
                                                            iv_dynnr     = CONV #( ms_actual_screen-dynpro )
*                                                            iv_width     =
*                                                            iv_height    =
*                                                            iv_top       =
*                                                            iv_left      =
                                                            iv_metric    = mv_metric
                                                            iv_caption   = mv_caption
                                                            iv_cnt_name  = mv_cnt_name
                                                            iv_style     = mv_style
                                                            iv_lifetime  = mv_lifetime ).

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_catched).
        DATA(lx_error) =
              CAST zcx_ca_scr_enhancement_by_dc(
                       zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_scr_enhancement_by_dc=>c_zcx_ca_scr_enhancement_by_dc
                                      iv_class    = 'ZCL_CA_CFW_UTIL'
                                      iv_method   = 'CREATE_DIALOGBOX_CONTAINER'
                                      ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_dialogbox_container


  METHOD create_docking_cnt_per_screen.
    "---------------------------------------------------------------------*
    "     Description
    "---------------------------------------------------------------------*
    CLEAR mr_container_buffer.
    mr_container_buffer = REF #( zcl_ca_scr_enhancement_by_dc=>mt_container_buffer[
                                                  KEY primary_key
                                                  COMPONENTS s_screen_name = ms_actual_screen ] OPTIONAL ).

    IF mr_container_buffer IS NOT BOUND.
      add_docking_cnt_to_buffer( ).

    ELSE.
      hide_other_docking_cnts( ).
      reconnect_docking_cnt_2_screen( ).
    ENDIF.
  ENDMETHOD.                    "create_docking_cnt_per_screen


  METHOD get_control_from_buffer.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    "At this point the LPORB must be filled
    IF is_lporb IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_scr_enhancement_by_dc
        EXPORTING
          textid   = zcx_ca_scr_enhancement_by_dc=>param_invalid
          mv_msgv1 = 'IS_LPORB'
          mv_msgv2 = 'SPACE' ##no_text.
    ENDIF.

    rr_control_buffer = REF #( zcl_ca_scr_enhancement_by_dc=>mt_control_buffer[ s_lporb = is_lporb ] OPTIONAL ).
    IF rr_control_buffer IS NOT BOUND.
      INSERT VALUE #( s_lporb = is_lporb
                      o_parent_container = mr_container_buffer->o_container )
                                                   INTO TABLE zcl_ca_scr_enhancement_by_dc=>mt_control_buffer
                                                   REFERENCE INTO rr_control_buffer.
    ENDIF.
  ENDMETHOD.                    "get_control_from_buffer


  METHOD get_docking_cnt_from_buffer.
    "-----------------------------------------------------------------*
    "   description
    "-----------------------------------------------------------------*
    TRY.
        "Initialize for later comparison
        mv_docking_cnt_is_new = abap_false.

        CASE mv_create_dc_per_screen.
          WHEN cs_usage-same_dc_for_each_screen.
            connect_docking_cnt_to_screen( ).

          WHEN cs_usage-new_dc_for_each_screen.
            create_docking_cnt_per_screen( ).
        ENDCASE.

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_scr_enhancement_by_dc( zcx_ca_error=>create_exception(
                                            iv_excp_cls = zcx_ca_scr_enhancement_by_dc=>c_zcx_ca_scr_enhancement_by_dc
                                            iv_class    = 'ZCL_CA_SCR_ENHANCEMENT_BY_DC'
                                            iv_method   = 'GET_DOCKING_CNT_FROM_BUFFER'
                                            ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_docking_cnt_from_buffer


  METHOD hide_other_docking_cnts.
    "-----------------------------------------------------------------*
    "   Hide other/recently displayed docking containers
    "-----------------------------------------------------------------*
    IF mv_create_dc_per_screen EQ cs_usage-same_dc_for_each_screen.
      "Only necessary if it is used with different docking container for each screen
      RETURN.
    ENDIF.

    "Hide any other docking container and its children
    LOOP AT zcl_ca_scr_enhancement_by_dc=>mt_container_buffer
                                              REFERENCE INTO DATA(lr_docking_cnt_buffer)
                                              WHERE s_screen_name NE mr_container_buffer->s_screen_name.
      set_control_n_childs_visibilty( io_control    = lr_docking_cnt_buffer->o_container
                                      iv_is_visible = abap_false ).
    ENDLOOP.
  ENDMETHOD.                    "hide_other_docking_cnts


  METHOD is_relevant_to_display.
    "-----------------------------------------------------------------*
    "   Check, if it is currently relevant to enhance the screen and
    "   display the docking container. Call SUPER->METHOD in any case!!
    "-----------------------------------------------------------------*
    rv_is_relevant = abap_false.

    "h a s   t o   b e   r e d e f i n e d   f r o m   h  e r e
  ENDMETHOD.                    "is_relevant_to_display


  METHOD reconnect_docking_cnt_2_screen.
    "-----------------------------------------------------------------*
    "   Recreate connection to screen if it is lost
    "-----------------------------------------------------------------*
    DATA(ls_link_info) = mr_container_buffer->o_container->get_link_info( ).
    IF ls_link_info-program NE mr_container_buffer->progname OR
       ls_link_info-dynnr   NE mr_container_buffer->dynpro.
      "Since connection of the docking container to the screen is lost after the dialog was closed,
      "the connection must be recreated here.
      zcl_ca_cfw_util=>link( io_from_container = mr_container_buffer->o_container
                             iv_prog           = mr_container_buffer->progname
                             iv_dynnr          = CONV #( mr_container_buffer->dynpro ) ).
    ENDIF.

    "This method is only called if an entry was found in the buffer, but make only docking container
    "visible. The children are the job of the consumer class.
    zcl_ca_cfw_util=>set_visible( io_control = mr_container_buffer->o_container ).
  ENDMETHOD.                    "reconnect_docking_cnt_2_screen
ENDCLASS.
