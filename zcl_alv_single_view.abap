class ZCL_ALV_SINGLE_VIEW definition
  public
  abstract
  create public .

public section.

  constants:
    BEGIN OF c_funcao,
        editar TYPE salv_de_function VALUE '&EDITAR',
        salvar TYPE salv_de_function VALUE '&SALVAR',
      END OF c_funcao .
  constants:
    BEGIN OF c_icone_funcao,
        editar TYPE string VALUE '@0Z@',
        salvar TYPE string VALUE '@2L@',
      END OF c_icone_funcao .
  constants:
    BEGIN OF c_tooltip_funcao,
        editar TYPE string VALUE 'Editar',
        salvar TYPE string VALUE 'Salvar',
      END OF c_tooltip_funcao .

  methods SHOW
    importing
      !IV_TABLE_TYPE type STRING optional
    changing
      !T_TABLE type TABLE
      !T_DETAIL type TABLE optional .
  methods CONSTRUCTOR
    importing
      !R_HOTSPOT_COLUMNS type TABLE optional
      !HEADER_TITLE type ANY optional
      !HEADER_SUBTITLE type ANY optional
      !LAYOUT type SLIS_VARI optional
      !ENABLE_EDIT type XFELD optional
      !EDIT_MODE type XFELD optional .
protected section.

  data O_TABLE type ref to CL_SALV_TABLE .
  data O_SALV_FUNCTIONS type ref to CL_SALV_FUNCTIONS_LIST .
  data V_TABLE_TYPE type STRING .

  methods CHANGE_COLUMN_NAME .
  methods ON_LINK_CLICK
    for event LINK_CLICK of CL_SALV_EVENTS_TABLE
    importing
      !ROW
      !COLUMN .
  methods ON_ADDED_FUNCTIONS_PRESSED
    for event ADDED_FUNCTION of CL_SALV_EVENTS_TABLE
    importing
      !E_SALV_FUNCTION .
  methods LOAD_TOOLBAR .
private section.

  data:
    r_hotspot_columns TYPE RANGE OF lvc_fname .
  data V_HEADER_TITLE type ITEX132 .
  data V_HEADER_SUBTITLE type ITEX132 .
  data O_LAYOUT type ref to CL_SALV_LAYOUT .
  data V_LAYOUT type SLIS_VARI .
  data O_CONTAINER type ref to CL_GUI_CONTAINER .
  data O_SPLITTER type ref to CL_GUI_SPLITTER_CONTAINER .
  data V_ENABLE_EDIT type XFELD .
  data V_EDIT_MODE type XFELD .

  methods SET_HOTSPOT .
  methods SET_EVENTS .
  methods SET_HEADER .
  methods SET_LAYOUT .
ENDCLASS.



CLASS ZCL_ALV_SINGLE_VIEW IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_SINGLE_VIEW->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] R_HOTSPOT_COLUMNS              TYPE        TABLE(optional)
* | [--->] HEADER_TITLE                   TYPE        ANY(optional)
* | [--->] HEADER_SUBTITLE                TYPE        ANY(optional)
* | [--->] LAYOUT                         TYPE        SLIS_VARI(optional)
* | [--->] ENABLE_EDIT                    TYPE        XFELD(optional)
* | [--->] EDIT_MODE                      TYPE        XFELD(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

    me->r_hotspot_columns = r_hotspot_columns[].
    me->v_header_title    = header_title.
    me->v_header_subtitle = header_subtitle.
    me->v_layout          = layout.
    me->v_enable_edit     = enable_edit.
    me->v_edit_mode       = edit_mode.

    IF edit_mode EQ abap_true.
      me->v_enable_edit = abap_true.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ALV_SINGLE_VIEW->LOAD_TOOLBAR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD load_toolbar.

    TRY.

        IF me->o_table IS BOUND.

          me->o_salv_functions = me->o_table->get_functions( ).
          me->o_salv_functions->set_all( ).

        ENDIF.

      CATCH cx_salv_msg.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ALV_SINGLE_VIEW->ON_ADDED_FUNCTIONS_PRESSED
* +-------------------------------------------------------------------------------------------------+
* | [--->] E_SALV_FUNCTION                LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_added_functions_pressed.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ALV_SINGLE_VIEW->ON_LINK_CLICK
* +-------------------------------------------------------------------------------------------------+
* | [--->] ROW                            LIKE
* | [--->] COLUMN                         LIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD on_link_click.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_SINGLE_VIEW->SET_EVENTS
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_events.

    DATA:
      o_events TYPE REF TO cl_salv_events_table.

    IF me->o_table IS BOUND.

      o_events ?= me->o_table->if_salv_gui_om_table_info~get_event( ).

      SET HANDLER me->on_link_click FOR o_events.
      SET HANDLER me->on_added_functions_pressed FOR o_events .

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_SINGLE_VIEW->SET_HEADER
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_header.

    DATA:
      o_top_element TYPE REF TO cl_salv_form_layout_grid,
      o_header      TYPE REF TO cl_salv_form_header_info,
      o_action      TYPE REF TO cl_salv_form_action_info.

    IF me->v_header_title    IS NOT INITIAL OR
       me->v_header_subtitle IS NOT INITIAL.
      o_top_element = NEW #( columns = 2 ).
    ENDIF.

    IF o_top_element IS BOUND.

      o_header = o_top_element->create_header_information(
          row     = 1
          column  = 1
          text    = me->v_header_title
          tooltip = me->v_header_title ).

      o_action = o_top_element->create_action_information(
          row     = 2
          column  = 1
          text    = me->v_header_subtitle
          tooltip = me->v_header_subtitle ).

      o_table->if_salv_gui_om_table_info~set_top_of_list(
          value = o_top_element ).

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_SINGLE_VIEW->SET_HOTSPOT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_hotspot.

    DATA:
      o_columns TYPE REF TO cl_salv_columns_table,
      o_column  TYPE REF TO cl_salv_column_table.

    o_columns = me->o_table->get_columns( ).

    TRY.

        LOOP AT me->r_hotspot_columns ASSIGNING FIELD-SYMBOL(<fs_hotspot>).

          IF <fs_hotspot>-low IN r_hotspot_columns.

            o_column ?= o_columns->get_column( columnname = <fs_hotspot>-low ).
            o_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

          ENDIF.

        ENDLOOP.

      CATCH cx_salv_not_found.

    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ALV_SINGLE_VIEW->SET_LAYOUT
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_layout.

    DATA:
      wa_layout_key TYPE salv_s_layout_key.

    IF me->o_table IS BOUND.

      me->o_table->get_layout( )->set_save_restriction( value = if_salv_c_layout=>restrict_none ).

      me->o_layout = me->o_table->if_salv_gui_om_table_info~get_layout( ).

      IF me->o_layout IS BOUND.

        wa_layout_key-report = sy-cprog.

        me->o_layout->set_key( value = wa_layout_key ).

        IF me->v_layout IS NOT INITIAL.
          o_layout->set_initial_layout( value = me->v_layout ).
        ENDIF.

      ENDIF.

      DATA(o_display) = me->o_table->get_display_settings( ).
      me->o_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
      o_display->set_striped_pattern( if_salv_c_bool_sap=>true ).
      me->o_table->get_columns( )->set_optimize( abap_true ).

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ALV_SINGLE_VIEW->SHOW
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_TABLE_TYPE                  TYPE        STRING(optional)
* | [<-->] T_TABLE                        TYPE        TABLE
* | [<-->] T_DETAIL                       TYPE        TABLE(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show.

    me->v_table_type = iv_table_type.

    cl_salv_table=>factory( IMPORTING r_salv_table = me->o_table
                            CHANGING  t_table      = t_table ).

    me->set_layout( ).

    me->set_header( ).

    me->load_toolbar( ).

    me->set_hotspot( ).

    me->set_events( ).

    me->change_column_name( ).

    me->o_table->display( ).

    WRITE: space.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ALV_SINGLE_VIEW->CHANGE_COLUMN_NAME
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD change_column_name.
  ENDMETHOD.
ENDCLASS.
