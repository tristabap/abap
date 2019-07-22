class ZCL_C_BRF_FUNCTION definition
  public
  final
  create private .

public section.

  interfaces ZIF_C_EXECUTABLE_BRF_FUNCTION .

  class-methods GET_WITH_NAME
    importing
      !IV_APPLICATION type IF_FDT_TYPES=>NAME
      !IV_FUNCTION type IF_FDT_TYPES=>NAME
    returning
      value(RO_INSTANCE) type ref to ZIF_C_EXECUTABLE_BRF_FUNCTION
    raising
      ZCX_C_BRF .
  methods CONSTRUCTOR
    importing
      !IV_FUNCTION type IF_FDT_TYPES=>NAME
      !IV_APPLICATION type IF_FDT_TYPES=>NAME
    raising
      ZCX_C_BRF .
  PRIVATE SECTION.

    DATA _v_application TYPE if_fdt_types=>name .
    DATA _v_function TYPE if_fdt_types=>name .
    DATA _o_factory TYPE REF TO if_fdt_factory .
    DATA _o_query TYPE REF TO if_fdt_query .
    DATA _o_application TYPE REF TO if_fdt_application .
    DATA _o_function TYPE REF TO if_fdt_function .

    METHODS set_application
      RAISING
        zcx_c_brf .
    METHODS set_function
      RAISING
        zcx_c_brf
        cx_fdt_input .
ENDCLASS.



CLASS ZCL_C_BRF_FUNCTION IMPLEMENTATION.


  METHOD constructor.

    TRY.
        _o_factory = cl_fdt_factory=>if_fdt_factory~get_instance( ).
        _o_query   = _o_factory->get_query( ).

        _v_application = iv_application.
        _v_function    = iv_function.

        " Set the application and the function instances
        set_application( ).
        set_function( ).

      CATCH cx_fdt_input INTO DATA(lo_x_fdt).
        " System error on BRF (appl. &1, func &2)
        MESSAGE e013(zc) WITH iv_application iv_function INTO DATA(lv_message_text).
        RAISE EXCEPTION TYPE zcx_c_brf
          EXPORTING
            previous       = lo_x_fdt
            go_message_log = NEW zcl_c_bapi_logger( )->zif_c_bapi_log~add_from_current_system_msg( ).
    ENDTRY.

  ENDMETHOD.


  METHOD GET_WITH_NAME.

    ro_instance ?= NEW zcl_c_brf_function( iv_application = iv_application
                                            iv_function    = iv_function ).

  ENDMETHOD.


  METHOD set_application.
    "== Set the private BRF application object using the application name.

    DATA lt_appl_ids TYPE if_fdt_types=>ts_object_id.

    "== Get the Application ID using Query
    _o_query->get_ids(
      EXPORTING
        iv_name        = _v_application
        iv_object_type = if_fdt_constants=>gc_object_type_application
      IMPORTING
        ets_object_id  = lt_appl_ids ).

    READ TABLE lt_appl_ids ASSIGNING FIELD-SYMBOL(<lv_appl_id>) INDEX 1.
    IF sy-subrc EQ 0.
      "== Get the application Object using the factory
      _o_application = _o_factory->get_application( <lv_appl_id> ).

    ELSE.
      "== BRF application &1 was not found
      MESSAGE e011(zc) WITH _v_application INTO DATA(lv_message_text).

      RAISE EXCEPTION TYPE zcx_c_brf
        EXPORTING
          go_message_log = NEW zcl_c_bapi_logger( )->zif_c_bapi_log~add_from_current_system_msg( ).

    ENDIF.
  ENDMETHOD.


  METHOD set_function.
    "== Set the function instance by querying the application

    DATA lt_func_ids  TYPE if_fdt_types=>ts_object_id.
    DATA lv_func_id   TYPE if_fdt_types=>id.
    CLEAR _o_function.

    "== Get all the Function IDs for given function name
    _o_query->get_ids(
          EXPORTING
            iv_name        = _v_function
            iv_object_type = if_fdt_constants=>gc_object_type_function
          IMPORTING
            ets_object_id  = lt_func_ids ).

    "== Get the function id which belongs to the current application
    LOOP AT lt_func_ids INTO lv_func_id.

      _o_query->get_name(
        EXPORTING
          iv_id             = lv_func_id    " Universal Unique Identifier
        IMPORTING
          ev_name           = DATA(ev_name)
      ).

      DATA(lo_function) = _o_factory->get_function( lv_func_id ).

      IF lo_function->if_fdt_admin_data~get_application( ) EQ _o_application->mv_id.

        IF _o_function IS NOT INITIAL. "== more than one function found
          DATA(lv_multiple_functions) = abap_true.
          CLEAR _o_function.
          EXIT.
        ENDIF.

        _o_function = lo_function.

      ENDIF.
    ENDLOOP.

    CHECK _o_function IS INITIAL.

    IF lv_multiple_functions EQ abap_true.
      "== BRF application &1 has more than one funtion with the name &2
      MESSAGE e014(zc) WITH  _v_application _v_function INTO DATA(lv_message_text).
    ELSE.
      "== BRF function &1 not found in application &2
      MESSAGE e012(zc) WITH _v_function _v_application INTO lv_message_text.
    ENDIF.


    RAISE EXCEPTION TYPE zcx_c_brf
      EXPORTING
        go_message_log = NEW zcl_c_bapi_logger( )->zif_c_bapi_log~add_from_current_system_msg( ).

  ENDMETHOD.


  METHOD ZIF_C_EXECUTABLE_BRF_FUNCTION~EXECUTE.

    DATA  lo_result              TYPE REF TO if_fdt_result.

    TRY.

        "== Fill the parameters
        "=======================
        DATA(lo_context) = _o_function->get_process_context( ).
        LOOP AT it_parameters ASSIGNING FIELD-SYMBOL(<ls_parameter>).
          lo_context->set_value( iv_name  = <ls_parameter>-name
                                 ir_value = <ls_parameter>-value_reference ).
        ENDLOOP.

        "== Execute the rule
        "====================
        _o_function->process(
                              EXPORTING io_context = lo_context
                              IMPORTING eo_result  = lo_result ).

         lo_result->get_value( IMPORTING er_value = ro_result ).

        "== No Data
        "===========
      CATCH cx_fdt_no_result INTO DATA(lo_x_fdt_no_result).
        "== Propagate empty result errors
        "== Note : decision tables must be configured to return this error else - they return a blank.
        RAISE EXCEPTION lo_x_fdt_no_result.

        "== All other BRF+ exceptions
        "==============================
      CATCH cx_fdt INTO DATA(lo_x_fdt).
        " System error on BRF (appl. &1, func &2)
        MESSAGE e057(zc) WITH _v_application _v_function INTO DATA(lv_message_text).

        RAISE EXCEPTION TYPE zcx_c_brf
          EXPORTING
            previous       = lo_x_fdt
            go_message_log = NEW zcl_c_bapi_logger( )->zif_c_bapi_log~add_from_current_system_msg( ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
