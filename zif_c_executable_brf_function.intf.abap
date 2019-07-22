interface ZIF_C_EXECUTABLE_BRF_FUNCTION
  public .


  types:
    BEGIN OF ty_s_parameter,
      name            TYPE  string,
      value_reference TYPE REF TO	data,
    END OF ty_s_parameter .
  types:
    ty_t_parameters TYPE TABLE OF ty_s_parameter .

  methods EXECUTE
    importing
      !IT_PARAMETERS type TY_T_PARAMETERS
    returning
      value(RO_RESULT) type ref to DATA
    raising
      ZCX_C_BRF
      CX_FDT_NO_RESULT .
endinterface.
