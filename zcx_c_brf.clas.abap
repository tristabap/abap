class ZCX_C_BRF definition
  public
  inheriting from ZCX_C_EXCEPTION
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !GO_MESSAGE_LOG type ref to ZIF_C_BAPI_LOG optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_C_BRF IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
GO_MESSAGE_LOG = GO_MESSAGE_LOG
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
