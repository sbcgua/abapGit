class ZCX_ABAPGIT_HTTP_ERROR definition
  public
  inheriting from ZCX_ABAPGIT_EXCEPTION
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  class-methods RAISE_HTTP
    importing
      !IV_TEXT type CLIKE
      !IX_PREVIOUS type ref to CX_ROOT optional
    raising
      ZCX_ABAPGIT_HTTP_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_ABAPGIT_HTTP_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


  method RAISE_HTTP.

    data lo_exception type ref to zcx_abapgit_http_error.

    lo_exception ?= create(
      iv_exception_class_name = 'ZCX_ABAPGIT_HTTP_ERROR'
      iv_text                 = iv_text
      ix_previous             = ix_previous ).

    RAISE EXCEPTION lo_exception.

  endmethod.
ENDCLASS.
