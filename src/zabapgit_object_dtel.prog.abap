*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_DTEL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    DATA: lv_date TYPE dats,
          lv_time TYPE tims,
          lv_ts   TYPE timestamp.

    SELECT SINGLE as4date as4time FROM dd04l
      INTO (lv_date, lv_time)
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    _object_check_timestamp lv_date lv_time.

  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    SELECT SINGLE as4user FROM dd04l INTO rv_user
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    DATA: lv_rollname TYPE dd04l-rollname.


    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE rollname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'E'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_DD_DELETE_OBJ, DTEL' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name        TYPE ddobjname,
          lv_langu       TYPE langu,
          ls_dd04v       TYPE dd04v,
          ls_dd04v_mlang TYPE dd04v,
          ls_dd04v_text  TYPE dd04v,
          ls_tpara       TYPE tpara,
          lt_dd04v_texts TYPE TABLE OF dd04v,
          lt_i18n_langs  TYPE TABLE OF langu.

    lv_name = ms_item-obj_name.

    SELECT DISTINCT ddlanguage as langu INTO TABLE lt_i18n_langs
      FROM dd04v
     WHERE rollname = lv_name.

    LOOP AT lt_i18n_langs INTO lv_langu.
      CLEAR ls_dd04v.
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = lv_name
          langu         = lv_langu
        IMPORTING
          dd04v_wa      = ls_dd04v_text
          tpara_wa      = ls_tpara
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'Error from DDIF_DTEL_GET' ).
      ENDIF.
      CLEAR: ls_dd04v_text-as4user,
             ls_dd04v_text-as4date,
             ls_dd04v_text-as4time.

      IF ls_dd04v_text-refkind = 'D'.
* clear values inherited from domain
        CLEAR: ls_dd04v_text-datatype,
               ls_dd04v_text-leng,
               ls_dd04v_text-decimals,
               ls_dd04v_text-outputlen,
               ls_dd04v_text-lowercase,
               ls_dd04v_text-signflag,
               ls_dd04v_text-convexit,
               ls_dd04v_text-entitytab.
      ENDIF.
      IF lv_langu NE mv_language.
        APPEND ls_dd04v_text TO lt_dd04v_texts.
      ELSE.
        ls_dd04v_mlang = ls_dd04v_text.
      ENDIF.
    ENDLOOP.
    IF ls_dd04v_mlang IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    io_xml->add( iv_name = 'DD04V'
                 ig_data = ls_dd04v_mlang ).

    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).

    check lines( lt_dd04v_texts ) is not initial.

    io_xml->add( iv_name = 'DD04V_TEXTS'
                 ig_data = lt_dd04v_texts ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

    DATA: lv_name        TYPE ddobjname,
          ls_dd04v       TYPE dd04v,
          lt_dd04v_texts TYPE TABLE OF dd04v.

    io_xml->read( EXPORTING iv_name = 'DD04V'
                  CHANGING  cg_data = ls_dd04v ).

    io_xml->read( EXPORTING iv_name = 'DD04V_TEXTS'
                  CHANGING  cg_data = lt_dd04v_texts ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion
    CALL FUNCTION 'DDIF_DTEL_PUT'  "Master language
      EXPORTING
        name              = lv_name
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DDIF_DTEL_PUT' ).
    ENDIF.

    LOOP AT lt_dd04v_texts INTO ls_dd04v.
      CHECK lcl_objects=>is_langu_installed( ls_dd04v-ddlanguage ) IS NOT INITIAL.
      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING
          name              = lv_name
          dd04v_wa          = ls_dd04v
        EXCEPTIONS
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from DDIF_DTEL_PUT' ).
      ENDIF.
    ENDLOOP.
    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_dtel IMPLEMENTATION