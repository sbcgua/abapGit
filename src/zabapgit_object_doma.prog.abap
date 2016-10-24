*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_DOMA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_objects_super FINAL.
"todo translate
  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

ENDCLASS.                    "lcl_object_doma DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma IMPLEMENTATION.

  METHOD lif_object~has_changed_since.

    DATA: lv_date    TYPE dats,
          lv_time    TYPE tims,
          lv_ts      TYPE timestamp.

    SELECT SINGLE as4date as4time FROM dd01l
      INTO (lv_date, lv_time)
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers  = '0000'.

    _object_check_timestamp lv_date lv_time.

  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.

    SELECT SINGLE as4user FROM dd01l INTO rv_user
      WHERE domname = ms_item-obj_name
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

    DATA: lv_domname TYPE dd01l-domname.


    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = ms_item-obj_name
      AND as4local = 'A'
      AND as4vers = '0000'.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-DOMA'
               iv_field = 'RSRD1-DOMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~delete.
* see class CL_WB_DDIC

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'D'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_DD_DELETE_OBJ, DOMA' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~serialize.

    DATA: lv_name         TYPE ddobjname,
          lv_langu        TYPE langu,
          ls_dd01v        TYPE dd01v,
          ls_dd01v_mlangu TYPE dd01v,
          ls_dd01v_text   TYPE dd01v,
          lt_dd01v_texts  TYPE TABLE OF dd01v,
          lt_dd07v        TYPE TABLE OF dd07v,
          lt_dd07v_texts  TYPE TABLE OF dd07v,
          lt_dd07v_mlangu TYPE TABLE OF dd07v,
          lt_i18n_langs   TYPE TABLE OF langu.

    lv_name = ms_item-obj_name.

    SELECT DISTINCT ddlanguage as langu INTO TABLE lt_i18n_langs
      FROM dd01v
     WHERE domname = lv_name.

    LOOP AT lt_i18n_langs INTO lv_langu.
      CLEAR ls_dd01v_text.
      refresh lt_dd07v.
      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_name
          langu         = lv_langu
        IMPORTING
          dd01v_wa      = ls_dd01v
        TABLES
          dd07v_tab     = lt_dd07v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      CHECK ls_dd01v-ddlanguage IS NOT INITIAL.     "text doesn't exist
      DELETE lt_dd07v WHERE ddlanguage IS INITIAL.  "text doesn't exist
      CLEAR: ls_dd01v-as4user,
             ls_dd01v-as4date,
             ls_dd01v-as4time.
      IF lv_langu EQ mv_language.
        ls_dd01v_mlangu = ls_dd01v.
        APPEND LINES OF lt_dd07v TO lt_dd07v_mlangu.
      ELSE.
        APPEND ls_dd01v TO lt_dd01v_texts.
        APPEND LINES OF lt_dd07v TO lt_dd07v_texts.
      ENDIF.
    ENDLOOP.
    IF ls_dd01v_mlangu IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    io_xml->add( iv_name = 'DD01V'
                 ig_data = ls_dd01v_mlangu ).

    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v_mlangu ).

    check lines( lt_i18n_langs ) > 1.
    io_xml->add( iv_name = 'I18N_LANGS'
                 ig_data = lt_i18n_langs ).

    io_xml->add( iv_name = 'DD01V_TEXTS'
                 ig_data = lt_dd01v_texts ).

    io_xml->add( iv_name = 'DD07V_TEXTS'
                 ig_data = lt_dd07v_texts ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?
    DATA: lv_name         TYPE ddobjname,
          lv_langu        TYPE langu,
          ls_dd01v        TYPE dd01v,
          ls_dd01v_text   TYPE dd01v,
          lt_dd07v        TYPE TABLE OF dd07v,
          lt_dd07v_tmp    TYPE TABLE OF dd07v,
          lt_dd01v_texts  TYPE TABLE OF dd01v,
          lt_dd07v_texts  TYPE TABLE OF dd07v,
          lt_i18n_langs   TYPE TABLE OF langu.

    io_xml->read( EXPORTING iv_name = 'DD01V'
                  CHANGING  cg_data = ls_dd01v ).

    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING  cg_data = lt_dd07v ).

    io_xml->read( EXPORTING iv_name = 'I18N_LANGS'
                  CHANGING  cg_data = lt_i18n_langs ).

    io_xml->read( EXPORTING iv_name = 'DD01V_TEXTS'
                  CHANGING  cg_data = lt_dd01v_texts ).

    io_xml->read( EXPORTING iv_name = 'DD07V_TEXTS'
                  CHANGING  cg_data = lt_dd07v_texts ).

    corr_insert( iv_package ).

    lv_name = ms_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DOMA_PUT'   "Master language
      EXPORTING
        name              = lv_name
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DDIF_DOMA_PUT' ).
    ENDIF.

    LOOP AT lt_i18n_langs INTO lv_langu.
      CHECK lcl_objects=>is_langu_installed( lv_langu ) IS NOT INITIAL.
      READ TABLE lt_dd01v_texts INTO ls_dd01v_text WITH KEY ddlanguage = lv_langu.
      CHECK sy-subrc IS INITIAL.
      lt_dd07v_tmp = lt_dd07v_texts.
      DELETE lt_dd07v_tmp WHERE ddlanguage NE lv_langu.
      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = lv_name
          dd01v_wa          = ls_dd01v_text
        TABLES
          dd07v_tab         = lt_dd07v_tmp
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from DDIF_DOMA_PUT' ).
      ENDIF.
    ENDLOOP.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_doma IMPLEMENTATION