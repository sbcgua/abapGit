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

    DATA: lv_name        TYPE ddobjname,
          lv_rc          TYPE sy-subrc,
          lv_langu       TYPE langu,
          ls_dd01v       TYPE dd01v,
          lt_dd01v       TYPE TABLE OF dd01v,
          lt_dd07v       TYPE TABLE OF dd07v,
          lt_dd01v_langu TYPE TABLE OF langu,
          lt_dd07v_langu TYPE TABLE OF langu.

    lv_name = ms_item-obj_name.

    SELECT DISTINCT ddlanguage as langu INTO TABLE lt_dd01v_langu
      FROM dd01v
     WHERE domname = lv_name.

    LOOP AT lt_dd01v_langu INTO lv_langu.
      CLEAR ls_dd01v.
      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = lv_name
          langu         = lv_langu
        IMPORTING
          dd01v_wa      = ls_dd01v
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from DDIF_DOMA_GET' ).
      ENDIF.
      APPEND ls_dd01v TO lt_dd01v.
    ENDLOOP.
    IF lines( lt_dd01v ) IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    SELECT DISTINCT ddlanguage as langu INTO TABLE lt_dd07v_langu
      FROM dd07v
     WHERE domname = lv_name.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_name
        text           = 'T'  "texts only
        langu          = '*'  "all languages
      IMPORTING
        rc             = lv_rc
      TABLES
        dd07v_tab      = lt_dd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from DD_DOMVALUES_GET' ).
    ENDIF.

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time.

    io_xml->add( iv_name = 'DD01V_TAB'
                 ig_data = lt_dd01v ).

    io_xml->add( iv_name = 'DD07V_LANGU'
                 ig_data = lt_dd07v_langu ).

    io_xml->add( iv_name = 'DD07V_TAB'
                 ig_data = lt_dd07v ).

  ENDMETHOD.                    "serialize

  METHOD lif_object~deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?
    DATA: lv_name         TYPE ddobjname,
          lv_langu        TYPE langu,
          lv_inst_langu   TYPE char40,
          ls_dd01v        TYPE dd01v,
          lt_dd01v        TYPE TABLE OF dd01v,
          lt_dd07v        TYPE TABLE OF dd07v,
          lt_dd07v_langu  TYPE TABLE OF langu,
          lt_dd07v_tmp    TYPE TABLE OF dd07v.

    io_xml->read( EXPORTING iv_name = 'DD01V_TAB'
                  CHANGING cg_data = lt_dd01v ).

    io_xml->read( EXPORTING iv_name = 'DD07V_LANGU'
                  CHANGING cg_data = lt_dd07v_langu ).

    io_xml->read( EXPORTING iv_name = 'DD07V_TAB'
                  CHANGING cg_data = lt_dd07v ).

    corr_insert( iv_package ).
    "import only installed languages - TODO: move to lcl_git_utils?
     CALL FUNCTION 'RSAQ_READ_INSTALLED_LANGUAGES'
       IMPORTING
         inst_languages = lv_inst_langu.

    lv_name = ms_item-obj_name. " type conversion
    LOOP AT lt_dd01v INTO ls_dd01v where ddlanguage CA lv_inst_langu.
      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = lv_name
          dd01v_wa          = ls_dd01v
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

    LOOP AT lt_dd07v_langu INTO lv_langu  where table_line ca lv_inst_langu.
      lt_dd07v_tmp = lt_dd07v.
      DELETE lt_dd07v_tmp WHERE ddlanguage NE lv_langu.
      CALL FUNCTION 'DD_DOFV_PUT'
        EXPORTING
          domain_name = lv_name
        TABLES
           dd07v_tab  = lt_dd07v_tmp
        EXCEPTIONS
           OTHERS     = 01.
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from DD_DOFV_PUT' ).
      ENDIF.
    ENDLOOP.

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_doma IMPLEMENTATION