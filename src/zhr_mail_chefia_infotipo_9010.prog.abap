*&---------------------------------------------------------------------*
*& Report ZHR_MAIL_CHEFIA_INFOTIPO_9010
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zhr_mail_chefia_infotipo_9010.

TABLES: pernr.

TYPES: BEGIN OF ty_out,
         status TYPE icon_d.
         INCLUDE STRUCTURE p9010.
         TYPES: msg    TYPE bapi_msg,
       END OF ty_out.

DATA: gt_out    TYPE TABLE OF ty_out,
      gs_out    LIKE LINE OF gt_out,
      gt_pa9010 TYPE TABLE OF pa9010,
      gs_pa9010 LIKE LINE OF gt_pa9010,
      gs_cc     TYPE zhr_centro_custo.


PARAMETERS: pa_teste AS CHECKBOX DEFAULT 'X'.


START-OF-SELECTION.

  REFRESH: gt_out.
  pn-begda = pn-endda = pn-begps = pn-endps = sy-datum.
  pnpbegda = pnpendda = pnpbegps = pnpendps = sy-datum.

GET pernr.

  PERFORM get_data.


end-of-SELECTION.

  IF pa_teste IS INITIAL.
    PERFORM update_it9010.
  ENDIF.
  PERFORM display_output.



FORM update_it9010.

  DATA: ls_return TYPE bapireturn1,
        ls_out    TYPE p9010.

  LOOP AT gt_out ASSIGNING FIELD-SYMBOL(<fs_out>).
    CLEAR: ls_return.
    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = <fs_out>-pernr
      IMPORTING
        return = ls_return.

    IF ls_return-id EQ space.
      CLEAR: ls_return.
      MOVE-CORRESPONDING <fs_out> TO ls_out.
      ls_out-infty = '9010'.
      CALL FUNCTION 'HR_INFOTYPE_OPERATION'
        EXPORTING
          infty         = ls_out-infty
          number        = ls_out-pernr
          subtype       = ls_out-subty
          validityend   = ls_out-endda
          validitybegin = ls_out-begda
          record        = ls_out
          operation     = 'INS'
          dialog_mode   = '0'
        IMPORTING
          return        = ls_return.

      IF ls_return-type NE 'S'
     AND ls_return-type IS NOT INITIAL.
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
            INTO <fs_out>-msg
            WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
        <fs_out>-status = icon_red_light.
      ELSE.
        <fs_out>-msg = 'Registo criado com sucesso'.
        <fs_out>-status = icon_green_light.
      ENDIF.

      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = <fs_out>-pernr
        IMPORTING
          return = ls_return.
    ELSE.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
          INTO <fs_out>-msg
          WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      <fs_out>-status = icon_red_light.
    ENDIF.
  ENDLOOP.

ENDFORM.
FORM display_output.

  DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
        lo_col_tab  TYPE REF TO cl_salv_column_table,
        lo_events   TYPE REF TO cl_salv_events_table.

  TRY.
      cl_salv_table=>factory(  EXPORTING list_display = sy-batch
                               IMPORTING r_salv_table = DATA(lo_tab)
                               CHANGING  t_table      = gt_out ).

      lo_tab->get_functions( )->set_all( ).
      lo_tab->get_columns( )->set_optimize( ).

      DATA(lo_columns) = lo_tab->get_columns( ).

      DATA(lo_column) = lo_columns->get_column( columnname = 'STATUS' ).
      lo_column->set_short_text( 'Status' ).

      lo_column = lo_columns->get_column( columnname = 'INFTY' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'SUBTY' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'OBJPS' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'SPRPS' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'SEQNR' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).


      lo_column = lo_columns->get_column( columnname = 'AEDTM' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'UNAME' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'HISTO' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'ITXEX' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'REFEX' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'ORDEX' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'ITBLD' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'PREAS' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'FLAG1' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'FLAG2' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'FLAG3' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'FLAG4' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'RESE1' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'RESE2' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'GRPVL' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'EMAIL' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'TITULO_TRABALHO' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'SUBAREA' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'EQUIPA' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'ATIVO' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
      lo_column = lo_columns->get_column( columnname = 'IDWORKVIVO' ).
      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).

*      lo_column = lo_columns->get_column( columnname = 'NOME_CHEFIA' ).
*      lo_column->set_visible( value = if_salv_c_bool_sap=>false ).
*      lo_column->set_short_text( 'NmChefia' ).
*      lo_column->set_medium_text( 'Nome Chefia' ).
*      lo_column->set_long_text( 'Nome Chefia' ).

      lo_events = lo_tab->get_event( ).
      lo_tab->display( ).
    CATCH cx_root.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  CLEAR: gs_pa9010, gs_out.
  SELECT * INTO gs_pa9010
           FROM pa9010
           WHERE pernr EQ pernr-pernr
             AND begda LE sy-datum
             AND endda GE sy-datum.
  ENDSELECT.
  IF sy-subrc NE 0.
    SELECT * INTO gs_pa9010
             FROM pa9010
             WHERE pernr EQ pernr-pernr.
    ENDSELECT.
  ENDIF.
  IF gs_pa9010 IS NOT INITIAL.
    CLEAR: gs_cc.
    SELECT * INTO gs_cc
             FROM zhr_centro_custo
             WHERE kostl EQ gs_pa9010-centro_cst
               AND begda LE gs_pa9010-endda
               AND endda GE gs_pa9010-begda.
    ENDSELECT.
    IF sy-subrc EQ 0.
* >>> INI Inetum SAM EMP/SS HR 7000235619 01.07.2025
* Comentado
*      IF gs_cc-mail NE gs_pa9010-idchefia.

      IF gs_cc-mail NE gs_pa9010-idchefia
     AND gs_cc-mail NE gs_pa9010-email.
* <<< END Inetum SAM EMP/SS HR 7000235619 01.07.2025

* Criar novo registo no IT9010
        MOVE-CORRESPONDING gs_pa9010 TO gs_out.
        gs_out-status   = icon_light_out.
        gs_out-idchefia = gs_cc-mail.
        gs_out-begda    = gs_cc-begda.
        gs_out-endda    = gs_cc-endda.
        APPEND gs_out TO gt_out.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
