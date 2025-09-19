*----------------------------------------------------------------------*
***INCLUDE ZHCM_WFM_WAGES_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr.

  DATA: lv_uzeit TYPE sy-uzeit,
        lv_data  LIKE pa_data,
        lv_begda TYPE begda,
        lv_endda TYPE endda,
        lv_hours TYPE i.

  REFRESH: gt_sched, gt_p0015.
  IF sy-uzeit LE '040000'.
    lv_uzeit = '200000'.
    lv_data  = pa_data - 1.

    SELECT * INTO TABLE gt_sched
             FROM ztwfm_hcm_sched
             WHERE ( ( aedtm EQ lv_data AND uzeit BETWEEN lv_uzeit AND '235959' )
                OR   ( aedtm EQ pa_data AND uzeit BETWEEN '000000' AND sy-uzeit ) )
               AND pernr     IN so_pernr
               AND integrado EQ space.
  ELSE.
    lv_hours = 3600 + 3600 + 3600 + 3600.  " Só avaliar até 4 horas para trás que foi a hora da última integração
    lv_uzeit = sy-uzeit - lv_hours.        " Só avaliar até 4 horas para trás que foi a hora da última integração
    lv_uzeit+2(4) = '0000'.

    SELECT * INTO TABLE gt_sched
             FROM ztwfm_hcm_sched
             WHERE aedtm     EQ pa_data AND uzeit BETWEEN lv_uzeit AND sy-uzeit
               AND pernr     IN so_pernr
               AND integrado EQ space.
  ENDIF.

  LOOP AT gt_sched INTO DATA(gs_sched).
    lv_begda = gs_sched-begda.
    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_begda
      IMPORTING
        last_day_of_month = lv_endda
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT * APPENDING TABLE gt_p0015
             FROM pa0015
             WHERE pernr EQ gs_sched-pernr
               AND begda GE lv_begda
               AND endda LE lv_endda
               AND lgart EQ cv_lgart.
  ENDLOOP.
  SORT gt_p0015.
  DELETE ADJACENT DUPLICATES FROM gt_p0015.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REGISTER_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM register_days .

  DATA: lv_hire_date TYPE begda,
        lv_fire_date TYPE endda,
        lv_opera     TYPE actio,
        lt_p0000     TYPE TABLE OF p0000,
        lt_p0001     TYPE TABLE OF p0001,
        ls_sched     LIKE LINE OF gt_sched,
        p0015        TYPE p0015.

  DATA: lt_total_sched TYPE TABLE OF ztwfm_hcm_sched,
        ls_return      TYPE bapireturn1,
        ls_key         TYPE bapipakey,
        lv_begda       TYPE begda,
        lv_endda       TYPE endda,
        lv_total       TYPE p LENGTH 3 DECIMALS 1,
        lv_data        TYPE REF TO data.

  SELECT * INTO TABLE @DATA(lt_subtype)
           FROM ztwfm_hcm_subtyp
           WHERE subtype EQ @cv_lgart.

  CHECK sy-subrc = 0.

  CLEAR: lv_total.
  SORT gt_sched BY pernr ASCENDING begda ASCENDING.
  LOOP AT gt_sched INTO DATA(gs_sched).
    MOVE-CORRESPONDING gs_sched TO ls_sched.
    ADD ls_sched-anzhl TO lv_total.

    AT END OF pernr.
      APPEND INITIAL LINE TO lt_total_sched ASSIGNING FIELD-SYMBOL(<fs_total_sched>).
      <fs_total_sched>-pernr = ls_sched-pernr.
      <fs_total_sched>-anzhl = lv_total.
      <fs_total_sched>-begda = ls_sched-begda.
      <fs_total_sched>-endda = ls_sched-endda.

      CLEAR lv_total.
    ENDAT.
  ENDLOOP.

  LOOP AT lt_total_sched INTO DATA(ls_total_sched).
    READ TABLE gt_p0015 INTO DATA(ls_p0015) WITH KEY pernr = ls_total_sched-pernr.
    IF sy-subrc NE 0.
      lv_opera = 'INS'.
    ELSE.
      lv_opera = 'MOD'.
    ENDIF.
    CHECK ls_p0015-anzhl NE ls_total_sched-anzhl.
    READ TABLE lt_subtype INTO DATA(ls_subtype) WITH KEY subtype = cv_lgart.
    IF sy-subrc = 0.
      IF lv_opera EQ 'INS'.
* Para determinar a data: será o último dia do mês ou o último dia enquanto ativo
        REFRESH: lt_p0000, lt_p0001.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_p0000
                 FROM pa0000
                 WHERE pernr EQ ls_total_sched-pernr
                   AND sprps EQ space.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_p0001
                 FROM pa0001
                 WHERE pernr EQ ls_total_sched-pernr
                   AND sprps EQ space.
        CLEAR: lv_hire_date, lv_fire_date.
        CALL FUNCTION 'HR_PT_HIRE_FIRE'
          IMPORTING
            fire_date            = lv_fire_date
            hire_date            = lv_hire_date
          TABLES
            pp0000               = lt_p0000
            pp0001               = lt_p0001
          EXCEPTIONS
            entry_date_not_found = 1
            feature_error        = 2
            OTHERS               = 3.
        IF sy-subrc NE 0.
        ENDIF.
        CLEAR: lv_begda.
        IF lv_hire_date GT ls_total_sched-begda.
          lv_begda = lv_hire_date.
        ELSEIF lv_fire_date LT ls_total_sched-endda.
          lv_begda = lv_fire_date.
        ELSE.
          CALL FUNCTION 'LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = ls_total_sched-begda
            IMPORTING
              last_day_of_month = lv_begda
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
          IF sy-subrc <> 0.
          ENDIF.
        ENDIF.

        p0015-pernr = ls_total_sched-pernr.
        p0015-infty = ls_subtype-infotype.
        p0015-subty = p0015-lgart = cv_lgart.
        p0015-begda = p0015-endda = lv_begda.
        p0015-anzhl = ls_total_sched-anzhl.
        p0015-zeinh = '001'.

      ELSEIF lv_opera EQ 'MOD'.
        MOVE-CORRESPONDING ls_p0015 TO p0015.
        p0015-pernr = ls_total_sched-pernr.
        p0015-infty = ls_subtype-infotype.
        p0015-subty = p0015-lgart = cv_lgart.
        p0015-begda = p0015-endda = ls_p0015-begda.
        p0015-anzhl = ls_total_sched-anzhl.
        p0015-zeinh = '001'.
      ENDIF.

      CLEAR: ls_return, ls_key.
      zcl_hcm_ws_wfm=>infotype_operation(
        EXPORTING
          iv_infotype        = ls_subtype-infotype   " Infotipo
          iv_employee_number = p0015-pernr            " Nº pessoal
          iv_subty           = p0015-subty            " Subinfotipo
          iv_begda           = p0015-begda            " Início da validade
          iv_endda           = p0015-endda            " Fim da validade
          iv_operation       = lv_opera              " Operação em infotipos
          iv_commit          = abap_false
          is_record          = p0015
        IMPORTING
          es_return          = ls_return             " Parâmetro de retorno
          es_key             = ls_key                " Chave para dados mestre HR
      ).
      MOVE-CORRESPONDING p0015 TO gs_alv.
      IF ls_return IS INITIAL.
        gs_alv-log = 'Processado com sucesso'.

        lv_begda = p0015-begda.
        lv_begda+6(2) = '01'.
        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = lv_begda
          IMPORTING
            last_day_of_month = lv_endda
          EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
        ENDIF.

        LOOP AT gt_sched ASSIGNING FIELD-SYMBOL(<fs_sched>)
                         WHERE pernr EQ p0015-pernr
                           AND begda LE lv_endda
                           AND endda GE lv_begda.
          <fs_sched>-integrado = 'X'.
          MODIFY ztwfm_hcm_sched FROM <fs_sched>.
        ENDLOOP.
      ELSE.
        gs_alv-log = ls_return-message.
      ENDIF.
      APPEND gs_alv TO gt_alv.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display.

  DATA: repid LIKE sy-repid.

  PERFORM layout   USING layout.
  PERFORM fieldcat USING fieldcat.

  SORT gt_alv.

  repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = repid
      is_layout          = layout
      it_fieldcat        = fieldcat
    TABLES
      t_outtab           = gt_alv
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout USING rs_layout TYPE slis_layout_alv.
  rs_layout-colwidth_optimize = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat USING r_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE slis_fieldcat_alv.

  REFRESH r_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name   = 'ZTWFM_HCM_SCHED_S'
      i_bypassing_buffer = 'X'
    CHANGING
      ct_fieldcat        = lt_fieldcat.

  CALL FUNCTION 'LVC_TRANSFER_TO_SLIS'
    EXPORTING
      it_fieldcat_lvc = lt_fieldcat
    IMPORTING
      et_fieldcat_alv = r_fieldcat.

  LOOP AT r_fieldcat INTO ls_fieldcat.
    IF ls_fieldcat-fieldname EQ 'MANDT'.
      ls_fieldcat-no_out = ls_fieldcat-tech = 'X'.
      MODIFY r_fieldcat FROM ls_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fieldcat_init
