*&---------------------------------------------------------------------*
*& Report ZHCM_WFM_WAGES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE zhcm_wfm_wage_9053_top.
INCLUDE zhcm_wfm_wage_9053_f01.

START-OF-SELECTION.
  PERFORM get_pernr.

END-OF-SELECTION.
  PERFORM: register_days,
           display.
