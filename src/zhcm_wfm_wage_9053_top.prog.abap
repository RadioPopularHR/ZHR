*&---------------------------------------------------------------------*
*&  Include           ZHCM_WFM_WAGES_TOP
*&---------------------------------------------------------------------*
REPORT zhcm_wfm_wage_9053.

TABLES: p0001.

DATA: gt_sched TYPE TABLE OF ztwfm_hcm_sched,
      gt_p0015 TYPE TABLE OF pa0015.

DATA: gt_alv TYPE TABLE OF ztwfm_hcm_sched_s,
      gs_alv LIKE LINE OF gt_alv.

DATA: fieldcat TYPE slis_t_fieldcat_alv,
      layout   TYPE slis_layout_alv.

CONSTANTS: cv_lgart TYPE lgart VALUE '9053'.



SELECT-OPTIONS: so_pernr FOR p0001-pernr MODIF ID sel
                MATCHCODE OBJECT prem.
PARAMETERS: pa_data TYPE begda DEFAULT sy-datum OBLIGATORY.
