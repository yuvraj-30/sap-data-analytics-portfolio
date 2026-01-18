"-----------------------------------------------------------------------
" Z_SD_SALES_EXTRACT
"-----------------------------------------------------------------------
" Purpose
"   Example ABAP report that extracts SD sales order data, applies basic
"   data quality checks, and produces an extract suitable for analytics.
"
" What this demonstrates
"   - Joining SD header/item
"   - Basic completeness and value validations
"   - Simple DQ summary output (counts by severity)
"   - Clean ABAP 7.4+ style (inline declarations, VALUE, FILTER)
"-----------------------------------------------------------------------

REPORT z_sd_sales_extract.

TABLES: vbak, vbap, vbkd.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_datef TYPE sy-datum OBLIGATORY DEFAULT sy-datum-30,
            p_datet TYPE sy-datum OBLIGATORY DEFAULT sy-datum,
            p_vkorg TYPE vbak-vkorg DEFAULT '',
            p_max   TYPE i DEFAULT 5000.
SELECTION-SCREEN END OF BLOCK b1.

TEXT-001 = 'Selection'.

TYPES: BEGIN OF ty_sales_row,
         sales_order TYPE vbak-vbeln,
         item        TYPE vbap-posnr,
         order_date  TYPE vbak-erdat,
         sales_org   TYPE vbak-vkorg,
         sold_to     TYPE vbak-kunnr,
         material    TYPE vbap-matnr,
         plant       TYPE vbap-werks,
         quantity    TYPE vbap-kwmeng,
         net_value   TYPE vbap-netwr,
         currency    TYPE vbak-waerk,
       END OF ty_sales_row.

TYPES: BEGIN OF ty_dq_issue,
         severity TYPE c LENGTH 1, " E=Error, W=Warning, I=Info
         check_id TYPE c LENGTH 30,
         key      TYPE string,
         message  TYPE string,
       END OF ty_dq_issue.

DATA: gt_sales TYPE STANDARD TABLE OF ty_sales_row WITH EMPTY KEY,
      gt_dq    TYPE STANDARD TABLE OF ty_dq_issue WITH EMPTY KEY.

CLASS lcl_dq DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS add_issue
      IMPORTING
        i_severity TYPE c
        i_check_id TYPE csequence
        i_key      TYPE string
        i_message  TYPE string.
ENDCLASS.

CLASS lcl_dq IMPLEMENTATION.
  METHOD add_issue.
    APPEND VALUE ty_dq_issue(
      severity = i_severity
      check_id = i_check_id
      key      = i_key
      message  = i_message
    ) TO gt_dq.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  IF p_datef > p_datet.
    MESSAGE 'From-date must be <= To-date' TYPE 'E'.
  ENDIF.

  "-------------------------------------------------------------------
  " 1. Extract: Join sales header (VBAK) and item (VBAP)
  "-------------------------------------------------------------------
  SELECT
    vbak~vbeln  AS sales_order,
    vbap~posnr  AS item,
    vbak~erdat  AS order_date,
    vbak~vkorg  AS sales_org,
    vbak~kunnr  AS sold_to,
    vbap~matnr  AS material,
    vbap~werks  AS plant,
    vbap~kwmeng AS quantity,
    vbap~netwr  AS net_value,
    vbak~waerk  AS currency
  FROM vbak
  INNER JOIN vbap
    ON vbap~vbeln = vbak~vbeln
  WHERE vbak~erdat BETWEEN @p_datef AND @p_datet
    AND ( @p_vkorg = '' OR vbak~vkorg = @p_vkorg )
  INTO TABLE @gt_sales
  UP TO @p_max ROWS.

  "-------------------------------------------------------------------
  " 2. Special Check: Orphan Items (Rule #6 in documentation)
  "-------------------------------------------------------------------
  PERFORM check_orphans.

  "-------------------------------------------------------------------
  " 3. Standard Data Quality Checks
  "-------------------------------------------------------------------
  LOOP AT gt_sales ASSIGNING FIELD-SYMBOL(<r>).
    DATA(lv_key) = |{ <r>-sales_order }/{ <r>-item }|.

    " Completeness checks
    IF <r>-sold_to IS INITIAL.
      lcl_dq=>add_issue( 'E' , 'SD_MISSING_SOLD_TO' , lv_key , 'Sold-to party (KUNNR) is initial.' ).
    ENDIF.

    IF <r>-material IS INITIAL.
      lcl_dq=>add_issue( 'E' , 'SD_MISSING_MATERIAL' , lv_key , 'Material (MATNR) is initial.' ).
    ENDIF.

    " Value checks
    IF <r>-quantity IS INITIAL OR <r>-quantity <= 0.
      lcl_dq=>add_issue( 'E' , 'SD_INVALID_QTY' , lv_key , |Quantity must be > 0. Got { <r>-quantity }| ).
    ENDIF.

    IF <r>-net_value < 0.
      lcl_dq=>add_issue( 'E' , 'SD_NEGATIVE_NETWR' , lv_key , |Net value is negative: { <r>-net_value }| ).
    ENDIF.

    " Sanity: currency should exist for header
    IF <r>-currency IS INITIAL.
      lcl_dq=>add_issue( 'W' , 'SD_MISSING_CURRENCY' , lv_key , 'Currency (WAERK) is initial; check header.' ).
    ENDIF.
  ENDLOOP.

  "-------------------------------------------------------------------
  " 4. Output Summary
  "-------------------------------------------------------------------
  DATA(lv_rows) = lines( gt_sales ).
  WRITE: / 'Extract rows:', lv_rows.
  WRITE: / 'DQ issues:', lines( gt_dq ).

  DATA(lv_e) = lines( FILTER #( gt_dq USING KEY primary_key WHERE severity = 'E' ) ).
  DATA(lv_w) = lines( FILTER #( gt_dq USING KEY primary_key WHERE severity = 'W' ) ).
  DATA(lv_i) = lines( FILTER #( gt_dq USING KEY primary_key WHERE severity = 'I' ) ).

  SKIP.
  WRITE: / 'Errors  :', lv_e.
  WRITE: / 'Warnings:', lv_w.
  WRITE: / 'Info    :', lv_i.

  SKIP.
  WRITE: / 'Sample output (first 20 rows):'.
  ULINE.

  LOOP AT gt_sales INTO DATA(ls) FROM 1 TO 20.
    WRITE: / ls-sales_order, ls-item, ls-order_date, ls-sales_org, ls-sold_to, ls-material, ls-quantity, ls-net_value, ls-currency.
  ENDLOOP.

  SKIP.
  IF lines( gt_dq ) > 0.
    WRITE: / 'DQ Issues (first 50):'.
    ULINE.
    LOOP AT gt_dq INTO DATA(ldq) FROM 1 TO 50.
      WRITE: / ldq-severity, ldq-check_id, ldq-key, ldq-message.
    ENDLOOP.
  ENDIF.

"-----------------------------------------------------------------------
" FORM Definitions
"-----------------------------------------------------------------------
FORM check_orphans.
  " Detect Sales Items (VBAP) that lack a Header (VBAK)
  " This is a Left Outer Join where the right side is NULL
  SELECT i~vbeln, i~posnr
    FROM vbap AS i
    LEFT OUTER JOIN vbak AS h ON h~vbeln = i~vbeln
    WHERE h~vbeln IS NULL
    INTO TABLE @DATA(lt_orphans)
    UP TO 100 ROWS.

  LOOP AT lt_orphans INTO DATA(ls_orphan).
    DATA(lv_key) = |{ ls_orphan-vbeln }/{ ls_orphan-posnr }|.
    lcl_dq=>add_issue(
      i_severity = 'E'
      i_check_id = 'SD_ORPHAN_ITEM'
      i_key      = lv_key
      i_message  = 'Sales Item exists without Sales Header (Broken Reference)'
    ).
  ENDLOOP.
ENDFORM.
