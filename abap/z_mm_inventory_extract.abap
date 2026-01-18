"-----------------------------------------------------------------------
" Z_MM_INVENTORY_EXTRACT
"-----------------------------------------------------------------------
" Purpose
"   Example ABAP report that snapshots MM inventory levels and validates
"   key fields and quantities. Designed for downstream analytics.
"
" Demonstrates
"   - Reading stock by plant/storage location (MARD)
"   - Enriching with material master (MARA) basics
"   - Data quality checks and reconciliation-ready totals
"-----------------------------------------------------------------------

REPORT z_mm_inventory_extract.

TABLES: mard, mara.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE mard-werks OBLIGATORY,
            p_lgort TYPE mard-lgort DEFAULT '',
            p_max   TYPE i DEFAULT 5000.
SELECTION-SCREEN END OF BLOCK b1.

TEXT-001 = 'Selection'.

TYPES: BEGIN OF ty_inv_row,
         material   TYPE mard-matnr,
         plant      TYPE mard-werks,
         sloc       TYPE mard-lgort,
         uom        TYPE mara-meins,
         stock_qty  TYPE mard-labst,
         stock_val  TYPE p LENGTH 16 DECIMALS 2, " placeholder (valuation is usually from MBEW)
       END OF ty_inv_row.

TYPES: BEGIN OF ty_dq_issue,
         severity TYPE c LENGTH 1,
         check_id TYPE c LENGTH 30,
         key      TYPE string,
         message  TYPE string,
       END OF ty_dq_issue.

DATA: gt_inv TYPE STANDARD TABLE OF ty_inv_row WITH EMPTY KEY,
      gt_dq  TYPE STANDARD TABLE OF ty_dq_issue WITH EMPTY KEY.

CLASS lcl_dq DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING i_sev TYPE c i_id TYPE csequence i_key TYPE string i_msg TYPE string.
ENDCLASS.

CLASS lcl_dq IMPLEMENTATION.
  METHOD add.
    APPEND VALUE ty_dq_issue( severity = i_sev check_id = i_id key = i_key message = i_msg ) TO gt_dq.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  " Snapshot stock quantities
  SELECT
    mard~matnr AS material,
    mard~werks AS plant,
    mard~lgort AS sloc,
    mard~labst AS stock_qty
  FROM mard
  WHERE mard~werks = @p_werks
    AND ( @p_lgort = '' OR mard~lgort = @p_lgort )
  INTO TABLE @DATA(lt_mard)
  UP TO @p_max ROWS.

  " Enrich with material base UoM (MARA-MEINS)
  LOOP AT lt_mard ASSIGNING FIELD-SYMBOL(<m>).
    SELECT SINGLE meins
      FROM mara
      WHERE matnr = @<m>-material
      INTO @DATA(lv_meins).

    APPEND VALUE ty_inv_row(
      material  = <m>-material
      plant     = <m>-plant
      sloc      = <m>-sloc
      uom       = lv_meins
      stock_qty = <m>-stock_qty
      stock_val = 0
    ) TO gt_inv.
  ENDLOOP.

  " Data quality checks
  LOOP AT gt_inv ASSIGNING FIELD-SYMBOL(<r>).
    DATA(lv_key) = |{ <r>-plant }/{ <r>-sloc }/{ <r>-material }|.

    IF <r>-material IS INITIAL.
      lcl_dq=>add( 'E' , 'MM_MISSING_MATNR' , lv_key , 'Material key is initial.' ).
    ENDIF.

    IF <r>-uom IS INITIAL.
      lcl_dq=>add( 'W' , 'MM_MISSING_UOM' , lv_key , 'Base UoM (MEINS) not found in MARA.' ).
    ENDIF.

    IF <r>-stock_qty < 0.
      lcl_dq=>add( 'E' , 'MM_NEGATIVE_STOCK' , lv_key , |Negative stock: { <r>-stock_qty }| ).
    ENDIF.

  ENDLOOP.

  " Output
  WRITE: / 'Inventory snapshot rows:', lines( gt_inv ).
  WRITE: / 'DQ issues:', lines( gt_dq ).

  SKIP.
  WRITE: / 'Sample output (first 20 rows):'.
  ULINE.
  LOOP AT gt_inv INTO DATA(ls) FROM 1 TO 20.
    WRITE: / ls-plant, ls-sloc, ls-material, ls-stock_qty, ls-uom.
  ENDLOOP.

  SKIP.
  IF lines( gt_dq ) > 0.
    WRITE: / 'DQ Issues (first 50):'.
    ULINE.
    LOOP AT gt_dq INTO DATA(ldq) FROM 1 TO 50.
      WRITE: / ldq-severity, ldq-check_id, ldq-key, ldq-message.
    ENDLOOP.
  ENDIF.

