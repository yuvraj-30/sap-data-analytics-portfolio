"-----------------------------------------------------------------------
" Z_MM_INVENTORY_EXTRACT
"-----------------------------------------------------------------------
" Purpose: Extract Inventory Snapshot with high-performance JOINs.
"-----------------------------------------------------------------------
REPORT z_mm_inventory_extract.

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
    CLASS-METHODS add IMPORTING i_sev TYPE c i_id TYPE csequence i_key TYPE string i_msg TYPE string.
ENDCLASS.

CLASS lcl_dq IMPLEMENTATION.
  METHOD add.
    APPEND VALUE ty_dq_issue( severity = i_sev check_id = i_id key = i_key message = i_msg ) TO gt_dq.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  " PERF FIX: Joined MARA directly to avoid SELECT inside LOOP
  SELECT
    mard~matnr AS material,
    mard~werks AS plant,
    mard~lgort AS sloc,
    mara~meins AS uom,
    mard~labst AS stock_qty
  FROM mard
  INNER JOIN mara ON mara~matnr = mard~matnr
  WHERE mard~werks = @p_werks
    AND ( @p_lgort = '' OR mard~lgort = @p_lgort )
  INTO TABLE @gt_inv
  UP TO @p_max ROWS.

  " Data quality checks
  LOOP AT gt_inv ASSIGNING FIELD-SYMBOL(<r>).
    DATA(lv_key) = |{ <r>-plant }/{ <r>-sloc }/{ <r>-material }|.

    IF <r>-uom IS INITIAL.
      lcl_dq=>add( 'W' , 'MM_MISSING_UOM' , lv_key , 'Base UoM (MEINS) missing.' ).
    ENDIF.

    IF <r>-stock_qty < 0.
      lcl_dq=>add( 'E' , 'MM_NEGATIVE_STOCK' , lv_key , |Negative stock: { <r>-stock_qty }| ).
    ENDIF.
  ENDLOOP.

  " Output
  WRITE: / 'Inventory snapshot rows:', lines( gt_inv ).
  WRITE: / 'DQ issues:', lines( gt_dq ).
  
  SKIP.
  LOOP AT gt_inv INTO DATA(ls) FROM 1 TO 20.
    WRITE: / ls-plant, ls-sloc, ls-material, ls-stock_qty, ls-uom.
  ENDLOOP.
