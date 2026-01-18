"-----------------------------------------------------------------------
" Z_FI_REVENUE_RECON
"-----------------------------------------------------------------------
REPORT z_fi_revenue_recon.

PARAMETERS: p_datef TYPE sy-datum OBLIGATORY DEFAULT sy-datum-30,
            p_datet TYPE sy-datum OBLIGATORY DEFAULT sy-datum,
            p_bukrs TYPE bkpf-bukrs OBLIGATORY,
            p_thr   TYPE p LENGTH 8 DECIMALS 2 DEFAULT '1000.00'.

TYPES: BEGIN OF ty_day_recon,
         day      TYPE sy-datum,
         sd_value TYPE p LENGTH 16 DECIMALS 2,
         fi_value TYPE p LENGTH 16 DECIMALS 2,
         variance TYPE p LENGTH 16 DECIMALS 2,
         status   TYPE c LENGTH 10,
       END OF ty_day_recon.

DATA gt_recon TYPE STANDARD TABLE OF ty_day_recon WITH EMPTY KEY.

START-OF-SELECTION.

  " 1) Aggregate SD Net Value
  SELECT
    vbak~erdat AS day,
    SUM( vbap~netwr ) AS sd_value
  FROM vbak
  INNER JOIN vbap ON vbap~vbeln = vbak~vbeln
  WHERE vbak~erdat BETWEEN @p_datef AND @p_datet
  GROUP BY vbak~erdat
  INTO TABLE @DATA(lt_sd).

  " 2) Aggregate FI Posted Amounts (Correcting for Debit/Credit)
  " LOGIC FIX: Check SHKZG. If 'H' (Credit), it is Revenue (Positive for comparison).
  " If 'S' (Debit), it is a reversal/return (Negative).
  SELECT
    bkpf~budat AS day,
    SUM( CASE WHEN bseg~shkzg = 'H' THEN bseg~dmbtr ELSE bseg~dmbtr * -1 END ) AS fi_value
  FROM bkpf
  INNER JOIN bseg ON bseg~bukrs = bkpf~bukrs
                 AND bseg~belnr = bkpf~belnr
                 AND bseg~gjahr = bkpf~gjahr
  WHERE bkpf~bukrs = @p_bukrs
    AND bkpf~budat BETWEEN @p_datef AND @p_datet
  GROUP BY bkpf~budat
  INTO TABLE @DATA(lt_fi).

  " 3) Merge and Compare
  DATA lt_days TYPE SORTED TABLE OF sy-datum WITH UNIQUE KEY table_line.
  LOOP AT lt_sd INTO DATA(ls_sd). INSERT ls_sd-day INTO TABLE lt_days. ENDLOOP.
  LOOP AT lt_fi INTO DATA(ls_fi). INSERT ls_fi-day INTO TABLE lt_days. ENDLOOP.

  LOOP AT lt_days INTO DATA(lv_day).
    READ TABLE lt_sd INTO ls_sd WITH KEY day = lv_day.
    DATA(lv_sd_val) = COND #( WHEN sy-subrc = 0 THEN ls_sd-sd_value ELSE 0 ).

    READ TABLE lt_fi INTO ls_fi WITH KEY day = lv_day.
    DATA(lv_fi_val) = COND #( WHEN sy-subrc = 0 THEN ls_fi-fi_value ELSE 0 ).

    DATA(lv_var) = lv_sd_val - lv_fi_val.
    DATA(lv_stat) = COND c( WHEN abs( lv_var ) > p_thr THEN 'FLAG' ELSE 'OK' ).

    APPEND VALUE ty_day_recon( day = lv_day sd_value = lv_sd_val fi_value = lv_fi_val 
                               variance = lv_var status = lv_stat ) TO gt_recon.
  ENDLOOP.

  SORT gt_recon BY day.
  
  cl_demo_output=>new_section( 'SD vs FI Revenue Reconciliation' ).
  cl_demo_output=>write_data( gt_recon ).
  cl_demo_output=>display( ).
