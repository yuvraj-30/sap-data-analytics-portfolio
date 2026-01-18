"-----------------------------------------------------------------------
" Z_FI_REVENUE_RECON
"-----------------------------------------------------------------------
" Purpose
"   Portfolio example of a reconciliation between SD order values and FI
"   posted revenue documents.
"
" Demonstrates
"   - Using BKPF/BSEG for FI postings (simplified)
"   - Comparing aggregated SD NetValue vs FI amounts by date
"   - Flagging variances beyond a threshold
"
" Notes
"   - Real implementations depend on company chart of accounts, revenue
"     GL accounts, billing integration (VBRK/VBRP), etc.
"   - This report illustrates the reconciliation pattern clearly.
"-----------------------------------------------------------------------

REPORT z_fi_revenue_recon.

TABLES: vbak, vbap, bkpf, bseg.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_datef TYPE sy-datum OBLIGATORY DEFAULT sy-datum-30,
            p_datet TYPE sy-datum OBLIGATORY DEFAULT sy-datum,
            p_bukrs TYPE bkpf-bukrs OBLIGATORY,
            p_thr   TYPE p LENGTH 8 DECIMALS 2 DEFAULT '1000.00'.
SELECTION-SCREEN END OF BLOCK b1.

TEXT-001 = 'Selection'.

TYPES: BEGIN OF ty_day_recon,
         day        TYPE sy-datum,
         sd_value   TYPE p LENGTH 16 DECIMALS 2,
         fi_value   TYPE p LENGTH 16 DECIMALS 2,
         variance   TYPE p LENGTH 16 DECIMALS 2,
         status     TYPE c LENGTH 10,
       END OF ty_day_recon.

DATA gt_recon TYPE STANDARD TABLE OF ty_day_recon WITH EMPTY KEY.

START-OF-SELECTION.

  IF p_datef > p_datet.
    MESSAGE 'From-date must be <= To-date' TYPE 'E'.
  ENDIF.

  "-------------------------------------------------------------------
  " 1) Aggregate SD net value by order date
  "-------------------------------------------------------------------
  SELECT
    vbak~erdat AS day,
    SUM( vbap~netwr ) AS sd_value
  FROM vbak
  INNER JOIN vbap ON vbap~vbeln = vbak~vbeln
  WHERE vbak~erdat BETWEEN @p_datef AND @p_datet
  GROUP BY vbak~erdat
  INTO TABLE @DATA(lt_sd).

  "-------------------------------------------------------------------
  " 2) Aggregate FI posted amounts by posting date
  "    Simplification: sum BSEG-DMBTR for revenue postings by date.
  "    In real systems you would filter by revenue GL accounts.
  "-------------------------------------------------------------------
  SELECT
    bkpf~budat AS day,
    SUM( bseg~dmbtr ) AS fi_value
  FROM bkpf
  INNER JOIN bseg ON bseg~bukrs = bkpf~bukrs
                 AND bseg~belnr = bkpf~belnr
                 AND bseg~gjahr = bkpf~gjahr
  WHERE bkpf~bukrs = @p_bukrs
    AND bkpf~budat BETWEEN @p_datef AND @p_datet
  GROUP BY bkpf~budat
  INTO TABLE @DATA(lt_fi).

  "-------------------------------------------------------------------
  " 3) Merge by date, compute variance, flag
  "-------------------------------------------------------------------
  DATA lt_days TYPE SORTED TABLE OF sy-datum WITH UNIQUE KEY table_line.
  LOOP AT lt_sd INTO DATA(ls_sd).
    INSERT ls_sd-day INTO TABLE lt_days.
  ENDLOOP.
  LOOP AT lt_fi INTO DATA(ls_fi).
    INSERT ls_fi-day INTO TABLE lt_days.
  ENDLOOP.

  LOOP AT lt_days INTO DATA(lv_day).
    READ TABLE lt_sd INTO ls_sd WITH KEY day = lv_day.
    READ TABLE lt_fi INTO ls_fi WITH KEY day = lv_day.

    DATA(lv_sd) = COND #( WHEN sy-subrc = 0 THEN ls_sd-sd_value ELSE 0 ).
    DATA(lv_fi) = COND #( WHEN sy-subrc = 0 THEN ls_fi-fi_value ELSE 0 ).
    DATA(lv_var) = lv_sd - lv_fi.

    DATA(lv_status) = COND c( WHEN abs( lv_var ) > p_thr THEN 'FLAG' ELSE 'OK' ).

    APPEND VALUE ty_day_recon(
      day      = lv_day
      sd_value = lv_sd
      fi_value = lv_fi
      variance = lv_var
      status   = lv_status
    ) TO gt_recon.
  ENDLOOP.

  SORT gt_recon BY day.

  " Output
  WRITE: / 'Reconciliation (SD vs FI) - by day'.
  WRITE: / 'Threshold (abs variance) =', p_thr.
  ULINE.

  WRITE: / 'Date', 15 'SD Value', 35 'FI Value', 55 'Variance', 75 'Status'.
  ULINE.

  LOOP AT gt_recon INTO DATA(ls).
    WRITE: / ls-day,
             15 ls-sd_value,
             35 ls-fi_value,
             55 ls-variance,
             75 ls-status.
  ENDLOOP.

