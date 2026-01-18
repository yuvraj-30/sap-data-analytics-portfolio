-- HANA Validation Queries (Portfolio)
-- These queries mirror the ABAP DQ checks and CDS constraints.
-- Adjust schema prefixes (e.g., SAPABAP1.) to match your HANA catalog.

-- 1) Missing sold-to in sales header
SELECT COUNT(*) AS missing_sold_to
FROM VBAK
WHERE KUNNR IS NULL OR KUNNR = '';

-- 2) Items with missing material
SELECT COUNT(*) AS missing_material
FROM VBAP
WHERE MATNR IS NULL OR MATNR = '';

-- 3) Invalid quantities (<= 0)
SELECT COUNT(*) AS invalid_qty
FROM VBAP
WHERE KWMENG <= 0;

-- 4) Negative net values
SELECT COUNT(*) AS negative_netwr
FROM VBAP
WHERE NETWR < 0;

-- 5) Referential integrity: items without header
SELECT COUNT(*) AS items_without_header
FROM VBAP i
LEFT JOIN VBAK h ON h.VBELN = i.VBELN
WHERE h.VBELN IS NULL;

-- 6) SD vs FI daily reconciliation skeleton
-- NOTE: In real FI, filter by relevant revenue accounts.
SELECT
  h.ERDAT AS day,
  SUM(i.NETWR) AS sd_value
FROM VBAK h
JOIN VBAP i ON i.VBELN = h.VBELN
WHERE h.ERDAT >= ADD_DAYS(CURRENT_DATE, -30)
GROUP BY h.ERDAT
ORDER BY h.ERDAT;
