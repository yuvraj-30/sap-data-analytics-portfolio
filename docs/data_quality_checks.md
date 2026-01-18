# Data Quality Checks

This portfolio implements a small but realistic set of **quality controls** that are common in ERP analytics.

## Principles
- **Fail fast** on critical issues (missing keys, invalid quantities)
- **Surface and quantify** issues (counts and samples)
- **Keep checks reproducible** (ABAP + SQL)

## Checks (implemented)

### SD Sales
1. **Missing sold-to party** (VBAK-KUNNR)
   - Severity: Error
   - Impact: Customer attribution impossible

2. **Missing material** (VBAP-MATNR)
   - Severity: Error
   - Impact: Product reporting impossible

3. **Invalid quantity** (VBAP-KWMENG <= 0)
   - Severity: Error
   - Impact: KPI distortion

4. **Negative net value** (VBAP-NETWR < 0)
   - Severity: Error
   - Impact: Revenue distortion

5. **Missing currency** (VBAK-WAERK)
   - Severity: Warning
   - Impact: Monetary ambiguity

6. **Items without header** (VBAP without matching VBAK)
   - Severity: Error
   - Impact: Broken referential integrity

### MM Inventory
7. **Negative stock** (MARD-LABST < 0)
   - Severity: Error
   - Impact: Stock integrity risk

8. **Missing base UoM** (MARA-MEINS)
   - Severity: Warning
   - Impact: Unit ambiguity

### FI vs SD
9. **Daily reconciliation variance**
   - Severity: Flag (review)
   - Impact: Financial consistency

## Escalation guidance
- Errors: must be corrected or excluded before publishing KPI datasets
- Warnings: publish with notes, track trend
- Flags: investigate root cause (timing, postings, account mapping)
