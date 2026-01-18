# Data Model (Logical)

This portfolio uses standard SAP ERP tables to demonstrate analytics modelling.

## Source tables (typical)

### SD (Sales & Distribution)
- **VBAK**: Sales document header
- **VBAP**: Sales document item
- **KNA1**: Customer master (general)

### MM (Materials Management)
- **MARD**: Storage location stock
- **MARA**: Material master (general)

### FI (Financial Accounting)
- **BKPF**: Accounting document header
- **BSEG**: Accounting document segment

## Analytics layer

### Interface views (I_*)
- `Z_I_SALES_ANALYTICS` (FACT-like): SD header+item semantics
- `Z_I_CUSTOMER` (DIM): Customer attributes for joins

### Consumption view (C_*)
- `Z_C_SALES_PERFORMANCE` (Query): Aggregated revenue/quantity by date, org, customer, material

## Lineage

VBAK + VBAP -> Z_I_SALES_ANALYTICS -> Z_C_SALES_PERFORMANCE
KNA1 -> Z_I_CUSTOMER -> join in Z_C_SALES_PERFORMANCE

## Why this design
- Clear separation of **semantic modelling** from **reporting consumption**
- Reusable interface views
- Easy extension (add billing, profitability, etc.)
