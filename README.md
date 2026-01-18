# SAP Data & Analytics Portfolio (ABAP + CDS + RAP)

## Purpose
This repository demonstrates **end-to-end SAP data & analytics engineering** using:
- **SAP ERP module context**: SD (Sales & Distribution), MM (Materials Management), FI/CO (Finance)
- **ABAP** extraction + validation patterns
- **ABAP CDS** interface and consumption views for analytics
- **RAP** (RESTful ABAP Programming Model) service exposure (read-only query)
- **HANA SQL** validation queries that mirror the data quality rules

This is intentionally **data-focused** (governed datasets + quality controls), rather than UI/Fiori.

## What you can show in interviews
- You understand **SAP transactional data** and how to make it **reporting-ready**
- You can implement **quality gates** (completeness, referential integrity, value checks, reconciliations)
- You can model analytics with **CDS FACT/DIM** and deliver **consumption views**
- You can expose data via **RAP** (OData) for SAC/Power BI/external consumers

---

## Repository structure

```
abap/   ABAP programs that extract/validate data patterns
cds/    CDS Interface (I_*) and Consumption (C_*) views
rap/    RAP query provider + service definition
sql/    HANA SQL checks equivalent to ABAP DQ rules
docs/   Data model + DQ rulebook
```

---

## Quick start (how to use this repo)

### 1) CDS objects
Create the CDS objects in ABAP Development Tools (ADT):
- `cds/z_i_customer.cds`
- `cds/z_i_sales_analytics.cds`
- `cds/z_c_sales_performance.cds`

Activate in this order:
1. `Z_I_CUSTOMER`
2. `Z_I_SALES_ANALYTICS`
3. `Z_C_SALES_PERFORMANCE`

### 2) RAP service exposure (read-only)
This repo includes:
- Behavior definition (read-only query)
- Query provider class implementing `IF_RAP_QUERY_PROVIDER`
- Service definition exposing `Z_C_SALES_PERFORMANCE`

In ADT:
1. Create class `ZCL_SALES_PERF_QP` from `rap/behavior_definition.abap` (copy/paste class code)
2. Activate the service definition from `rap/service_definition.abap`
3. Create a Service Binding (OData V4 recommended) and publish

### 3) ABAP programs
Programs in `abap/` show robust patterns:
- Parameterized extracts
- Centralised validation checks
- Structured DQ logging to internal table (and optional persistence)

> Note: The programs are written to be **drop-in adaptable**. Where persistence tables are needed (e.g., `ZDQ_LOG`), the code clearly marks them as optional.

---

## Modules covered
- **SD**: Sales order header/item, quantities, net values
- **MM**: Inventory snapshot / stock (example based on common MM structures)
- **FI**: Revenue reconciliation against SD totals (high-level pattern)

---

## Data quality philosophy
See `docs/data_quality_checks.md` for:
- Rule list
- Severity definitions
- Escalation approach
- Reconciliation logic

---

## Disclaimer
This is a **portfolio repository** intended to demonstrate capability and approach.
SAP systems differ by implementation and industry; you should adapt table names, fields, authorizations, and performance techniques (e.g., CDS pushdown, buffering, partitioning) to the target system.

---

## Author
**Yuvraj Singh**
- GitHub: https://github.com/yuvraj-30
