"-----------------------------------------------------------------------
" RAP Service Definition (DDL Source)
"-----------------------------------------------------------------------
" Create an ADT repository object: Service Definition
" Name: ZUI_SALES_PERF_SRV
"
" This service exposes the CDS consumption view for external consumers.
"-----------------------------------------------------------------------

@EndUserText.label: 'Sales Performance Service'

define service ZUI_SALES_PERF_SRV {
  expose Z_C_SALES_PERFORMANCE as SalesPerformance;
}
