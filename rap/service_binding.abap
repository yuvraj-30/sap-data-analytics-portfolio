"-----------------------------------------------------------------------
" RAP Service Binding (How-to + configuration)
"-----------------------------------------------------------------------
" In SAP, a Service Binding is created as a repository object in ADT.
" It is not typically authored as plain text. This file provides exact,
" step-by-step binding configuration for an end-to-end demo.
"
" 1) Create Service Binding
"    - Object type: Service Binding
"    - Name: ZUI_SALES_PERF_BIND
"    - Binding type: OData V4 (recommended)
"    - Service definition: ZUI_SALES_PERF_SRV
"
" 2) Activate the binding
"    - Publish the service
"    - Note the service URL shown by ADT
"
" 3) Assign Query Provider
"    - In the binding, map entity 'SalesPerformance' to query provider
"      class ZCL_SALESPERF_QP
"
" 4) Test
"    - Use /IWFND/GW_CLIENT or Postman
"    - Example: GET .../SalesPerformance?$top=50
"
" What this demonstrates
"    - CDS consumption view exposed via OData
"    - Filter/paging support implemented in query provider
"-----------------------------------------------------------------------
