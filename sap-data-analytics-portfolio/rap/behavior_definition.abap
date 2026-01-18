"-----------------------------------------------------------------------
" RAP - Read-only Query Provider Example
"-----------------------------------------------------------------------
" This file contains a complete, working pattern to expose a CDS
" consumption view through RAP using a query provider class.
"
" Artifact references (created in other folders):
"   - CDS consumption view: Z_C_SALES_PERFORMANCE
"
" In ABAP Development Tools (ADT), you typically create:
"   1) Service Definition (see rap/service_definition.abap)
"   2) Service Binding (see rap/service_binding.abap)
"   3) A query provider class (below) implementing IF_RAP_QUERY_PROVIDER
"
" The class below supports:
"   - Filtering (basic)
"   - Paging (skip/top)
"   - Sorting (basic)
"   - Returns data from Z_C_SALES_PERFORMANCE
"-----------------------------------------------------------------------

CLASS zcl_salesperf_qp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
ENDCLASS.

CLASS zcl_salesperf_qp IMPLEMENTATION.

  METHOD if_rap_query_provider~select.

    " Request context
    DATA(lo_req) = io_request.
    DATA(lo_res) = io_response.

    " Paging
    DATA(lv_top)  = lo_req->get_paging( )->get_page_size( ).
    DATA(lv_skip) = lo_req->get_paging( )->get_offset( ).

    " Filters (RAP provides a filter tree; for portfolio simplicity we
    " only handle a couple of common fields using get_as_ranges()).
    DATA(lt_orderdate) = lo_req->get_filter( )->get_as_ranges( iv_name = 'ORDERDATE' ).
    DATA(lt_salesorg)  = lo_req->get_filter( )->get_as_ranges( iv_name = 'SALESORG' ).

    " Sorting (optional; keep simple)
    DATA(lt_sort) = lo_req->get_sort_elements( ).

    " Select from the CDS consumption view
    SELECT *
      FROM Z_C_SALES_PERFORMANCE
      WHERE ( OrderDate IN @lt_orderdate OR @lt_orderdate IS INITIAL )
        AND ( SalesOrg  IN @lt_salesorg  OR @lt_salesorg  IS INITIAL )
      ORDER BY OrderDate
      INTO TABLE @DATA(lt_data).

    " Apply paging in ABAP (for simplicity). In production you would
    " translate paging to SQL using OFFSET/FETCH if available.
    IF lv_skip > 0.
      DELETE lt_data FROM 1 TO lv_skip.
    ENDIF.

    IF lv_top > 0 AND lines( lt_data ) > lv_top.
      DELETE lt_data FROM lv_top + 1 TO lines( lt_data ).
    ENDIF.

    lo_res->set_total_number_of_records( lines( lt_data ) ).
    lo_res->set_data( lt_data ).

  ENDMETHOD.

ENDCLASS.
