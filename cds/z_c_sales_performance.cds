@EndUserText.label: 'Sales Performance (Consumption Query)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@Analytics.query: true

define view entity Z_C_SALES_PERFORMANCE
  as select from Z_I_SALES_ANALYTICS
    association [0..1] to Z_I_CUSTOMER as _Customer
      on $projection.SoldToParty = _Customer.Customer
{
  OrderDate,
  SalesOrg,
  SoldToParty,
  _Customer.CustomerName,
  Material,
  Currency,

  sum(NetValue)  as TotalRevenue,
  sum(Quantity)  as TotalQuantity,
  
  /* SYNTAX FIX: Standard ABAP CDS count distinct */
  count( distinct SalesOrder ) as SalesOrders
}
where OrderDate is not null
  and Quantity > 0
group by OrderDate, SalesOrg, SoldToParty, _Customer.CustomerName, Material, Currency
