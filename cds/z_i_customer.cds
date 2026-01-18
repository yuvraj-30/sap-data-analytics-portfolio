@EndUserText.label: 'Customer Interface View (DIM)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true

define view entity Z_I_CUSTOMER
  as select from kna1
{
  key kna1.kunnr as Customer,
      kna1.name1 as CustomerName,
      kna1.land1 as Country,
      kna1.ort01 as City,
      kna1.regio as Region,
      kna1.ktokd as AccountGroup
}
