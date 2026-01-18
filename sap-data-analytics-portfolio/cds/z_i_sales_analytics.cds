@EndUserText.label: 'Sales Analytics Interface View (SD)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@Analytics.dataCategory: #FACT

define view entity Z_I_SALES_ANALYTICS
  as select from vbap
    association [0..1] to vbak as _Header
      on $projection.SalesOrder = _Header.vbeln
{
  key vbap.vbeln          as SalesOrder,
  key vbap.posnr          as SalesItem,
      _Header.erdat       as OrderDate,
      _Header.vkorg       as SalesOrg,
      _Header.kunnr       as SoldToParty,
      vbap.matnr          as Material,
      vbap.werks          as Plant,
      vbap.kwmeng         as Quantity,
      vbap.netwr          as NetValue,
      _Header.waerk       as Currency,

      _Header
}
