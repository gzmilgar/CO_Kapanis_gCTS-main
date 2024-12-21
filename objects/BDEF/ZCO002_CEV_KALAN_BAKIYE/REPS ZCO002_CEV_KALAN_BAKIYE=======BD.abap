unmanaged implementation in class zbp_co002_cev_kalan_bakiye unique;
//strict ( 2 ); //Uncomment this line in order to enable strict mode 2. The strict mode has two variants (strict(1), strict(2)) and is prerequisite to be future proof regarding syntax and to be able to release your BO.
strict ( 1 );

define behavior for zco002_cev_kalan_bakiye //alias <alias_name>
//late numbering
lock master
authorization master ( instance )
//etag master <field_name>
{
  //  create;
  //  update;
  //  delete;
  static action btnRecord parameter zco002_dd_log_kayit_param;
  field ( readonly ) CostCenter, OrderId, FiscalYearPeriod, CompanyCode;
}