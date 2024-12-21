unmanaged implementation in class zbp_co002_dd_kal_bak_blg_yrt unique;
strict ( 1 );

define behavior for ZCO002_DD_KAL_BAK_BLG_YRT //alias <alias_name>
//late numbering
lock master
authorization master ( instance )
//etag master <field_name>
{
  //  create;
  //  update;
  //  delete;
  static action belge_yarat parameter zco002_dd_log_kayit_param;
  static action ters_kayit parameter zco002_dd_log_kayit_param;
  field ( readonly ) Costcenter, Orderid, Fiscalyearperiod, Companycode;
}