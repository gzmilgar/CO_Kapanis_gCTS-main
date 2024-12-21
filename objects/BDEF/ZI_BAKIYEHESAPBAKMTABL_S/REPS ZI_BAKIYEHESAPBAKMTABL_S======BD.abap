managed implementation in class ZBP_I_BAKIYEHESAPBAKMTABL_S unique;
strict;
with draft;
define behavior for ZI_BakiyeHesapBakMTabl_S alias BakiyeHesapBakMTAll
draft table ZCO002_T_BK_D_S
with unmanaged save
lock master total etag LastChangedAtMax
authorization master( global )

{
  field ( readonly )
   SingletonID;

  field ( notrigger )
   SingletonID,
   LastChangedAtMax;


  update;
  internal create;
  internal delete;

  draft action ( features : instance ) Edit;
  draft action Activate optimized;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare;

  association _BakiyeHesapBakMTabl { create ( features : instance ); with draft; }
}

define behavior for ZI_BakiyeHesapBakMTabl alias BakiyeHesapBakMTabl ##UNMAPPED_FIELD
persistent table ZCO002_T_BKY_ACC
draft table ZCO002_T_BKY__D
lock dependent by _BakiyeHesapBakMTAll
authorization dependent by _BakiyeHesapBakMTAll

{
  field ( mandatory : create )
   SaknrLow,
   SaknrHigh;

  field ( readonly )
   SingletonID;

  field ( readonly : update )
   SaknrLow,
   SaknrHigh;

  field ( notrigger )
   SingletonID;


  update( features : global );
  delete( features : global );

  factory action ( features : instance ) CopyBakiyeHesapBakMTabl parameter ZD_CopyBakiyeHesapBakMTablP [1];

  mapping for ZCO002_T_BKY_ACC
  {
    SaknrLow = SAKNR_LOW;
    SaknrHigh = SAKNR_HIGH;
  }

  association _BakiyeHesapBakMTAll { with draft; }
}