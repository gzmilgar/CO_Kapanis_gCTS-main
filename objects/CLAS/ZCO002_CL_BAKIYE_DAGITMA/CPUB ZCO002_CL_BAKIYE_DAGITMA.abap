CLASS zco002_cl_bakiye_dagitma DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.

    CLASS-DATA: lt_tt TYPE TABLE OF zco002_cev_kalan_bakiye.

    CLASS-METHODS: get_kalan_bakiye IMPORTING iv_period      TYPE fis_jahrper_conv
                                              iv_bukrs       TYPE bukrs
                                              iv_top         TYPE int8 OPTIONAL
                                              iv_skip        TYPE int8 OPTIONAL
                                              iv_pagging     TYPE abap_boolean
                                    EXPORTING et_data        LIKE lt_tt
                                              ev_total_count TYPE int8 .
