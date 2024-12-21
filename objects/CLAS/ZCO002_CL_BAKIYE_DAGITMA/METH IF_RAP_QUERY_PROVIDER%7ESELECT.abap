  METHOD if_rap_query_provider~select.
    DATA: lt_output_all  TYPE TABLE OF zco002_cev_kalan_bakiye,
          lt_output      TYPE TABLE OF zco002_cev_kalan_bakiye,
          lv_total_count TYPE int8.

    DATA: lr_period TYPE RANGE OF zco002_cev_kalan_bakiye-fiscalyearperiod,
          lr_bukrs  TYPE RANGE OF zco002_cev_kalan_bakiye-companycode.

    DATA(lo_paging) = io_request->get_paging( ).

    DATA(lv_top) = lo_paging->get_page_size( ).
    DATA(lv_skip) = lo_paging->get_offset( ).

    DATA(lv_where_clause) =  io_request->get_filter( )->get_as_sql_string( ).
    DATA(lt_sort) =  io_request->get_sort_elements( ).

    TRY.
        DATA(lt_filter) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range.
    ENDTRY.

    IF line_exists( lt_filter[ name = 'FISCALYEARPERIOD' ] ).
      lr_period = CORRESPONDING #( lt_filter[ name = 'FISCALYEARPERIOD' ]-range ).
    ENDIF.

    IF line_exists( lt_filter[ name = 'COMPANYCODE' ] ).
      lr_bukrs = CORRESPONDING #( lt_filter[ name = 'COMPANYCODE' ]-range ).
    ENDIF.

    get_kalan_bakiye(
      EXPORTING
        iv_period      = lr_period[ 1 ]-low
        iv_bukrs       = lr_bukrs[ 1 ]-low
        iv_top         = lv_top
        iv_skip        = lv_skip
        iv_pagging     = abap_true
      IMPORTING
        et_data        = lt_output
        ev_total_count = lv_total_count
    ).


    IF io_request->is_total_numb_of_rec_requested(  ).
      io_response->set_total_number_of_records( lv_total_count ).
    ENDIF.

    io_response->set_data( lt_output ).
  ENDMETHOD.