CLASS lhc_zco002_dd_kal_bak_blg_yrt DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PUBLIC SECTION.
    CLASS-DATA: lt_bakiye TYPE TABLE OF zco002_t_bakiye,
                lt_out    TYPE TABLE OF zco002_t_msg_log.
    METHODS: call_journal_entry IMPORTING iv_test     TYPE xsdboolean
                                          iv_end_date TYPE datum
                                EXPORTING et_output   LIKE lt_out
                                CHANGING  ct_bakiye   LIKE lt_bakiye
                                RAISING   cx_ai_system_fault
                                          cx_soap_destination_error,
      call_reverse_journal_entry IMPORTING iv_end_date TYPE datum
                                 EXPORTING et_output   LIKE lt_out
                                 CHANGING  ct_bakiye   LIKE lt_bakiye
                                 RAISING   cx_ai_system_fault
                                           cx_soap_destination_error.

  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zco002_dd_kal_bak_blg_yrt RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zco002_dd_kal_bak_blg_yrt RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zco002_dd_kal_bak_blg_yrt.

    METHODS belge_yarat FOR MODIFY
      IMPORTING keys FOR ACTION zco002_dd_kal_bak_blg_yrt~belge_yarat.
    METHODS ters_kayit FOR MODIFY
      IMPORTING keys FOR ACTION zco002_dd_kal_bak_blg_yrt~ters_kayit.

ENDCLASS.

CLASS lhc_zco002_dd_kal_bak_blg_yrt IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD belge_yarat.
    DATA:lt_bakiye TYPE TABLE OF zco002_t_bakiye.

    DATA(lv_companycode) = VALUE #( keys[ 1 ]-%param-companycode ).
    DATA(lv_fiscalyearperiod) = VALUE #( keys[ 1 ]-%param-fiscalyearperiod ).

    IF lv_fiscalyearperiod IS INITIAL OR lv_companycode IS INITIAL.
      DATA(lv_msg) = new_message_with_text( text = 'İşlem için parametre girilmelidir.' severity = cl_abap_behv=>ms-error ).

      APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.

    ELSE.

      SELECT * FROM zco002_t_bakiye
      WHERE status = @abap_false
        AND companycode = @lv_companycode
        AND fiscalyearperiod = @lv_fiscalyearperiod
      INTO TABLE @lt_bakiye.

      IF sy-subrc NE 0.
        lv_msg = new_message_with_text( text = 'Girilen parametrelere ait veri yoktur.' severity = cl_abap_behv=>ms-error ).

        APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.
        EXIT.
      ENDIF.

      SELECT SINGLE fiscalperiodenddate FROM i_fiscalyearperiod WITH PRIVILEGED ACCESS
        WHERE fiscalyearperiod = @lv_fiscalyearperiod
          AND fiscalyearvariant = 'SZ'
        INTO @DATA(lv_end_date).

      TRY.
          call_journal_entry(
            EXPORTING
              iv_test     = abap_true
              iv_end_date = lv_end_date
            IMPORTING
              et_output   = DATA(lt_output)
            CHANGING
              ct_bakiye   = lt_bakiye
          ).

          IF lt_output IS INITIAL.

            CLEAR: lt_output.

            call_journal_entry(
              EXPORTING
                iv_test     = abap_false
                iv_end_date = lv_end_date
              IMPORTING
                et_output   = lt_output
              CHANGING
                ct_bakiye   = lt_bakiye
            ).

            MODIFY zco002_t_bakiye FROM TABLE @lt_bakiye.

            CLEAR: lv_msg.
            lv_msg = new_message_with_text( text = 'Belgeler oluşturulmuştur. Tablo kontrol edilebilir.' severity = cl_abap_behv=>ms-success ).

            APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.

          ELSE.

            CLEAR: lv_msg.
            MODIFY zco002_t_msg_log FROM TABLE @lt_output.

            lv_msg = new_message_with_text( text = 'Mesaj log tablosunu kontrol ediniz.' severity = cl_abap_behv=>ms-information ).

            APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.

          ENDIF.

          " handle response
        CATCH cx_soap_destination_error.
          " handle error
        CATCH cx_ai_system_fault.
          " handle error
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD call_journal_entry.
*    DATA: lt_data    TYPE zjournal_entry_create_requ_tab,
*          lv_amt_com TYPE zco002_de_fis_hsl,
*          lv_amt_glo TYPE zco002_de_fis_hsl,
*          lt_msg_log TYPE TABLE OF zco002_t_msg_log,
*          lt_log     TYPE TABLE OF zlog_item.
*
*    SORT ct_bakiye BY costcenter confirmationyieldquantity DESCENDING.
*
*    LOOP AT ct_bakiye ASSIGNING FIELD-SYMBOL(<lfs_bakiye>) GROUP BY ( costcenter = <lfs_bakiye>-costcenter ).
*      GET TIME STAMP FIELD DATA(lv_date_time).
*
*      IF ( <lfs_bakiye>-amountincompanycodecurrency > 0 AND <lfs_bakiye>-amountinglobalcurrency < 0  )
*      OR ( <lfs_bakiye>-amountincompanycodecurrency < 0 AND <lfs_bakiye>-amountinglobalcurrency > 0  ).
*        DATA(lv_diff) = abap_true.
*      ENDIF.
*
*      CLEAR: lt_data.
*      APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
*      <lfs_data>-message_header = VALUE #( creation_date_time = lv_date_time
*                                           test_data_indicator = iv_test ).
*      <lfs_data>-journal_entry = VALUE zjournal_entry_create_reques18( original_reference_document_ty = 'BKPFF'
*                                                                       business_transaction_type = 'RFBU'
*                                                                       created_by_user = sy-uname
*                                                                       accounting_document_type = 'SA'
*                                                                       company_code = <lfs_bakiye>-companycode
*                                                                       document_date = sy-datum
*                                                                       posting_date = iv_end_date
*                                                                       tax_determination_date = iv_end_date
*                                          ).
*      IF lv_diff = abap_true.
*        APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<lfs_data2>).
*        <lfs_data2> = CORRESPONDING #( <lfs_data> ).
*      ENDIF.
*      CLEAR:lv_amt_com, lv_amt_glo.
*      LOOP AT GROUP <lfs_bakiye> INTO DATA(ls_bakiye).
*
*        <lfs_data>-journal_entry-item = VALUE #( BASE <lfs_data>-journal_entry-item
*                                                ( glaccount = VALUE #( content = '8120100001' )
*                                                  amount_in_transaction_currency = VALUE zamount( currency_code = ls_bakiye-companycodecurrency
*                                                                                                  content = ls_bakiye-amountincompanycodecurrency )
*                                                  amount_in_group_currency = COND #( WHEN lv_diff = abap_false
*                                                                                     THEN VALUE zamount( currency_code = ls_bakiye-globalcurrency
*                                                                                                         content = ls_bakiye-amountinglobalcurrency )
*                                                                                     ELSE VALUE zamount( currency_code = ls_bakiye-globalcurrency
*                                                                                                         content = 0 ) )
*                                                  account_assignment = VALUE #( order_id = ls_bakiye-orderid ) ) ).
*        lv_amt_com += ls_bakiye-amountincompanycodecurrency.
*        lv_amt_glo += ls_bakiye-amountinglobalcurrency.
*
*        IF lv_diff = abap_true.
*          <lfs_data2>-journal_entry-item = VALUE #( BASE <lfs_data2>-journal_entry-item
*                                                  ( glaccount = VALUE #( content = '8120100001' )
*                                                    amount_in_transaction_currency = VALUE zamount( currency_code = ls_bakiye-globalcurrency
*                                                                                                    content = ls_bakiye-amountinglobalcurrency )
*                                                    amount_in_company_code_currenc = VALUE zamount( currency_code = ls_bakiye-companycodecurrency
*                                                                                                    content = 0 )
*                                                    account_assignment = VALUE #( order_id = ls_bakiye-orderid ) ) ).
*        ENDIF.
*      ENDLOOP.
*
*      <lfs_data>-journal_entry-item = VALUE #( BASE <lfs_data>-journal_entry-item
*                                             ( glaccount = VALUE #( content = '8120100001' )
*                                               amount_in_transaction_currency = VALUE zamount( currency_code = ls_bakiye-companycodecurrency
*                                                                                               content = -1 * lv_amt_com )
*                                               amount_in_group_currency = COND #( WHEN lv_diff = abap_false
*                                                                                  THEN VALUE zamount( currency_code = ls_bakiye-globalcurrency
*                                                                                                      content = -1 * lv_amt_glo )
*                                                                                  ELSE VALUE zamount( currency_code = ls_bakiye-globalcurrency
*                                                                                                      content = 0 ) )
*                                               account_assignment = VALUE #( cost_center = ls_bakiye-costcenter ) ) ).
*
*      IF lv_diff = abap_true.
*        <lfs_data2>-journal_entry-item = VALUE #( BASE <lfs_data2>-journal_entry-item
*                                            ( glaccount = VALUE #( content = '8120100001' )
*                                              amount_in_transaction_currency = VALUE zamount( currency_code = ls_bakiye-globalcurrency
*                                                                                              content = -1 * lv_amt_glo )
*                                              amount_in_company_code_currenc = VALUE zamount( currency_code = ls_bakiye-companycodecurrency
*                                                                                              content = 0 )
*                                              account_assignment = VALUE #( cost_center = ls_bakiye-costcenter ) ) ).
*      ENDIF.
*
*      DATA(lo_destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
*        comm_scenario = 'ZFI000_CS_JOURNEYENTRY'
*      ).
*
*      DATA(lo_proxy) = NEW zco_journal_entry_create_reque( destination = lo_destination ).
*
*      " fill request
*      DATA(ls_str) = VALUE zjournal_entry_create_reques19( message_header = <lfs_data>-message_header
*                                                           journal_entry_create_request = lt_data ).
*      DATA(ls_request) = VALUE zjournal_entry_bulk_create_req( journal_entry_bulk_create_requ = ls_str ).
*
*      lo_proxy->journal_entry_create_request_c(
*        EXPORTING
*          input  = ls_request
*        IMPORTING
*          output = DATA(lo_response)
*      ).
*
*      DATA(ls_response) = VALUE #( lo_response-journal_entry_bulk_create_conf-journal_entry_create_confirmat[ 1 ] OPTIONAL ).
*      DATA(ls_response2) = VALUE #( lo_response-journal_entry_bulk_create_conf-journal_entry_create_confirmat[ 2 ] OPTIONAL ).
*
*      IF iv_test = abap_true.
*        lt_log = CORRESPONDING #( BASE ( lt_log ) ls_response-log-item ).
*        lt_log = CORRESPONDING #( BASE ( lt_log ) ls_response2-log-item ).
*      ENDIF.
*
*      IF ls_response-journal_entry_create_confirmat-accounting_document NE '0000000000'.
*        IF ls_response2 IS NOT INITIAL AND ls_response2-journal_entry_create_confirmat-accounting_document EQ '0000000000'.
*        ELSE.
*          IF iv_test = abap_false.
*            LOOP AT GROUP <lfs_bakiye> ASSIGNING FIELD-SYMBOL(<lfs_bakiye2>).
*              <lfs_bakiye2>-accdocument = ls_response-journal_entry_create_confirmat-accounting_document.
*              <lfs_bakiye2>-accdocument_eur = COND #( WHEN ls_response2-journal_entry_create_confirmat-accounting_document NE '0000000000'
*                                                       AND ls_response2-journal_entry_create_confirmat-accounting_document IS NOT INITIAL
*                                                      THEN ls_response2-journal_entry_create_confirmat-accounting_document
*                                                      ELSE ls_response-journal_entry_create_confirmat-accounting_document ).
*              <lfs_bakiye2>-status = abap_true.
*              CLEAR: <lfs_bakiye2>-reverse_document, <lfs_bakiye2>-reverse_document_eur.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      TRY.
*          DATA(lv_uuid) = cl_uuid_factory=>create_system_uuid(  )->create_uuid_x16( ).
*        CATCH cx_uuid_error.
*      ENDTRY.
*
*      LOOP AT lt_log INTO DATA(ls_message) WHERE severity_code <> 1.
*        lt_msg_log = VALUE #( BASE lt_msg_log
*                            ( log_uuid = lv_uuid
*                              costcenter = <lfs_bakiye>-costcenter
*                              fiscalyearperiod = <lfs_bakiye>-fiscalyearperiod
*                              companycode = <lfs_bakiye>-companycode
*                              message_no = sy-tabix
*                              message = ls_message-note
*                              created_at    = lv_date_time ) ).
*      ENDLOOP.
*
*      CLEAR: ls_response, ls_response2, lv_diff.
*    ENDLOOP.
*
*    et_output = lt_msg_log.

  ENDMETHOD.

  METHOD ters_kayit.
    DATA:lt_bakiye TYPE TABLE OF zco002_t_bakiye.

    DATA(lv_companycode) = VALUE #( keys[ 1 ]-%param-companycode ).
    DATA(lv_fiscalyearperiod) = VALUE #( keys[ 1 ]-%param-fiscalyearperiod ).

    IF lv_fiscalyearperiod IS INITIAL OR lv_companycode IS INITIAL.
      DATA(lv_msg) = new_message_with_text( text = 'İşlem için parametre girilmelidir.' severity = cl_abap_behv=>ms-error ).

      APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.

    ELSE.

      SELECT * FROM zco002_t_bakiye
      WHERE status = @abap_true
        AND companycode = @lv_companycode
        AND fiscalyearperiod = @lv_fiscalyearperiod
      INTO TABLE @lt_bakiye.

      IF sy-subrc NE 0.
        lv_msg = new_message_with_text( text = 'Girilen parametrelere ait veri yoktur.' severity = cl_abap_behv=>ms-error ).

        APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.
        EXIT.
      ENDIF.

      SELECT SINGLE fiscalperiodenddate FROM i_fiscalyearperiod WITH PRIVILEGED ACCESS
        WHERE fiscalyearperiod = @lv_fiscalyearperiod
          AND fiscalyearvariant = 'SZ'
        INTO @DATA(lv_end_date).

      TRY.
          call_reverse_journal_entry(
            EXPORTING
              iv_end_date = lv_end_date
            IMPORTING
              et_output   = DATA(lt_output)
            CHANGING
              ct_bakiye   = lt_bakiye
          ).

          IF lt_output IS NOT INITIAL.

            MODIFY zco002_t_msg_log FROM TABLE @lt_output.

            lv_msg = new_message_with_text( text = 'Mesaj log tablosunu kontrol ediniz.' severity = cl_abap_behv=>ms-information ).

            APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.
            EXIT.
          ENDIF.

          MODIFY zco002_t_bakiye FROM TABLE @lt_bakiye.

          CLEAR: lv_msg.
          lv_msg = new_message_with_text( text = 'Belgeler oluşturulmuştur. Tablo kontrol edilebilir.' severity = cl_abap_behv=>ms-success ).

          APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_dd_kal_bak_blg_yrt.

          " handle response
        CATCH cx_soap_destination_error.
          " handle error
        CATCH cx_ai_system_fault.
          " handle error
      ENDTRY.

    ENDIF.

  ENDMETHOD.

  METHOD call_reverse_journal_entry.
*    DATA: lt_data    TYPE zjournal_entry_create_requ_tab,
*          lv_amt_com TYPE zco002_de_fis_hsl,
*          lv_amt_glo TYPE zco002_de_fis_hsl,
*          lt_msg_log TYPE TABLE OF zco002_t_msg_log,
*          lt_log     TYPE TABLE OF zlog_item.
*
*    SORT ct_bakiye BY costcenter.
*
*    LOOP AT ct_bakiye ASSIGNING FIELD-SYMBOL(<lfs_bakiye>) GROUP BY ( costcenter = <lfs_bakiye>-costcenter ).
*      GET TIME STAMP FIELD DATA(lv_date_time).
*
*      CLEAR: lt_data.
*      APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).
*      <lfs_data>-message_header = VALUE #( creation_date_time = lv_date_time ).
*      <lfs_data>-journal_entry = VALUE zjournal_entry_create_reques18( original_reference_document_ty = 'BKPFF'
*                                                                business_transaction_type = 'RFBU'
*                                                                reversal_reason = '01'
*                                                                reversal_reference_document = |{ <lfs_bakiye>-accdocument }{ <lfs_bakiye>-companycode }{ iv_end_date(4) }|
*                                                                company_code = <lfs_bakiye>-companycode
*                                                                created_by_user = sy-uname
*                                                                ).
*
*      DATA(lo_destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
*        comm_scenario = 'ZFI000_CS_JOURNEYENTRY'
*      ).
*
*      DATA(lo_proxy) = NEW zco_journal_entry_create_reque( destination = lo_destination ).
*
*      " fill request
*      DATA(ls_str) = VALUE zjournal_entry_create_reques19( message_header = <lfs_data>-message_header
*                                                           journal_entry_create_request = lt_data ).
*      DATA(ls_request) = VALUE zjournal_entry_bulk_create_req( journal_entry_bulk_create_requ = ls_str ).
*
*      lo_proxy->journal_entry_create_request_c(
*        EXPORTING
*          input  = ls_request
*        IMPORTING
*          output = DATA(lo_response)
*      ).
*
*      DATA(ls_response) = VALUE #( lo_response-journal_entry_bulk_create_conf-journal_entry_create_confirmat[ 1 ] OPTIONAL ).
*
*      IF ls_response-journal_entry_create_confirmat-accounting_document NE '0000000000'.
*        IF <lfs_bakiye>-accdocument <> <lfs_bakiye>-accdocument_eur.
*          ls_str-journal_entry_create_request[ 1 ]-journal_entry-reversal_reference_document = |{ <lfs_bakiye>-accdocument_eur }{ <lfs_bakiye>-companycode }{ iv_end_date(4) }|.
*          ls_request = VALUE zjournal_entry_bulk_create_req( journal_entry_bulk_create_requ = ls_str ).
*
*          lo_proxy->journal_entry_create_request_c(
*            EXPORTING
*              input  = ls_request
*            IMPORTING
*              output = lo_response
*          ).
*
*          DATA(ls_response_rev) = VALUE #( lo_response-journal_entry_bulk_create_conf-journal_entry_create_confirmat[ 1 ] OPTIONAL ).
*
*          IF ls_response_rev-journal_entry_create_confirmat-accounting_document EQ '0000000000'.
*            lt_log = CORRESPONDING #( BASE ( lt_log ) ls_response_rev-log-item ).
*          ENDIF.
*        ENDIF.
*        LOOP AT GROUP <lfs_bakiye> ASSIGNING FIELD-SYMBOL(<lfs_bakiye2>).
*          <lfs_bakiye2>-reverse_document = ls_response-journal_entry_create_confirmat-accounting_document.
*          <lfs_bakiye2>-reverse_document_eur = COND #( WHEN ls_response_rev-journal_entry_create_confirmat-accounting_document NE '0000000000'
*                                                        AND ls_response_rev-journal_entry_create_confirmat-accounting_document IS NOT INITIAL
*                                                       THEN ls_response_rev-journal_entry_create_confirmat-accounting_document
*                                                       ELSE ls_response-journal_entry_create_confirmat-accounting_document ).
*          <lfs_bakiye2>-status = abap_false.
*        ENDLOOP.
*      ELSE.
*        lt_log = CORRESPONDING #( BASE ( lt_log ) ls_response-log-item ).
*      ENDIF.
*
*      TRY.
*          DATA(lv_uuid) = cl_uuid_factory=>create_system_uuid(  )->create_uuid_x16( ).
*        CATCH cx_uuid_error.
*      ENDTRY.
*
*      LOOP AT lt_log INTO DATA(ls_message) WHERE severity_code <> 1.
*
*        lt_msg_log = VALUE #( BASE lt_msg_log
*                            ( log_uuid = lv_uuid
*                              costcenter = <lfs_bakiye>-costcenter
*                              fiscalyearperiod = <lfs_bakiye>-fiscalyearperiod
*                              companycode = <lfs_bakiye>-companycode
*                              message_no = sy-tabix
*                              message = ls_message-note
*                              created_at    = lv_date_time ) ).
*      ENDLOOP.
*
*      CLEAR: ls_response, ls_response_rev.
*    ENDLOOP.
*
*    et_output = lt_msg_log.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_zco002_dd_kal_bak_blg_yrt DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zco002_dd_kal_bak_blg_yrt IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.