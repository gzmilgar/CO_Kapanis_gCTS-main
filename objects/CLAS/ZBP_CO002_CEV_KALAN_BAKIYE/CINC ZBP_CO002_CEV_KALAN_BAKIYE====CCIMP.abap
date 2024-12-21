CLASS lhc_zco002_cev_kalan_bakiye DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zco002_cev_kalan_bakiye RESULT result.

    METHODS read FOR READ
      IMPORTING keys FOR READ zco002_cev_kalan_bakiye RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK zco002_cev_kalan_bakiye.

    METHODS btnrecord FOR MODIFY
      IMPORTING keys FOR ACTION zco002_cev_kalan_bakiye~btnrecord.

ENDCLASS.

CLASS lhc_zco002_cev_kalan_bakiye IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  METHOD btnrecord.
    DATA: lt_bakiye TYPE TABLE OF zco002_t_bakiye.

    DATA(lv_companycode) = VALUE #( keys[ 1 ]-%param-companycode ).
    DATA(lv_fiscalyearperiod) = VALUE #( keys[ 1 ]-%param-fiscalyearperiod ).

    IF lv_fiscalyearperiod IS INITIAL OR lv_companycode IS INITIAL.
      DATA(lv_msg) = new_message_with_text( text = 'İşlem için parametre girilmelidir.' severity = cl_abap_behv=>ms-error ).

      APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_cev_kalan_bakiye.

    ELSE.
      zco002_cl_bakiye_dagitma=>get_kalan_bakiye(
        EXPORTING
          iv_period  = lv_fiscalyearperiod
          iv_bukrs   = lv_companycode
          iv_pagging = abap_false
        IMPORTING
          et_data    = DATA(lt_output)
      ).

      lt_bakiye = CORRESPONDING #( lt_output ) .

      SELECT * FROM @lt_bakiye AS itab
      INNER JOIN zco002_t_bakiye AS bakiye ON itab~companycode = bakiye~companycode
                                          AND itab~fiscalyearperiod = bakiye~fiscalyearperiod
                                          AND bakiye~accdocument IS NOT INITIAL
                                          AND bakiye~reverse_document IS INITIAL
      INTO TABLE @DATA(lt_dummy).
      IF sy-subrc EQ 0.

        lv_msg = new_message_with_text( text = 'Girilen dönem için ters kayıt alınmalıdır.' severity = cl_abap_behv=>ms-error ).

        APPEND VALUE #( %msg  = lv_msg ) TO reported-zco002_cev_kalan_bakiye.

        EXIT.

      ENDIF.

      DELETE FROM zco002_t_bakiye WHERE companycode = @lv_companycode
                                    AND fiscalyearperiod = @lv_fiscalyearperiod.

      MODIFY zco002_t_bakiye FROM TABLE @lt_bakiye.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lsc_zco002_cev_kalan_bakiye DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_zco002_cev_kalan_bakiye IMPLEMENTATION.

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