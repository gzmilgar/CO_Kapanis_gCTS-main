  METHOD get_kalan_bakiye.

    DATA: lt_output_all  TYPE TABLE OF zco002_cev_kalan_bakiye,
          lv_total_count TYPE int8.

    SELECT i~costcenter,
               SUM( i~amountincompanycodecurrency ) AS try_total,
               SUM( i~amountinglobalcurrency ) AS eur_total
         FROM i_actualplanjournalentryitem AS i
         INNER JOIN i_costcenter AS cs
         ON i~costcenter EQ cs~costcenter
         INNER JOIN zco002_t_bky_acc AS zb
         ON i~glaccount BETWEEN zb~saknr_low AND zb~saknr_high
         WHERE i~ledger EQ '0L'
           AND i~fiscalyearperiod EQ @iv_period
           AND i~companycode  EQ @iv_bukrs
           AND cs~costcentercategory EQ 'F'
         GROUP BY i~costcenter
         ORDER BY i~costcenter
         INTO TABLE @DATA(lt_amount_total).


    DATA(lt_cost_center) = lt_amount_total.
    DELETE ADJACENT DUPLICATES FROM lt_cost_center COMPARING costcenter.

    DATA: lv_begda TYPE sy-datum,
          lv_endda TYPE sy-datum.

    lv_begda = iv_period(4) && iv_period+5(2) && '01'.
*
    lv_endda = iv_period(4) && iv_period+5(2) && '01'.
    lv_endda+6(2) = '01'.
    lv_endda+4(2) += 1.
    lv_endda -= 1.

    SELECT DISTINCT
           wrk~costcenter,
           mfg~manufacturingorder,
           mfg~confirmationunit,
           SUM( mfg~confirmationyieldquantity ) AS confirmationyieldquantity
    FROM @lt_cost_center AS cs
    INNER JOIN i_workcentercostcenter AS wrk
    ON cs~costcenter EQ wrk~costcenter
    INNER JOIN  i_mfgorderconfirmation AS mfg
    ON wrk~workcenterinternalid EQ mfg~workcenterinternalid
    WHERE cs~costcenter NOT IN ( 'M1200','M1400' )
      AND mfg~postingdate GE @lv_begda
      AND mfg~postingdate LE @lv_endda
      AND mfg~companycode EQ @iv_bukrs
      AND mfg~isreversal NE @abap_true
      AND mfg~isreversed NE @abap_true
      AND wrk~costcenterallocation EQ 001
    GROUP BY wrk~costcenter,
             mfg~manufacturingorder,
             mfg~confirmationunit
    INTO TABLE @DATA(lt_pp_quantity).


    SELECT DISTINCT
           CASE WHEN plant = '1200' THEN 'M1200'
           ELSE 'M1400'
           END AS costcenter,
           orderid,
           materialbaseunit,
           SUM( quantityinbaseunit ) AS quantityinbaseunit
    FROM i_materialdocumentitem_2
    WHERE plant       IN ( '1200','1400' )
      AND postingdate GE @lv_begda
      AND postingdate LE @lv_endda
      AND companycode EQ @iv_bukrs
      AND goodsmovementtype IN ( '131','132' )
      AND reversedmaterialdocument EQ ''
    GROUP BY plant,
             orderid,
             materialbaseunit
    INTO TABLE @DATA(lt_mm_quantity).

    DELETE lt_mm_quantity WHERE quantityinbaseunit EQ 0.
    DELETE lt_pp_quantity WHERE confirmationyieldquantity EQ 0.


    ev_total_count = lines( lt_mm_quantity ) + lines( lt_pp_quantity ).

    LOOP AT lt_pp_quantity INTO DATA(ls_pp).
      APPEND INITIAL LINE TO lt_output_all ASSIGNING FIELD-SYMBOL(<lfs_output>).
      <lfs_output>-costcenter                = ls_pp-costcenter.
      <lfs_output>-orderid                   = ls_pp-manufacturingorder.
      <lfs_output>-confirmationyieldquantity = ls_pp-confirmationyieldquantity.
      <lfs_output>-meinh                     = ls_pp-confirmationunit.
      <lfs_output>-companycode               = iv_bukrs.
      <lfs_output>-fiscalyearperiod          = iv_period.
    ENDLOOP.

    LOOP AT lt_mm_quantity INTO DATA(ls_mm).
      APPEND INITIAL LINE TO lt_output_all ASSIGNING <lfs_output>.
      <lfs_output>-costcenter                = ls_mm-costcenter.
      <lfs_output>-orderid                   = ls_mm-orderid.
      <lfs_output>-confirmationyieldquantity = ls_mm-quantityinbaseunit.
      <lfs_output>-companycode               = iv_bukrs.
      <lfs_output>-fiscalyearperiod          = iv_period.
      <lfs_output>-meinh                     = ls_mm-materialbaseunit.
    ENDLOOP.

    IF iv_pagging EQ abap_true.
      SELECT itab~*
       FROM @lt_output_all AS itab
       ORDER BY costcenter
       INTO TABLE @DATA(lt_output)
      UP TO @iv_top ROWS
      OFFSET @iv_skip.
    ELSE.
      lt_output = lt_output_all.
    ENDIF.


    DATA: lv_menge_d TYPE zco002_cev_kalan_bakiye-confirmationyieldquantity.

    LOOP AT lt_output ASSIGNING FIELD-SYMBOL(<lfs_out>) GROUP BY ( costcenter = <lfs_out>-costcenter )
                                                        INTO DATA(ls_members_grp).

*      LOOP AT GROUP ls_members_grp ASSIGNING FIELD-SYMBOL(<lfs_member>).
*        lv_menge_d += <lfs_member>-confirmationyieldquantity.
*      ENDLOOP.

      LOOP AT lt_output_all INTO DATA(ls_all) WHERE costcenter = ls_members_grp-costcenter.
        lv_menge_d += ls_all-confirmationyieldquantity.
      ENDLOOP.

      READ TABLE lt_amount_total INTO DATA(ls_amount) WITH KEY costcenter = ls_members_grp-costcenter.
      IF sy-subrc IS INITIAL.
        LOOP AT GROUP ls_members_grp ASSIGNING FIELD-SYMBOL(<lfs_member>).
          <lfs_member>-companycodecurrency = 'TRY'.
          <lfs_member>-amountincompanycodecurrency = ls_amount-try_total * ( 100 * <lfs_member>-confirmationyieldquantity / lv_menge_d ) / 100.

          <lfs_member>-globalcurrency = 'EUR'.
          <lfs_member>-amountinglobalcurrency = ls_amount-eur_total * ( 100 * <lfs_member>-confirmationyieldquantity / lv_menge_d ) / 100.
        ENDLOOP.
      ENDIF.

      CLEAR: lv_menge_d, ls_amount.
    ENDLOOP.

    et_data = lt_output.

  ENDMETHOD.