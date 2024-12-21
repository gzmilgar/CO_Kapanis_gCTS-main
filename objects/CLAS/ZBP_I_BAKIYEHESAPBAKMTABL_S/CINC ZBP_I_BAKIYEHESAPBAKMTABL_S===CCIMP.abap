CLASS LHC_RAP_TDAT_CTS DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      GET
        RETURNING
          VALUE(RESULT) TYPE REF TO IF_MBC_CP_RAP_TDAT_CTS.

ENDCLASS.

CLASS LHC_RAP_TDAT_CTS IMPLEMENTATION.
  METHOD GET.
    result = mbc_cp_api=>rap_tdat_cts( tdat_name = 'ZBAKIYEHESAPBAKMTABL'
                                       table_entity_relations = VALUE #(
                                         ( entity = 'BakiyeHesapBakMTabl' table = 'ZCO002_T_BKY_ACC' )
                                       ) ) ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.
CLASS LHC_ZI_BAKIYEHESAPBAKMTABL_S DEFINITION FINAL INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_INSTANCE_FEATURES FOR INSTANCE FEATURES
        IMPORTING
          KEYS REQUEST requested_features FOR BakiyeHesapBakMTAll
        RESULT result,
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR BakiyeHesapBakMTAll
        RESULT result.
ENDCLASS.

CLASS LHC_ZI_BAKIYEHESAPBAKMTABL_S IMPLEMENTATION.
  METHOD GET_INSTANCE_FEATURES.
    DATA: edit_flag            TYPE abp_behv_op_ctrl    VALUE if_abap_behv=>fc-o-enabled.

    IF lhc_rap_tdat_cts=>get( )->is_editable( ) = abap_false.
      edit_flag = if_abap_behv=>fc-o-disabled.
    ENDIF.
    result = VALUE #( (
               %TKY = keys[ 1 ]-%TKY
               %ACTION-edit = edit_flag
               %ASSOC-_BakiyeHesapBakMTabl = edit_flag ) ).
  ENDMETHOD.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
*    AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZI_BAKIYEHESAPBAKMTABL' ID 'ACTVT' FIELD '02'.
*    DATA(is_authorized) = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed
*                                  ELSE if_abap_behv=>auth-unauthorized ).
*    result-%UPDATE      = is_authorized.
*    result-%ACTION-Edit = is_authorized.
  ENDMETHOD.
ENDCLASS.
CLASS LSC_ZI_BAKIYEHESAPBAKMTABL_S DEFINITION FINAL INHERITING FROM CL_ABAP_BEHAVIOR_SAVER.
  PROTECTED SECTION.
    METHODS:
      SAVE_MODIFIED REDEFINITION,
      CLEANUP_FINALIZE REDEFINITION.
ENDCLASS.

CLASS LSC_ZI_BAKIYEHESAPBAKMTABL_S IMPLEMENTATION.
  METHOD SAVE_MODIFIED ##NEEDED.
  ENDMETHOD.
  METHOD CLEANUP_FINALIZE ##NEEDED.
  ENDMETHOD.
ENDCLASS.
CLASS LHC_ZI_BAKIYEHESAPBAKMTABL DEFINITION FINAL INHERITING FROM CL_ABAP_BEHAVIOR_HANDLER.
  PRIVATE SECTION.
    METHODS:
      GET_GLOBAL_FEATURES FOR GLOBAL FEATURES
        IMPORTING
          REQUEST REQUESTED_FEATURES FOR BakiyeHesapBakMTabl
        RESULT result,
      COPYBAKIYEHESAPBAKMTABL FOR MODIFY
        IMPORTING
          KEYS FOR ACTION BakiyeHesapBakMTabl~CopyBakiyeHesapBakMTabl,
      GET_GLOBAL_AUTHORIZATIONS FOR GLOBAL AUTHORIZATION
        IMPORTING
           REQUEST requested_authorizations FOR BakiyeHesapBakMTabl
        RESULT result,
      GET_INSTANCE_FEATURES FOR INSTANCE FEATURES
        IMPORTING
          KEYS REQUEST requested_features FOR BakiyeHesapBakMTabl
        RESULT result.
ENDCLASS.

CLASS LHC_ZI_BAKIYEHESAPBAKMTABL IMPLEMENTATION.
  METHOD GET_GLOBAL_FEATURES.
    DATA edit_flag TYPE abp_behv_op_ctrl VALUE if_abap_behv=>fc-o-enabled.
    IF lhc_rap_tdat_cts=>get( )->is_editable( ) = abap_false.
      edit_flag = if_abap_behv=>fc-o-disabled.
    ENDIF.
    result-%UPDATE = edit_flag.
    result-%DELETE = edit_flag.
  ENDMETHOD.
  METHOD COPYBAKIYEHESAPBAKMTABL.
    DATA new_BakiyeHesapBakMTabl TYPE TABLE FOR CREATE ZI_BakiyeHesapBakMTabl_S\_BakiyeHesapBakMTabl.

    IF lines( keys ) > 1.
      INSERT mbc_cp_api=>message( )->get_select_only_one_entry( ) INTO TABLE reported-%other.
      failed-BakiyeHesapBakMTabl = VALUE #( FOR fkey IN keys ( %TKY = fkey-%TKY ) ).
      RETURN.
    ENDIF.

    READ ENTITIES OF ZI_BakiyeHesapBakMTabl_S IN LOCAL MODE
      ENTITY BakiyeHesapBakMTabl
        ALL FIELDS WITH CORRESPONDING #( keys )
        RESULT DATA(ref_BakiyeHesapBakMTabl)
        FAILED DATA(read_failed).

    IF ref_BakiyeHesapBakMTabl IS NOT INITIAL.
      ASSIGN ref_BakiyeHesapBakMTabl[ 1 ] TO FIELD-SYMBOL(<ref_BakiyeHesapBakMTabl>).
      DATA(key) = keys[ KEY draft %TKY = <ref_BakiyeHesapBakMTabl>-%TKY ].
      DATA(key_cid) = key-%CID.
      APPEND VALUE #(
        %TKY-SingletonID = 1
        %IS_DRAFT = <ref_BakiyeHesapBakMTabl>-%IS_DRAFT
        %TARGET = VALUE #( (
          %CID = key_cid
          %IS_DRAFT = <ref_BakiyeHesapBakMTabl>-%IS_DRAFT
          %DATA = CORRESPONDING #( <ref_BakiyeHesapBakMTabl> EXCEPT
          SingletonID
        ) ) )
      ) TO new_BakiyeHesapBakMTabl ASSIGNING FIELD-SYMBOL(<new_BakiyeHesapBakMTabl>).
      <new_BakiyeHesapBakMTabl>-%TARGET[ 1 ]-SaknrLow = key-%PARAM-SaknrLow.
      <new_BakiyeHesapBakMTabl>-%TARGET[ 1 ]-SaknrHigh = key-%PARAM-SaknrHigh.

      MODIFY ENTITIES OF ZI_BakiyeHesapBakMTabl_S IN LOCAL MODE
        ENTITY BakiyeHesapBakMTAll CREATE BY \_BakiyeHesapBakMTabl
        FIELDS (
                 SaknrLow
                 SaknrHigh
               ) WITH new_BakiyeHesapBakMTabl
        MAPPED DATA(mapped_create)
        FAILED failed
        REPORTED reported.

      mapped-BakiyeHesapBakMTabl = mapped_create-BakiyeHesapBakMTabl.
    ENDIF.

    INSERT LINES OF read_failed-BakiyeHesapBakMTabl INTO TABLE failed-BakiyeHesapBakMTabl.

    IF failed-BakiyeHesapBakMTabl IS INITIAL.
      reported-BakiyeHesapBakMTabl = VALUE #( FOR created IN mapped-BakiyeHesapBakMTabl (
                                                 %CID = created-%CID
                                                 %ACTION-CopyBakiyeHesapBakMTabl = if_abap_behv=>mk-on
                                                 %MSG = mbc_cp_api=>message( )->get_item_copied( )
                                                 %PATH-BakiyeHesapBakMTAll-%IS_DRAFT = created-%IS_DRAFT
                                                 %PATH-BakiyeHesapBakMTAll-SingletonID = 1 ) ).
    ENDIF.
  ENDMETHOD.
  METHOD GET_GLOBAL_AUTHORIZATIONS.
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM' ID 'TABLE' FIELD 'ZI_BAKIYEHESAPBAKMTABL' ID 'ACTVT' FIELD '02'.
    DATA(is_authorized) = COND #( WHEN sy-subrc = 0 THEN if_abap_behv=>auth-allowed
                                  ELSE if_abap_behv=>auth-unauthorized ).
    result-%ACTION-CopyBakiyeHesapBakMTabl = is_authorized.
  ENDMETHOD.
  METHOD GET_INSTANCE_FEATURES.
    result = VALUE #( FOR row IN keys ( %TKY = row-%TKY
                                        %ACTION-CopyBakiyeHesapBakMTabl = COND #( WHEN row-%IS_DRAFT = if_abap_behv=>mk-off THEN if_abap_behv=>fc-o-disabled ELSE if_abap_behv=>fc-o-enabled )
    ) ).
  ENDMETHOD.
ENDCLASS.