*---------------------------------------------------------------------*
*  Report  ZR_TAXRATE_BCB
*---------------------------------------------------------------------*
*  Upload exchange rates from Brazil Central Bank in CSV format
*---------------------------------------------------------------------*
*  Copia customizada do programa standard RFIMPNBS (note 1286897)
*---------------------------------------------------------------------*

REPORT zr_taxrate_bcb NO STANDARD PAGE HEADING LINE-SIZE 250.

INCLUDE emsg.

*---------------------------------------------------------------------*
*     data declaration
*---------------------------------------------------------------------*
TYPE-POOLS: slis, ixml, abap.

TABLES bapi1093_0.

* global types
TYPES:

  BEGIN OF t_curr_line,                            "line in curr.table
    waers TYPE waers_curc,
    isocd TYPE isocd,
    rate  TYPE string,
  END OF t_curr_line,

  BEGIN OF t_csv_line,                             "line in input file
    data(80) TYPE c,
  END OF t_csv_line,

  BEGIN OF t_err_line,                             "error line
    curr TYPE waers,
    text TYPE bapi_msg,
  END OF t_err_line,

  t_data_src(3) TYPE c,                            "data source

  t_curr        TYPE STANDARD TABLE OF t_curr_line, "currencies
  t_fcurr       TYPE STANDARD TABLE OF bapi1093_3, "type table
  "for 'From currency'
  t_bapi1093_0  TYPE STANDARD TABLE OF bapi1093_0.

* global constants
CONSTANTS:

  gc_www      TYPE t_data_src VALUE 'WWW',                  "web
  gc_as       TYPE t_data_src VALUE 'AS',                   "application server
  gc_pc       TYPE t_data_src VALUE 'PC',                   "local PC
  gc_filename TYPE fileintern VALUE 'FI_TAXRATE_BCB_FILE'.  "logical file name

* global data
DATA:

  gt_curr        TYPE TABLE OF t_curr_line,        "curr. for import

  gt_csv_tab     TYPE TABLE OF t_csv_line,         "file content
  g_csv_tab_size TYPE i,                           "file size

  g_content      TYPE string,                      "http content
  g_data_src     TYPE t_data_src,                  "data source

  gt_exch_rate   TYPE TABLE OF bapi1093_0,         "for BAPI interface
  gt_err         TYPE TABLE OF t_err_line,         "error table

* variables used to test inactivity of some fields  "ACC30 >>>
  gv_inctv_www   TYPE abap_bool VALUE abap_false,
  gv_inctv_as    TYPE abap_bool VALUE abap_false,
  gv_inctv_pc    TYPE abap_bool VALUE abap_false,           "ACC30 <<<

  gv_valueori    TYPE c VALUE 'P',
  gv_invert      TYPE abap_bool VALUE abap_false,
  gv_email       TYPE abap_bool VALUE abap_false,
  gs_loghandle   TYPE balloghndl.

*---------------------------------------------------------------------*
*     selection screen
*---------------------------------------------------------------------*
* general data
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS:
    gp_date  TYPE gdatu_cur DEFAULT sy-datum OBLIGATORY,
    gp_movdt TYPE rfimp_movdt AS CHECKBOX DEFAULT abap_true.
  SELECTION-SCREEN SKIP.

  PARAMETERS:
    gp_kurst TYPE kurst_curr DEFAULT 'EURX' OBLIGATORY.

  SELECT-OPTIONS:
    gso_fcur FOR bapi1093_0-from_curr NO INTERVALS OBLIGATORY.
  PARAMETERS:
    gp_tcurr TYPE tcurr_curr DEFAULT 'BRL' OBLIGATORY MODIF ID cur,
    gp_dev   TYPE bapi1093_2-dev_allow DEFAULT '010' OBLIGATORY.

  PARAMETERS:
    gp_orip RADIOBUTTON GROUP g1,
    gp_oris RADIOBUTTON GROUP g1.

  SELECTION-SCREEN SKIP.
  PARAMETERS:
    gp_inv TYPE xfeld AS CHECKBOX DEFAULT abap_true.
  SELECTION-SCREEN SKIP.

  PARAMETERS:
    gp_mail TYPE xfeld AS CHECKBOX DEFAULT abap_false,
    gp_test TYPE rfimp_test AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b1.

* data source
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      rb_www   RADIOBUTTON GROUP rg1 DEFAULT 'X' USER-COMMAND ac1.
    SELECTION-SCREEN COMMENT (31) TEXT-rb1 FOR FIELD gp_dest.
    SELECTION-SCREEN POSITION 37.
    PARAMETERS:
      gp_dest TYPE rfcdes-rfcdest MODIF ID www LOWER CASE.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      rb_as    RADIOBUTTON GROUP rg1.
    SELECTION-SCREEN COMMENT (31) TEXT-rb2 FOR FIELD gp_filas.
    SELECTION-SCREEN POSITION 37.
    PARAMETERS:
      gp_filas TYPE file_name LOWER CASE MODIF ID as.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
      rb_pc    RADIOBUTTON GROUP rg1.
    SELECTION-SCREEN COMMENT (31) TEXT-rb3 FOR FIELD gp_filpc.
    SELECTION-SCREEN POSITION 37.
    PARAMETERS:
      gp_filpc TYPE localfile MODIF ID pc.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

*---------------------------------------------------------------------*
*     initialization
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM auth_check.

*---------------------------------------------------------------------*
*     at selection-screen on block b3
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK b3.
  PERFORM: init_global,
           check_input_screen.

*---------------------------------------------------------------------*
*     at selection-screen on value-request
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR gp_filpc.
  PERFORM file_open_dialog CHANGING gp_filpc.

*---------------------------------------------------------------------*
*     at selection-screen output                            "ACC30
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.                                 "ACC30 >>>
  gv_inctv_www = abap_false.
  gv_inctv_as  = abap_false.
  gv_inctv_pc  = abap_false.

  LOOP AT SCREEN.
    IF screen-name = 'GP_DEST'.
      IF rb_www <> abap_true.
        screen-input = '0'.
        gv_inctv_www = abap_true.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-name = 'GP_FILAS'.
      IF rb_as <> abap_true.
        screen-input = '0'.
        gv_inctv_as = abap_true.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-name = 'GP_FILPC'.
      IF rb_pc <> abap_true.
        screen-input = '0'.
        gv_inctv_pc = abap_true.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.                                                  "ACC30 <<<

*---------------------------------------------------------------------*
*     start-of-selection
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: init_global,
           fill_iso_code USING gso_fcur[]
                               gt_curr,
           create_log.

  CASE g_data_src.
    WHEN gc_www.
      PERFORM http_get USING    gp_dest
                                gp_date
                       CHANGING gt_csv_tab.

    WHEN gc_as.
      PERFORM upload_as CHANGING gp_filas
                                 gt_csv_tab
                                 g_csv_tab_size.

    WHEN gc_pc.
      PERFORM upload_pc USING    gp_filpc
                        CHANGING gt_csv_tab
                                 g_csv_tab_size.
  ENDCASE.

* process error
  IF NOT gt_err IS INITIAL.
    PERFORM process_error.
    RETURN.
  ENDIF.

* find rate in csv
  PERFORM convert_data USING    gp_date
                                gt_csv_tab
                       CHANGING gt_curr.

  IF NOT gt_err IS INITIAL.
    PERFORM process_error.
    RETURN.
  ENDIF.

* convert to BAPI structure
  PERFORM fill_item USING    gp_date
                             gp_movdt
                             gp_kurst
                             gp_tcurr
                             gt_curr
                    CHANGING gt_exch_rate.

  IF NOT gt_err IS INITIAL.
    PERFORM process_error.
    RETURN.
  ENDIF.

* sorting
  SORT gt_exch_rate.

* check correct date
  PERFORM check_date USING gt_exch_rate
                           gp_date.
  IF NOT gt_err IS INITIAL.
    PERFORM process_error.
    RETURN.
  ENDIF.

* add 1 day
  IF gp_movdt = abap_true.
    PERFORM change_date USING gt_exch_rate.
  ENDIF.

* update
  PERFORM update_rates USING gp_test
                             gp_dev
                             gt_exch_rate.
  IF NOT gt_err IS INITIAL.
    PERFORM process_error.
    RETURN.
  ELSE.
    PERFORM: save_log,
             send_log,
             output_list USING gt_exch_rate.
  ENDIF.

*---------------------------------------------------------------------*
*       Form upload_pc
*---------------------------------------------------------------------*
*       read data from PC
*---------------------------------------------------------------------*
FORM upload_pc USING    p_file     TYPE localfile
               CHANGING pt_csv_tab TYPE STANDARD TABLE
                        p_size     TYPE i.

  CONSTANTS lc_file_type TYPE char10 VALUE 'ASC'.

  DATA l_file TYPE string.

  l_file = p_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = l_file
      filetype                = lc_file_type
    IMPORTING
      filelength              = p_size
    CHANGING
      data_tab                = pt_csv_tab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    MESSAGE e002(fb) WITH p_file.
  ENDIF.

ENDFORM.                               " upload_pc

*---------------------------------------------------------------------*
*       FORM upload_as
*---------------------------------------------------------------------*
*       read data from application server
*---------------------------------------------------------------------*
FORM upload_as CHANGING p_file         TYPE file_name
                        pt_csv_tab     TYPE STANDARD TABLE
                        p_csv_tab_size TYPE i.

  DATA: ls_content TYPE t_csv_line,
        l_len      TYPE i,
        l_subrc    TYPE syst-subrc.

* Begin of note 1745442
  CONDENSE p_file.
  CALL FUNCTION 'FILE_VALIDATE_NAME'
    EXPORTING
      logical_filename           = gc_filename
      parameter_1                = sy-cprog
    CHANGING
      physical_filename          = p_file
    EXCEPTIONS
      logical_filename_not_found = 1
      validation_failed          = 2
      OTHERS                     = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
* End of note 1745442

  OPEN DATASET p_file FOR INPUT IN BINARY MODE.

  IF sy-subrc <> 0.
    MESSAGE e002(fb) WITH p_file.
  ENDIF.

  CLEAR p_csv_tab_size.

  DO.
    CLEAR ls_content.
    READ DATASET p_file INTO ls_content LENGTH l_len.
    l_subrc = sy-subrc.

    p_csv_tab_size = p_csv_tab_size + l_len.   "calculate file size
    APPEND ls_content TO pt_csv_tab.

    IF l_subrc <> 0.       "for last row in input file sy-subrc = 4
      EXIT.
    ENDIF.
  ENDDO.

  CLOSE DATASET p_file.

ENDFORM.                               " upload_as

*---------------------------------------------------------------------*
*      Form  auth_check
*---------------------------------------------------------------------*
*      authority check
*---------------------------------------------------------------------*
FORM auth_check.

  CALL FUNCTION 'VIEW_AUTHORITY_CHECK'
    EXPORTING
      view_name                      = 'V_TCURR'
    EXCEPTIONS
      invalid_action                 = 1
      no_authority                   = 2
      no_clientindependent_authority = 3
      table_not_found                = 4
      no_linedependent_authority     = 5
      OTHERS                         = 6.

  IF sy-subrc <> 0.
    MESSAGE e052(sv).
  ENDIF.

ENDFORM.                               " auth_check

*---------------------------------------------------------------------*
*      Form  http_get
*---------------------------------------------------------------------*
*      read data with http
*---------------------------------------------------------------------*
FORM http_get USING    p_dest    TYPE rfcdes-rfcdest
                       p_date    TYPE datum
              CHANGING pt_csv_tab TYPE STANDARD TABLE.

  DATA:
    l_client        TYPE REF TO if_http_client,
    lt_hfields      TYPE tihttpnvp,
    l_hfield        TYPE ihttpnvp,
    l_content       TYPE string,
    l_mess          TYPE string,
    l_subrc         TYPE sy-subrc,
    l_line(72)      TYPE c,
    l_len           TYPE i,
    l_len2          TYPE i,
    ls_err          TYPE t_err_line,
    l_str           TYPE string,
    l_url           TYPE string,
    l_server        TYPE rfcdisplay-rfchost,
    l_proxy_host    TYPE rfcdisplay-rfcgwhost,
    l_proxy_service TYPE rfcdisplay-rfcgwserv,
    l_path_prefix   TYPE string,
    l_ssl           TYPE rfcdisplay-rfcsnc.
  CONSTANTS:
    lc_regexdt TYPE string VALUE '[0-9]{8}',
    lc_http    TYPE string VALUE 'HTTP://',
    lc_https   TYPE string VALUE 'HTTPS://'.

* read destination
  CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
    EXPORTING
      destination             = p_dest
      authority_check         = ' '
      bypass_buf              = 'X'
    IMPORTING
      server                  = l_server          "www4.bcb.gov.br
      path_prefix             = l_path_prefix     "/Download/fechamento/20200102.csv
      ssl                     = l_ssl             "'X'
      proxy_host              = l_proxy_host
      proxy_service           = l_proxy_service
    EXCEPTIONS
      authority_not_available = 1
      destination_not_exist   = 2
      information_failure     = 3
      internal_failure        = 4
      no_http_destination     = 5
      OTHERS                  = 6.
  IF sy-subrc <> 0.
*   error
    l_str = sy-subrc.
    CONCATENATE TEXT-e05 l_str INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
    RETURN.
  ENDIF.

* setup url
  l_url = SWITCH #( l_ssl WHEN abap_true THEN lc_https ELSE lc_http  )
       && l_server
       && replace( val = l_path_prefix regex = lc_regexdt with = p_date ).

* create connection
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url           = l_url
      proxy_host    = CONV #( l_proxy_host )
      proxy_service = CONV #( l_proxy_service )
    IMPORTING
      client        = l_client
    EXCEPTIONS
      OTHERS        = 1.

  IF sy-subrc <> 0.
*   error
    l_str = sy-subrc.
    CONCATENATE TEXT-e05 l_str INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
    RETURN.
  ENDIF.

* set header fields
  CLEAR l_hfield.
  l_hfield-name  = '~request_method'.                       "#EC NOTEXT
  l_hfield-value = 'GET'.                                   "#EC NOTEXT
  APPEND l_hfield TO lt_hfields.

  CLEAR l_hfield.
  l_hfield-name  = '~server_protocol'.                      "#EC NOTEXT
  l_hfield-value = 'HTTP/1.1'.                              "#EC NOTEXT
  APPEND l_hfield TO lt_hfields.

  CLEAR l_hfield.
  l_hfield-name  = 'cache-control'.                         "#EC NOTEXT
  l_hfield-value = 'no-cache'.                              "#EC NOTEXT
  APPEND l_hfield TO lt_hfields.

  CALL METHOD l_client->request->set_header_fields
    EXPORTING
      fields = lt_hfields.

* send
  CALL METHOD l_client->send
    EXPORTING
      timeout                    = 0
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      http_invalid_timeout       = 4.

  IF sy-subrc <> 0.
*   error
    CALL METHOD l_client->get_last_error
      IMPORTING
        code    = l_subrc
        message = l_mess.

    l_str = l_subrc.
    CONCATENATE TEXT-e06 l_str INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
    CONCATENATE TEXT-e07 l_mess INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
    RETURN.
  ENDIF.

* receive
  CALL METHOD l_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    CALL METHOD l_client->get_last_error
      IMPORTING
        code    = l_subrc
        message = l_mess.

    l_str = l_subrc.
    CONCATENATE TEXT-e06 l_str INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
    CONCATENATE TEXT-e07 l_mess INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
    RETURN.
  ENDIF.

* get content
  l_content = l_client->response->get_cdata( ).
  IF l_content IS NOT INITIAL.
    SPLIT l_content AT cl_abap_char_utilities=>newline INTO TABLE pt_csv_tab[].
  ENDIF.

* close connection
  CALL METHOD l_client->close ##SUBRC_OK
    EXCEPTIONS
      http_invalid_state = 1
      OTHERS             = 2.

ENDFORM.                    " http_get

*---------------------------------------------------------------------*
*      Form  update_rates
*---------------------------------------------------------------------*
*      update rates
*---------------------------------------------------------------------*
FORM update_rates USING p_test       TYPE c
                        p_dev        TYPE bapi1093_2-dev_allow
                        pt_exch_rate TYPE t_bapi1093_0.

  DATA: lt_return    TYPE TABLE OF bapiret2,
        ls_return    TYPE bapiret2,
        ls_err       TYPE t_err_line,
        ls_exch_rate TYPE bapi1093_0,
        l_lin        TYPE i,
        l_num(4)     TYPE n,
        l_text(200)  TYPE c,
        l_str        TYPE string,
        l_pos        TYPE i.                                "n1713187

  CALL FUNCTION 'BAPI_EXCHRATE_CREATEMULTIPLE'
    EXPORTING
      upd_allow     = space
      chg_fixed     = space
      dev_allow     = p_dev
    TABLES
      exchrate_list = pt_exch_rate
      return        = lt_return.

* check errors
  DELETE lt_return WHERE type = 'I' OR
                         type = 'W' OR
                         type = 'S'.

  DESCRIBE TABLE lt_return LINES l_lin.
  IF l_lin > 0.
*   error during update
    ROLLBACK WORK.                                     "#EC CI_ROLLBACK

    LOOP AT lt_return INTO ls_return.
*     error E! - different errors 'Line xxx'
      IF ( ls_return-id = 'E!' ) AND
         ( ls_return-number >= '008' AND ls_return-number <= '021' ).
        FIND FIRST OCCURRENCE OF ':' IN ls_return-message   "n1713187
             MATCH OFFSET l_pos.                            "n1713187
        l_pos = l_pos - 4.                                  "n1713187
        WRITE ls_return-message+l_pos(4) TO l_num.          "n1713187
        READ TABLE pt_exch_rate INTO ls_exch_rate INDEX l_num.
        ls_err-curr = ls_exch_rate-from_curr.
      ENDIF.

      ls_err-text = ls_return-message.
      APPEND ls_err TO gt_err.
    ENDLOOP.

    RETURN.
  ELSE.

*   update succesfull
    DESCRIBE TABLE pt_exch_rate LINES l_lin.
    l_str = l_lin.
    CONCATENATE TEXT-011 l_str INTO l_text SEPARATED BY space.
    IF syst-batch = abap_true.
      CONCATENATE l_text TEXT-012 INTO l_text SEPARATED BY space.
    ENDIF.
    PERFORM msg_add_free_text USING co_msg_success l_text.
  ENDIF.

  IF p_test IS INITIAL.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.                                     "#EC CI_ROLLBACK
  ENDIF.

ENDFORM.                    " update_rates

*---------------------------------------------------------------------*
*      Form  check_date
*---------------------------------------------------------------------*
*      check valid date in data source
*---------------------------------------------------------------------*
FORM check_date USING pt_exch_rate TYPE t_bapi1093_0
                      p_date       TYPE bapi1093_0-valid_from.

  DATA: ls_exch_rate TYPE bapi1093_0,
        ls_err       TYPE t_err_line,
        l_date(10)   TYPE c.

  READ TABLE pt_exch_rate INTO ls_exch_rate INDEX 1.

  IF ls_exch_rate-valid_from <> p_date.
    WRITE p_date TO l_date DD/MM/YYYY.
    CONCATENATE TEXT-e04 l_date INTO ls_err-text SEPARATED BY space.
    APPEND ls_err TO gt_err.
  ENDIF.

ENDFORM.                    " check_date

*---------------------------------------------------------------------*
*      Form  file_open_dialog
*---------------------------------------------------------------------*
FORM file_open_dialog CHANGING lp_fname TYPE rlgrap-filename.

  DATA: lt_table  TYPE STANDARD TABLE OF file_table,
        lv_source TYPE file_table,
        lv_rc     TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = lt_table
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT lt_table INTO lv_source.
      lp_fname = lv_source.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "file_open_dialog

*---------------------------------------------------------------------*
*       Form top_of_page
*---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  DATA l_len TYPE i.

  IF gp_test = abap_true.
    WRITE / TEXT-001 COLOR COL_NEGATIVE.
  ELSE.
    WRITE / TEXT-005 COLOR COL_POSITIVE.
  ENDIF.

  WRITE: / TEXT-002, gp_date,
         / TEXT-006, gp_movdt AS CHECKBOX INPUT OFF,
         / TEXT-003, gp_kurst,
         / TEXT-004, gp_tcurr,
         / TEXT-008, gp_dev,
         / TEXT-009.

  CASE g_data_src.
    WHEN gc_www.
      WRITE gp_dest.
    WHEN gc_pc.
      l_len = strlen( gp_filpc ).
      WRITE gp_filpc(l_len).
  ENDCASE.

  SKIP.

ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
*      Form  fill_item
*---------------------------------------------------------------------*
*      convert to BAPI structure
*---------------------------------------------------------------------*
FORM fill_item USING    p_date_from  TYPE gdatu_cur
                        p_movdt      TYPE xfeld
                        p_kurst      TYPE kurst_curr
                        p_tcurr      TYPE tcurr_curr
                        pt_fcurr     TYPE t_curr
               CHANGING pt_exch_rate TYPE t_bapi1093_0.

  CONSTANTS: lc_dirq   TYPE tcurn-notation VALUE '1',
             lc_indirq TYPE tcurn-notation VALUE '2'.

  DATA: l_date       TYPE gdatu_cur,
        l_notation   TYPE tcurn-notation,

        l_curr1      TYPE fcurr_curr,
        l_curr2      TYPE tcurr_curr,
        l_ffact      TYPE bapi1093_0-from_factor,
        l_tfact      TYPE bapi1093_0-to_factor,

        ls_return    TYPE bapiret1,
        ls_err       TYPE t_err_line,

        l_str1(20)   TYPE c,                             "valid digits
        l_str2(20)   TYPE c,
        l_hstr(20)   TYPE c,
        l_dig1       TYPE i,
        l_dig2       TYPE i,

        ls_fcurr     TYPE t_curr_line,
        ls_exch_rate TYPE bapi1093_0.

* add 1 day
  IF p_movdt = abap_true.
    l_date = p_date_from + 1.
  ELSE.
    l_date = p_date_from.
  ENDIF.

  LOOP AT pt_fcurr INTO ls_fcurr.
*   get quotation
    CALL FUNCTION 'READ_NOTATION'
      EXPORTING
        foreign_currency       = ls_fcurr-waers
        local_currency         = p_tcurr
        date                   = l_date
      IMPORTING
        notation               = l_notation
      EXCEPTIONS
        empty_foreign_currency = 1
        empty_local_currency   = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      ls_err-curr = ls_fcurr-waers.
      ls_err-text = TEXT-e13.
      APPEND ls_err TO gt_err.
      RETURN.
    ENDIF.

*   ratio factors
    IF l_notation = lc_dirq.
      l_curr1 = ls_fcurr-waers.
      l_curr2 = p_tcurr.
    ELSE.
*     switch currencies - note 783877 part I/5
      l_curr1 = p_tcurr.
      l_curr2 = ls_fcurr-waers.
    ENDIF.

    CALL FUNCTION 'BAPI_EXCHANGERATE_GETFACTORS'
      EXPORTING
        rate_type   = p_kurst
        from_curr   = l_curr1
        to_currncy  = l_curr2
        date        = l_date
      IMPORTING
        from_factor = l_ffact
        to_factor   = l_tfact
        return      = ls_return.

    IF ls_return-type IS NOT INITIAL.
      ls_err-curr = ls_fcurr-waers.
      ls_err-text = ls_return-message.
      APPEND ls_err TO gt_err.
      RETURN.
    ENDIF.

    IF l_tfact IS INITIAL.
      ls_err-curr = ls_fcurr-waers.
      CONCATENATE TEXT-e21 l_curr2 '/' l_curr1 INTO ls_err-text SEPARATED BY space.
      APPEND ls_err TO gt_err.
      RETURN.
    ENDIF.

    CLEAR ls_exch_rate.

    ls_exch_rate-rate_type  = p_kurst.
    ls_exch_rate-from_curr  = ls_fcurr-waers.
    ls_exch_rate-to_currncy = p_tcurr.
    ls_exch_rate-valid_from = p_date_from.

    CASE l_notation.
      WHEN lc_dirq.
        ls_exch_rate-from_factor   = l_ffact.
        ls_exch_rate-to_factor     = l_tfact.
        ls_exch_rate-exch_rate     = ls_fcurr-rate * l_ffact / l_tfact.
        l_str2                     = ls_exch_rate-exch_rate.

      WHEN lc_indirq.
*       multiply by switched factors - note 783877 part I/5
        ls_exch_rate-from_factor_v = l_tfact.
        ls_exch_rate-to_factor_v   = l_ffact.
        ls_exch_rate-exch_rate_v   = ls_fcurr-rate / l_tfact * l_ffact.
        l_str2                     = ls_exch_rate-exch_rate_v.

      WHEN OTHERS.
*       error notation
        ls_err-curr = ls_fcurr-waers.
        ls_err-text = TEXT-e14.
        APPEND ls_err TO gt_err.
    ENDCASE.

*   count valid digits
    l_str1 = ls_fcurr-rate.
    l_hstr = l_str2.
    PERFORM: count_valid_digits USING l_str1
                                      l_dig1,
             count_valid_digits USING l_str2
                                      l_dig2.

    IF l_dig1 <> l_dig2.
      ls_err-curr = ls_fcurr-waers.
      CONCATENATE TEXT-e08 ls_fcurr-rate l_hstr INTO ls_err-text
        SEPARATED BY space.
      APPEND ls_err TO gt_err.
    ENDIF.

    APPEND ls_exch_rate TO pt_exch_rate.

**********************************************************************
*** Insere taxa invertida
    IF gv_invert = abap_true.
      APPEND VALUE bapi1093_0(
        rate_type     = ls_exch_rate-rate_type
        valid_from    = ls_exch_rate-valid_from
        from_curr     = ls_exch_rate-to_currncy
        to_currncy    = ls_exch_rate-from_curr
        exch_rate_v   = ls_exch_rate-exch_rate
        exch_rate     = ls_exch_rate-exch_rate_v
        from_factor   = ls_exch_rate-from_factor_v
        from_factor_v = ls_exch_rate-from_factor
        to_factor     = ls_exch_rate-to_factor_v
        to_factor_v   = ls_exch_rate-to_factor
      ) TO pt_exch_rate[].
    ENDIF.
**********************************************************************
  ENDLOOP.

ENDFORM.                    " fill_item

*---------------------------------------------------------------------*
*      Form  convert_data
*---------------------------------------------------------------------*
*      find and convert CSV data
*
*      CSV structure:
*        dt_arquivo
*        codigo
*        tipo
*        simbolo
*        tx_compra
*        tx_venda
*        paridade_compra
*        paridade_venda
*---------------------------------------------------------------------*
FORM convert_data USING    p_date_from    TYPE gdatu_cur
                           pt_csv_tab     TYPE STANDARD TABLE
                  CHANGING pt_curr        TYPE t_curr.

  DATA:
    lt_content TYPE STANDARD TABLE OF t_csv_line,
    ls_curr    TYPE t_curr_line,
    ls_err     TYPE t_err_line.

*   error - missing ISO code in table TCURC
  READ TABLE pt_curr INTO ls_curr WITH KEY isocd = ''.
  IF sy-subrc = 0.
    ls_err-curr = ls_curr-waers.
    ls_err-text = TEXT-e16.
    APPEND ls_err TO gt_err.
    EXIT.
  ENDIF.

  "Monta filtro para data (DD/MM/YYYY)
  DATA(lv_filter_date) = |{ p_date_from+6(2) }/{ p_date_from+4(2) }/{ p_date_from(4) }|.

  "Recupera taxas de cambio do arquivo
  LOOP AT pt_csv_tab[] ASSIGNING FIELD-SYMBOL(<fs_csv_line>).
    "Filtra dia da execucao
    CHECK condense( lv_filter_date ) = condense( <fs_csv_line>(10) ).

    "Recupera valores do arquivo
    SPLIT <fs_csv_line> AT ';' INTO DATA(lv_dt_arquivo)
                                    DATA(lv_codigo)
                                    DATA(lv_tipo)
                                    DATA(lv_simbolo)
                                    DATA(lv_tx_compra)
                                    DATA(lv_tx_venda)
                                    DATA(lv_paridade_compra)
                                    DATA(lv_paridade_venda).

    "Recupera cadastro da moeda
    READ TABLE gt_curr[] ASSIGNING FIELD-SYMBOL(<fs_curr>)
      WITH KEY isocd = condense( lv_simbolo ).
    CHECK sy-subrc IS INITIAL.

    TRY.
        "Recupera taxa de cambio para atualizacao
        CASE gv_valueori.
          WHEN 'P'. "Purchase
            REPLACE ALL OCCURRENCES OF '.' IN lv_tx_compra WITH ' '.
            REPLACE ',' WITH '.' INTO lv_tx_compra.
            CONDENSE lv_tx_compra.
            <fs_curr>-rate = lv_tx_compra.
          WHEN 'S'. "Sales
            REPLACE ALL OCCURRENCES OF '.' IN lv_tx_venda WITH ' '.
            REPLACE ',' WITH '.' INTO lv_tx_venda.
            CONDENSE lv_tx_venda.
            <fs_curr>-rate = lv_tx_venda.
        ENDCASE.

      CATCH cx_sy_conversion_error.
        ls_err-curr = <fs_curr>-waers.
        ls_err-text = TEXT-e22.
        APPEND ls_err TO gt_err.
        EXIT.
    ENDTRY.
  ENDLOOP.

* error - missing exchange rate for ISO code
  READ TABLE pt_curr INTO ls_curr WITH KEY rate = ''.
  IF sy-subrc = 0.
    ls_err-curr = ls_curr-waers.
    CONCATENATE TEXT-e17 ls_curr-isocd INTO ls_err-text
      SEPARATED BY space.
    APPEND ls_err TO gt_err.
    EXIT.
  ENDIF.

ENDFORM.                    " convert_data

*---------------------------------------------------------------------*
*      Form  init_global
*---------------------------------------------------------------------*
*      initialization of global data
*---------------------------------------------------------------------*
FORM init_global.

  IF rb_www = abap_true.
    g_data_src = gc_www.
  ELSEIF rb_pc = abap_true.
    g_data_src = gc_pc.
  ENDIF.

  CASE abap_true.
    WHEN gp_orip.
      gv_valueori = 'P'.
    WHEN gp_oris.
      gv_valueori = 'S'.
  ENDCASE.

  gv_invert = gp_inv.
  gv_email  = gp_mail.

ENDFORM.                    " init_global

*---------------------------------------------------------------------*
*      Form  fill_iso_code
*---------------------------------------------------------------------*
*      fill ISO code for currencies
*---------------------------------------------------------------------*
FORM fill_iso_code USING pt_fcurr TYPE t_fcurr
                         pt_curr  TYPE t_curr.

  SELECT * FROM tcurc
    INTO CORRESPONDING FIELDS OF TABLE pt_curr
    WHERE waers IN pt_fcurr.

ENDFORM.                    " fill_iso_code

*---------------------------------------------------------------------*
*      Form  check_input_screen
*---------------------------------------------------------------------*
*      check screen
*---------------------------------------------------------------------*
FORM check_input_screen.

  DATA lv_ssl TYPE rfcdisplay-rfcsnc.   "SSL active/inactive

* secure connection
  IF g_data_src = gc_www.
    CALL FUNCTION 'RFC_READ_HTTP_DESTINATION'
      EXPORTING
        destination             = gp_dest
      IMPORTING
        ssl                     = lv_ssl
      EXCEPTIONS
        authority_not_available = 1
        destination_not_exist   = 2
        information_failure     = 3
        internal_failure        = 4
        no_http_destination     = 5
        OTHERS                  = 6.

    IF ( sy-subrc = 0 ) AND lv_ssl = abap_false.
      MESSAGE w398(00) WITH TEXT-e19 TEXT-e20 gp_dest space.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_input_screen

*---------------------------------------------------------------------*
*      Form  change_date
*---------------------------------------------------------------------*
*      add 1 day to result date
*---------------------------------------------------------------------*
FORM change_date USING pt_exch_rate TYPE t_bapi1093_0.

  DATA ls_exch_rate TYPE bapi1093_0.

  LOOP AT pt_exch_rate INTO ls_exch_rate.
    ls_exch_rate-valid_from = ls_exch_rate-valid_from + 1.
    MODIFY pt_exch_rate FROM ls_exch_rate.
  ENDLOOP.

ENDFORM.                    " change_date

*---------------------------------------------------------------------*
*      Form  process_error
*---------------------------------------------------------------------*
*      error output
*---------------------------------------------------------------------*
FORM process_error.

  DATA: ls_err      TYPE t_err_line,
        l_text(200) TYPE c.

  LOOP AT gt_err INTO ls_err.
    CONCATENATE ls_err-curr ls_err-text INTO l_text SEPARATED BY space.
    CONDENSE l_text.
    PERFORM msg_add_free_text USING co_msg_error l_text.
  ENDLOOP.

  CONCATENATE TEXT-011 '0' INTO l_text SEPARATED BY space.
  PERFORM: msg_add_free_text USING co_msg_error l_text,
           save_log,
           send_log,
           display_log.

* error message for correct batch process
  IF sy-batch = abap_true.
    IF lines( gt_err ) > 0.
      MESSAGE e398(00) WITH TEXT-e18 space space space.
    ENDIF.
  ENDIF.

ENDFORM.                    " process_error

*---------------------------------------------------------------------*
*      Form  output_list
*---------------------------------------------------------------------*
*      output
*---------------------------------------------------------------------*
FORM output_list USING pt_exch_rate TYPE t_bapi1093_0.

* data for ALV
  DATA: ls_layout  TYPE slis_layout_alv,
        ls_variant TYPE disvariant,
        ls_events  TYPE slis_alv_event,
        lt_events  TYPE slis_t_event,
        ls_print   TYPE slis_print_alv.

* init ALV
  ls_layout-zebra = abap_true.
  ls_variant-report  = syst-repid.
  ls_print-no_print_listinfos = abap_true.

  ls_events-name = 'TOP_OF_PAGE'.
  ls_events-form = 'TOP_OF_PAGE'.
  APPEND ls_events TO  lt_events.

* output
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = syst-repid
      i_structure_name   = 'BAPI1093_0'
      i_save             = 'A'
      is_layout          = ls_layout
      is_variant         = ls_variant
      it_events          = lt_events[]
      is_print           = ls_print
    TABLES
      t_outtab           = pt_exch_rate.

ENDFORM.                    " output_list

*---------------------------------------------------------------------*
*      Form  count_valid_digits
*---------------------------------------------------------------------*
*      count valid digits between source exchange rate and calculated
*      exchange rate (multiplying by factors and cutting of "last"
*      decimal numbers
*---------------------------------------------------------------------*
FORM count_valid_digits USING p_str TYPE c
                              p_dig TYPE i.

* remove decimal separator
  TRANSLATE p_str USING '. , '.

* delete leading zeroes
  CONDENSE p_str NO-GAPS.
  SHIFT p_str LEFT DELETING LEADING '0'.

* delete trailing zeroes
  WRITE p_str TO p_str RIGHT-JUSTIFIED NO-SIGN.
  SHIFT p_str RIGHT DELETING TRAILING '0'.

  CONDENSE p_str NO-GAPS.
  p_dig = strlen( p_str ).

ENDFORM.                    " count_valid_digits

*---------------------------------------------------------------------*
*      Form  create_log
*---------------------------------------------------------------------*
*      create application log
*---------------------------------------------------------------------*
FORM create_log.

  CONSTANTS lc_object TYPE balobj_d VALUE 'FEDI'.

  DATA: ls_log      TYPE bal_s_log,
        l_text(200) TYPE c,
        l_str(20)   TYPE c.

* define header data of log
  ls_log-extnumber = TEXT-010.
  ls_log-aluser    = sy-uname.
  ls_log-alprog    = sy-repid.
  ls_log-object    = lc_object.

* create log
  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = gs_loghandle
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* output of parameters
  IF gp_test = abap_true.
    l_text = TEXT-001.
  ELSE.
    l_text = TEXT-005.
  ENDIF.
  PERFORM msg_add_free_text USING co_msg_information l_text.

  WRITE gp_date TO l_str DD/MM/YYYY.
  CONCATENATE TEXT-002 l_str INTO l_text SEPARATED BY space.
  PERFORM msg_add_free_text USING co_msg_information l_text.

  CONCATENATE TEXT-006 gp_movdt INTO l_text SEPARATED BY space.
  PERFORM msg_add_free_text USING co_msg_information l_text.

  CONCATENATE TEXT-003 gp_kurst INTO l_text SEPARATED BY space.
  PERFORM msg_add_free_text USING co_msg_information l_text.

  CONCATENATE TEXT-004 gp_tcurr INTO l_text SEPARATED BY space.
  PERFORM msg_add_free_text USING co_msg_information l_text.

  CONCATENATE TEXT-008 gp_dev INTO l_text SEPARATED BY space.
  PERFORM msg_add_free_text USING co_msg_information l_text.

  CASE g_data_src.
    WHEN gc_www.
      l_str = gp_dest.
    WHEN gc_pc.
      l_str = gp_filpc.
  ENDCASE.

  CONCATENATE TEXT-009 l_str INTO l_text SEPARATED BY space.
  PERFORM msg_add_free_text USING co_msg_information l_text.

ENDFORM.                    " create_log

*---------------------------------------------------------------------*
*      Form display_log
*---------------------------------------------------------------------*
*      display application log
*---------------------------------------------------------------------*
FORM display_log.

  DATA ls_display_profile TYPE bal_s_prof.

* get standard display profile
  CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
    IMPORTING
      e_s_display_profile = ls_display_profile
    EXCEPTIONS
      OTHERS              = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* optimized column width
  ls_display_profile-cwidth_opt = abap_true.


* set report for display variants
  ls_display_profile-disvariant-report = sy-repid.

* when you use also other ALV lists in your report,
* please specify a handle to distinguish between the display
* variants of these different lists
  ls_display_profile-disvariant-handle = 'LOG'.

* display log
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile = ls_display_profile
    EXCEPTIONS
      OTHERS              = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_log

*---------------------------------------------------------------------*
*      Form  msg_add_free_text
*---------------------------------------------------------------------*
*      add free text to application log
*---------------------------------------------------------------------*
FORM msg_add_free_text USING p_msgtype TYPE symsgty
                             p_text    TYPE c.

  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_msgty       = p_msgtype
      i_text        = p_text
    EXCEPTIONS
      log_not_found = 0
      OTHERS        = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " msg_add_free_text

*---------------------------------------------------------------------*
*      Form  save_log
*---------------------------------------------------------------------*
*      save application log
*---------------------------------------------------------------------*
FORM save_log.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all       = abap_true
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " save_log

*&---------------------------------------------------------------------*
*&      Form  SEND_LOG
*&---------------------------------------------------------------------*
FORM send_log.

  DATA lt_body       TYPE soli_tab.
  DATA lt_attach     TYPE soli_tab.
  DATA lv_subject    TYPE so_obj_des.
  DATA lt_recipients TYPE bcsy_smtpa.

  CHECK gv_email = abap_true.

  "Prepara corpo do email
  PERFORM email_prep_body TABLES lt_body[].

  "Prepara anexo do email
  PERFORM email_prep_attachment TABLES lt_attach[].

  "Prepara assunto
  PERFORM email_prep_subject CHANGING lv_subject.

  "Prepara destinatarios
  PERFORM email_prep_recipients TABLES lt_recipients[].

  "Envia email
  PERFORM email_send USING lv_subject
                           sy-uname
                           lt_recipients[]
                           lt_body[]
                           lt_attach[].

ENDFORM.                    " send_log

*&---------------------------------------------------------------------*
*&      Form  EMAIL_PREP_BODY
*&---------------------------------------------------------------------*
FORM email_prep_body TABLES ct_body TYPE soli_tab.

  DATA lt_loghandle TYPE bal_t_logh.
  DATA lt_msghandle TYPE bal_t_msgh.
  DATA lv_message   TYPE text255.

  CLEAR ct_body[].

  CHECK gs_loghandle IS NOT INITIAL.

  APPEND gs_loghandle TO lt_loghandle[].

  "Recupera handle das msgs do log
  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = lt_loghandle[]
    IMPORTING
      e_t_msg_handle = lt_msghandle[]
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  CHECK sy-subrc IS INITIAL.

  "Monta corpo do email
  LOOP AT lt_msghandle[] ASSIGNING FIELD-SYMBOL(<fs_msghandle>).
    CLEAR lv_message.

    "Recupera msg do log
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = <fs_msghandle>
        i_langu        = sy-langu
      IMPORTING
        e_txt_msg      = lv_message
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF ct_body[] IS INITIAL.
      "Segue log do processamento da atualização da
      APPEND TEXT-c02 TO ct_body[].
      "taxa de câmbio do BCB para o dia &1
      APPEND |{ TEXT-c03 } { gp_date DATE = USER }:| TO ct_body[].
    ENDIF.

    "Insere msg no corpo do email
    APPEND condense( lv_message ) TO ct_body[].
  ENDLOOP.
  CHECK ct_body[] IS NOT INITIAL.

  "Linha em branco
  APPEND space TO ct_body[].

  "Sistema SAP
  APPEND TEXT-c04 TO ct_body[].

ENDFORM.                    " email_prep_body

*&---------------------------------------------------------------------*
*&      Form  EMAIL_PREP_ATTACHMENT
*&---------------------------------------------------------------------*
FORM email_prep_attachment TABLES ct_attachment TYPE soli_tab.

  CLEAR ct_attachment[].

  CHECK gt_exch_rate[] IS NOT INITIAL.

  "Recupera colunas
  DATA(lt_fcat) = CAST cl_abap_structdescr(
    CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( gt_exch_rate[] ) )->get_table_line_type( )
  )->get_ddic_field_list( ).

  "Insere cabecalho
  APPEND INITIAL LINE TO ct_attachment[] ASSIGNING FIELD-SYMBOL(<fs_attach_line>).
  LOOP AT lt_fcat[] ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF <fs_attach_line>-line IS INITIAL.
      <fs_attach_line>-line = condense( <fs_fcat>-fieldname ).
    ELSE.
      <fs_attach_line>-line = |{ <fs_attach_line>-line };{ condense( <fs_fcat>-fieldname ) }|.
    ENDIF.
  ENDLOOP.
  <fs_attach_line>-line = |{ <fs_attach_line>-line } { cl_abap_char_utilities=>newline }|.

  "Preenche itens da bapi no anexo
  LOOP AT gt_exch_rate[] ASSIGNING FIELD-SYMBOL(<fs_exch_rate>).
    APPEND INITIAL LINE TO ct_attachment[] ASSIGNING <fs_attach_line>.

    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_exch_rate> TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF <fs_attach_line>-line IS INITIAL.
        <fs_attach_line>-line = condense( <fs_field> ).
      ELSE.
        <fs_attach_line>-line = |{ <fs_attach_line>-line };{ condense( <fs_field> ) }|.
      ENDIF.
    ENDDO.

    <fs_attach_line>-line = |{ <fs_attach_line>-line } { cl_abap_char_utilities=>newline }|.
  ENDLOOP.

ENDFORM.                    " email_prep_attachment

*&---------------------------------------------------------------------*
*&      Form  EMAIL_PREP_SUBJECT
*&---------------------------------------------------------------------*
FORM email_prep_subject CHANGING cv_subject TYPE so_obj_des.

  CLEAR cv_subject.
  cv_subject = TEXT-c01.  "[SAP] Updt. Exchange Rates from BCB

ENDFORM.                    " email_prep_subject

*&---------------------------------------------------------------------*
*&      Form  EMAIL_PREP_RECIPIENTS
*&---------------------------------------------------------------------*
FORM email_prep_recipients TABLES ct_recipients TYPE bcsy_smtpa.

  SELECT email
    FROM ztb_fi_0003
    INTO TABLE ct_recipients[]
    WHERE codbanco = 'BR'.

ENDFORM.                    " email_prep_recipients

*&---------------------------------------------------------------------*
*&      Form  EMAIL_SEND
*&---------------------------------------------------------------------*
FORM email_send USING iv_subject     TYPE so_obj_des
                      iv_sender      TYPE uname
                      it_recipients  TYPE bcsy_smtpa
                      it_body        TYPE soli_tab
                      it_attach      TYPE soli_tab.

  TRY.
      "Cria doc de email
      DATA(lo_document) =  cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = it_body[]
        i_subject = iv_subject
      ).

      TRY.
          "Insere anexo no doc
          lo_document->add_attachment(
            EXPORTING
              i_attachment_type     = 'CSV' "downlod como excel
              i_attachment_subject  = CONV #( TEXT-c07 )  "Rates
              i_att_content_hex     = cl_bcs_convert=>soli_to_solix( it_attach[] )
          ).
        CATCH cx_bcs INTO DATA(lx_cx_bcs).
          MESSAGE lx_cx_bcs->get_text( ) TYPE 'E'.
      ENDTRY.

    CATCH cx_document_bcs INTO DATA(lx_document_bcs).
      MESSAGE lx_document_bcs->get_text( ) TYPE 'E'.
  ENDTRY.

  TRY.
      "Intancia CL_BCS
      DATA(lo_request) = cl_bcs=>create_persistent( ).

      "Insere corpo do email
      lo_request->set_document( lo_document ).

      "Insere remetente: usuario que esta enviando o email
      lo_request->set_sender(
        cl_sapuser_bcs=>create( iv_sender )
      ).

      "Insere destinatarios
      LOOP AT it_recipients[] ASSIGNING FIELD-SYMBOL(<fs_recipient>).
        lo_request->add_recipient(
          cl_cam_address_bcs=>create_internet_address(
            i_address_string = <fs_recipient>
          )
        ).
      ENDLOOP.

      "Envia email
      DATA(lv_sent) = lo_request->send( abap_false ).
      IF lv_sent = abap_false.
        ROLLBACK WORK.

        "Erro ao enviar email de log
        MESSAGE TEXT-c05 TYPE 'E'.

      ELSE.
        COMMIT WORK AND WAIT.

        "Email de log enviado
        MESSAGE TEXT-c06 TYPE 'S'.

      ENDIF.

    CATCH cx_address_bcs  INTO DATA(lx_address_bcs).
      MESSAGE lx_address_bcs->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
      MESSAGE lx_send_req_bcs->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.                    " email_send