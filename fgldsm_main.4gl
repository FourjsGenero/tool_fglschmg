IMPORT os
IMPORT FGL fgltbasics
IMPORT FGL fgltdialogs
IMPORT FGL fgltfiledlg
IMPORT FGL fgldsm_schema

GLOBALS "fgldsm_globals.4gl"

CONSTANT toolname      = "fglschmg"
CONSTANT tooltitle     = "Database Schema Manager"
CONSTANT toolversion   = "1.1"

DEFINE curschema STRING -- path to .dbs schema header file
DEFINE curtable INTEGER
DEFINE tabdef_saved INTEGER

DEFINE tabname STRING
DEFINE old_tabname STRING
DEFINE pkeyname STRING
CONSTANT tabdef_msgtitle = "Table"

DEFINE columns t_collist
DEFINE old_colname STRING
CONSTANT cols_msgtitle = "Columns"
DEFINE cols_inserting SMALLINT

DEFINE skeys t_skeys
CONSTANT skeys_msgtitle = "Unique constraints"

DEFINE fkeys t_fkeys
CONSTANT fkeys_msgtitle = "Foreign constraints"

DEFINE checks t_checks
CONSTANT checks_msgtitle = "Check constraints"

DEFINE tabopts t_tabopts
DEFINE tabopts_dbtype t_dbtype
DEFINE tabopts_sqltext STRING
CONSTANT topts_msgtitle = "Table SQL Options"


MAIN
  DEFINE c,saved INTEGER

  DEFER INTERRUPT
  DEFER QUIT

  IF fglt_cmdarg_option_used("V") THEN
     DISPLAY toolname || " " || toolversion
     EXIT PROGRAM 0
  END IF

  IF fglt_cmdarg_option_used( "h" ) THEN
     CALL display_usage()
     EXIT PROGRAM 0
  END IF

  OPTIONS HELP FILE "fgldsm_help.iem"

  CALL addCommonStyles()
  CALL addStyleList1()

  OPTIONS INPUT WRAP
  CLOSE WINDOW screen

  CALL dtdef_init()

  CALL ui.Interface.setText(tooltitle)

  LET curschema = fglt_cmdarg_option_param("ds")

  CALL params_init(curschema)

  IF fglt_cmdarg_option_used( "gs" ) THEN
     IF curschema IS NULL THEN
        CALL display_usage()
        EXIT PROGRAM 1
     END IF
     IF schema_load(curschema) != 0 THEN
        EXIT PROGRAM 1
     END IF
     IF sql_generate_script(curschema) != 0 THEN
        EXIT PROGRAM 1
     END IF
     EXIT PROGRAM 0
  END IF

  OPEN WINDOW w_sched WITH FORM "fgldsm_main" ATTRIBUTES(STYLE="main2")

  LET saved = TRUE

  IF curschema IS NOT NULL THEN
     IF schema_load(curschema) != 0 THEN
        EXIT PROGRAM 1
     END IF
  ELSE
     LET c = fglt_choice(tooltitle,
             "The schema manager was started without -ds option.\n"
           ||"What do you want to do?",
             "Extract a new schema from an existing database|"
           ||"Load an existing schema|"
           ||"Edit a new empty schema"
             , 2)
     CASE c
         WHEN 1
           LET saved = FALSE
           IF sql_extract_schema() != 0 THEN
              EXIT PROGRAM 1
           END IF
         WHEN 2
           IF ask_schema_to_load() THEN
              IF schema_load(curschema) != 0 THEN
                 EXIT PROGRAM 1
              END IF
           ELSE
              EXIT PROGRAM
           END IF
         WHEN 3
           LET saved = FALSE
           LET hprms.schname = "dummy"
           LET hprms.schcreat = CURRENT YEAR TO SECOND
           LET hprms.schmodif = hprms.schcreat
           LET hprms.schrevision = 0
           LET hprms.schquotedids = "n"
           LET curschema = hprms.schname || fext_schema
           LET curtable = 1
           CALL tables.appendElement()
           LET tables[curtable].tabname = table_get_newname(curtable)
           CALL tables[curtable].cols.appendElement()
           CALL cols_get_defaults(curtable,1) RETURNING tables[curtable].cols[1].*
         OTHERWISE
            EXIT PROGRAM
     END CASE
  END IF

  IF tables.getLength() == 0 THEN
     CALL __mbox_ok(tooltitle,"No column type definition found","information")
     EXIT PROGRAM
  END IF

  CALL tabdef_edit(saved)

  CALL params_save()

END MAIN

FUNCTION params_filename()
  CONSTANT sqlgp_resource = ".fglschmg"
  RETURN os.Path.homeDir() || os.Path.separator() || sqlgp_resource
END FUNCTION

FUNCTION params_init(schema)
  DEFINE schema STRING
  DEFINE ch base.Channel
  DEFINE pname, pvalue, sqlfile STRING

  LET sqlfile = os.Path.baseName(schema)
  LET sqlfile = get_schname_from_fname(sqlfile) || ".sql"

  LET glb_params.newtabname = "table%1"
  LET glb_params.newpkeyname = "%1_pky"
  LET glb_params.newskeyname = "%1_skey%2"
  LET glb_params.newfkeyname = "%1_fkey%2"
  LET glb_params.newcheckname = "%1_check%2"
  LET glb_params.newcolname = "%1_col%2"
  LET glb_params.newcoltp = dtn_varchar
  LET glb_params.newcolnn = TRUE

  LET dlg_params.fileopendir = NULL

  LET ext_params.cvmeth = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
  LET ext_params.systables = FALSE
  LET ext_params.ignerrors = TRUE

  LET sql_params.dbtype      = "IFX"
  LET sql_params.filename    = sqlfile
  LET sql_params.namedconst  = TRUE
  LET sql_params.createfkeys = TRUE
  LET sql_params.droptables  = FALSE
  LET sql_params.quotedids   = FALSE
  LET sql_params.upcaseids   = FALSE
  LET sql_params.tableopts   = TRUE

  LET ch = base.Channel.create()
  CALL ch.setDelimiter("^")

  WHENEVER ERROR CONTINUE
  CALL ch.openFile(params_filename(),"r")
  WHENEVER ERROR STOP
  IF status < 0 THEN RETURN END IF

  WHILE ch.read([pname,pvalue])

      CASE pname

           WHEN "glb_params.newtabname"   LET glb_params.newtabname   = pvalue
           WHEN "glb_params.newpkeyname"  LET glb_params.newpkeyname  = pvalue
           WHEN "glb_params.newskeyname"  LET glb_params.newskeyname  = pvalue
           WHEN "glb_params.newfkeyname"  LET glb_params.newfkeyname  = pvalue
           WHEN "glb_params.newcheckname" LET glb_params.newcheckname = pvalue
           WHEN "glb_params.newcolname"   LET glb_params.newcolname   = pvalue
           WHEN "glb_params.newcoltp"     LET glb_params.newcoltp     = pvalue
           WHEN "glb_params.newcolnn"     LET glb_params.newcolnn     = pvalue

           WHEN "dlg_params.fileopendir"  LET dlg_params.fileopendir  = pvalue

           WHEN "ext_params.dbname"       LET ext_params.dbname       = pvalue
           WHEN "ext_params.dbdriver"     LET ext_params.dbdriver     = pvalue
           WHEN "ext_params.username"     LET ext_params.username     = pvalue
           WHEN "ext_params.password"     LET ext_params.password     = pvalue
           WHEN "ext_params.dbowner"      LET ext_params.dbowner      = pvalue
           WHEN "ext_params.cvmeth"       LET ext_params.cvmeth       = pvalue
           WHEN "ext_params.tabname"      LET ext_params.tabname      = pvalue
           WHEN "ext_params.systables"    LET ext_params.systables    = pvalue
           WHEN "ext_params.ignerrors"    LET ext_params.ignerrors    = pvalue

           WHEN "sql_params.dbtype"       LET sql_params.dbtype       = pvalue
           WHEN "sql_params.filename"     LET sql_params.filename     = pvalue
           WHEN "sql_params.namedconst"   LET sql_params.namedconst   = pvalue
           WHEN "sql_params.createfkeys"  LET sql_params.createfkeys  = pvalue
           WHEN "sql_params.droptables"   LET sql_params.droptables   = pvalue
           WHEN "sql_params.quotedids"    LET sql_params.quotedids    = pvalue
           WHEN "sql_params.updcaseids"   LET sql_params.upcaseids    = pvalue
           WHEN "sql_params.tableopts"    LET sql_params.tableopts    = pvalue

      END CASE

  END WHILE

  CALL ch.close()

  LET pvalue = fglt_cmdarg_option_param("dt")
  IF pvalue IS NOT NULL THEN
     LET sql_params.dbtype = pvalue
  END IF

  LET pvalue = fglt_cmdarg_option_param("of")
  IF pvalue IS NOT NULL THEN
     LET sql_params.filename = pvalue
  END IF

  IF fglt_cmdarg_option_used( "gnc" ) THEN LET sql_params.namedconst = TRUE END IF
  IF fglt_cmdarg_option_used( "gfk" ) THEN LET sql_params.createfkeys = TRUE END IF
  IF fglt_cmdarg_option_used( "gdt" ) THEN LET sql_params.droptables = TRUE END IF
  IF fglt_cmdarg_option_used( "gqi" ) THEN LET sql_params.quotedids = TRUE END IF
  IF fglt_cmdarg_option_used( "gui" ) THEN LET sql_params.upcaseids = TRUE END IF
  IF fglt_cmdarg_option_used( "gto" ) THEN LET sql_params.tableopts = TRUE END IF

END FUNCTION

FUNCTION params_save()
  DEFINE ch base.Channel

  LET ch = base.Channel.create()
  CALL ch.setDelimiter("^")

  WHENEVER ERROR CONTINUE
  CALL ch.openFile(params_filename(),"w")
  WHENEVER ERROR STOP
  IF status < 0 THEN RETURN END IF

  CALL ch.write( ["glb_params.newtabname",   glb_params.newtabname] )
  CALL ch.write( ["glb_params.newpkeyname",  glb_params.newpkeyname] )
  CALL ch.write( ["glb_params.newskeyname",  glb_params.newskeyname] )
  CALL ch.write( ["glb_params.newfkeyname",  glb_params.newfkeyname] )
  CALL ch.write( ["glb_params.newcheckname", glb_params.newcheckname] )
  CALL ch.write( ["glb_params.newcolname",   glb_params.newcolname] )
  CALL ch.write( ["glb_params.newcoltp",     glb_params.newcoltp] )
  CALL ch.write( ["glb_params.newcolnn",     glb_params.newcolnn] )

  CALL ch.write( ["dlg_params.fileopendir",  dlg_params.fileopendir] )

  CALL ch.write( ["ext_params.dbname",        ext_params.dbname] )
  CALL ch.write( ["ext_params.dbdriver",      ext_params.dbdriver] )
  CALL ch.write( ["ext_params.username",      ext_params.username] )
  CALL ch.write( ["ext_params.password",      ext_params.password] )
  CALL ch.write( ["ext_params.dbowner",       ext_params.dbowner] )
  CALL ch.write( ["ext_params.cvmeth",        ext_params.cvmeth] )
  CALL ch.write( ["ext_params.tabname",       ext_params.tabname] )
  CALL ch.write( ["ext_params.systables",     ext_params.systables] )
  CALL ch.write( ["ext_params.ignerrors",     ext_params.ignerrors] )

  CALL ch.write( ["sql_params.dbtype",        sql_params.dbtype] )
  CALL ch.write( ["sql_params.filename",      sql_params.filename] )
  CALL ch.write( ["sql_params.namedconst",    sql_params.namedconst] )
  CALL ch.write( ["sql_params.createfkeys",   sql_params.createfkeys] )
  CALL ch.write( ["sql_params.droptables",    sql_params.droptables] )
  CALL ch.write( ["sql_params.quotedids",     sql_params.quotedids] )
  CALL ch.write( ["sql_params.updcaseids",    sql_params.upcaseids] )
  CALL ch.write( ["sql_params.tableopts",     sql_params.tableopts] )

  CALL ch.close()

END FUNCTION

FUNCTION ask_schema_to_load()
  DEFINE filename STRING
  LET filename = fglt_file_opendlg(
                      NULL,
                      dlg_params.fileopendir,
                      NULL,
                      fext_dsfilt,"cd,dd,df,sh,oe")
  IF filename IS NOT NULL THEN
     LET curschema = filename
     LET dlg_params.fileopendir = os.Path.dirName(filename)
     RETURN TRUE
  ELSE
     RETURN FALSE
  END IF
END FUNCTION

FUNCTION ask_schema_to_saveas()
  DEFINE filename STRING
  LET filename = get_schname_from_fname(curschema) || "_copy.dbs"
  LET filename = fglt_file_savedlg(
                      NULL,
                      os.Path.dirName(filename),
                      os.Path.baseName(filename),
                      fext_dsfilt,"cd,dd,df,sh,oe")
  IF filename IS NOT NULL THEN
     LET curschema = filename
     RETURN TRUE
  ELSE
     RETURN FALSE
  END IF
END FUNCTION

FUNCTION tabdef_touch_setup(d)
  DEFINE d ui.Dialog
  LET tabdef_saved = FALSE
  CALL tabdef_setup(d)
END FUNCTION

FUNCTION tabdef_clean_setup(d)
  DEFINE d ui.Dialog
  LET tabdef_saved = TRUE
  LET cols_inserting = FALSE
  CALL tabdef_setup(d)
  CALL cols_setup(d)
END FUNCTION

FUNCTION tabdef_setup(d)
  DEFINE d ui.Dialog
  CALL d.setActionActive("dialogtouched", tabdef_saved)
  CALL d.setActionActive("save", NOT tabdef_saved)
  CALL d.setActionActive("load", tabdef_saved)
  CALL d.setActionActive("new_table", tabdef_saved)
  CALL d.setActionActive("copy_table", tabdef_saved)
  CALL d.setActionActive("select_table", tabdef_saved)
  CALL d.setActionActive("first_table", curtable > 1)
  CALL d.setActionActive("prev_table", curtable > 1)
  CALL d.setActionActive("next_table", curtable < tables.getLength())
  CALL d.setActionActive("last_table", curtable < tables.getLength())
  CALL tabopts_sync_fields(d)
END FUNCTION

FUNCTION cols_setup(d)
  DEFINE d ui.Dialog
  DEFINE r, b1,b2 INTEGER
  LET r = d.getCurrentRow("sa_cols")
  IF r>0 THEN
     IF columns[r].typenum IS NOT NULL THEN 
        LET b1 = dtdef_has_length_qual(columns[r].typenum,1)
        LET b2 = dtdef_has_length_qual(columns[r].typenum,2)
     END IF
  END IF
  CALL d.setFieldActive("typel1", b1)
  CALL d.setFieldActive("typel2", b2)
  CALL d.setActionActive("sa_cols.move_up", NOT cols_inserting AND (r>1))
  CALL d.setActionActive("sa_cols.move_down", NOT cols_inserting AND (r<d.getArrayLength("sa_cols")))
END FUNCTION

FUNCTION cols_exchange(d, dir)
  DEFINE d ui.Dialog
  DEFINE dir CHAR(1)
  DEFINE r INTEGER
  DEFINE tmp t_column
  IF cols_inserting THEN RETURN END IF
  LET r = d.getCurrentRow("sa_cols")
  IF dir == "U" THEN
     IF r == 1 THEN RETURN END IF
     LET tmp.* = columns[r].*
     LET columns[r].* = columns[r-1].* 
     LET columns[r].colindex = r
     LET columns[r-1].* = tmp.*
     LET columns[r-1].colindex = r-1
     CALL d.setCurrentRow("sa_cols",r-1)
  END IF
  IF dir == "D" THEN
     IF r == d.getArrayLength("sa_cols") THEN RETURN END IF
     LET tmp.* = columns[r].*
     LET columns[r].* = columns[r+1].* 
     LET columns[r].colindex = r
     LET columns[r+1].* = tmp.*
     LET columns[r+1].colindex = r+1
     CALL d.setCurrentRow("sa_cols",r+1)
  END IF
  LET tabdef_saved = FALSE
  CALL tabdef_invars_save(d)
  CALL tabdef_setup(d)
  CALL cols_setup(d)
END FUNCTION

FUNCTION tabdef_invars_save(d)
  DEFINE d ui.Dialog
  DEFINE i INTEGER
  LET tables[curtable].tabname = tabname
  LET tables[curtable].pkeyname = pkeyname
  CALL tables[curtable].cols.clear()
  FOR i=1 TO columns.getLength()
      LET tables[curtable].cols[i].* = columns[i].*
  END FOR
  CALL tables[curtable].skeys.clear()
  FOR i=1 TO skeys.getLength()
      LET tables[curtable].skeys[i].* = skeys[i].*
  END FOR
  CALL tables[curtable].checks.clear()
  FOR i=1 TO checks.getLength()
      LET tables[curtable].checks[i].* = checks[i].*
  END FOR
  CALL tables[curtable].fkeys.clear()
  FOR i=1 TO fkeys.getLength()
      LET tables[curtable].fkeys[i].* = fkeys[i].*
  END FOR
  LET i = d.getCurrentRow("sa_topts")
  IF i>0 THEN
     LET tabopts[i].dbtype = tabopts_dbtype
     LET tabopts[i].sqltext = tabopts_sqltext
  END IF
  CALL tables[curtable].tabopts.clear()
  FOR i=1 TO tabopts.getLength()
      LET tables[curtable].tabopts[i].* = tabopts[i].*
  END FOR
END FUNCTION

FUNCTION tabdef_invars_read(d)
  DEFINE d ui.Dialog
  DEFINE i INTEGER
  LET tabname = tables[curtable].tabname
  LET old_tabname = tabname
  LET pkeyname = tables[curtable].pkeyname
  CALL columns.clear()
  FOR i=1 TO tables[curtable].cols.getLength()
      LET columns[i].* = tables[curtable].cols[i].*
      LET columns[i].colindex = i
  END FOR
  CALL skeys.clear()
  FOR i=1 TO tables[curtable].skeys.getLength()
      LET skeys[i].* = tables[curtable].skeys[i].*
  END FOR
  CALL fkeys.clear()
  FOR i=1 TO tables[curtable].fkeys.getLength()
      LET fkeys[i].* = tables[curtable].fkeys[i].*
  END FOR
  CALL checks.clear()
  FOR i=1 TO tables[curtable].checks.getLength()
      LET checks[i].* = tables[curtable].checks[i].*
  END FOR
  CALL tabopts.clear()
  LET tabopts_sqltext = NULL
  FOR i=1 TO tables[curtable].tabopts.getLength()
      LET tabopts[i].* = tables[curtable].tabopts[i].*
      IF i==1 THEN
         LET tabopts_dbtype  = tabopts[i].dbtype
         LET tabopts_sqltext = tabopts[i].sqltext
      END IF
  END FOR
  IF d IS NOT NULL THEN
     CALL d.setFieldTouched("tabname", FALSE)
     CALL d.setFieldTouched("pkeyname", FALSE)
     CALL d.setFieldTouched("sa_cols.*", FALSE)
     CALL d.setFieldTouched("sa_skeys.*", FALSE)
     CALL d.setFieldTouched("sa_fkeys.*", FALSE)
     CALL d.setFieldTouched("sa_checks.*", FALSE)
     CALL d.setFieldTouched("sa_topts.*", FALSE)
     LET cols_inserting = FALSE
  END IF
END FUNCTION

FUNCTION tabdef_check_tabname(d)
  DEFINE d ui.Dialog
  DEFINE i,n INTEGER
  LET d = NULL
  IF NOT is_identifier_1(tabname) THEN
     CALL __mbox_ok(cols_msgtitle, "The name of the table must be an identifier", "stop")
     RETURN FALSE
  END IF
  LET i = table_lookup_ex(tabname, curtable)
  IF i>0 THEN
     CALL __mbox_ok(cols_msgtitle, "This table name is already used", "stop")
     RETURN FALSE
  END IF
  IF old_tabname IS NOT NULL AND tabname != old_tabname THEN
     CALL fkeys_lookup_reftable(old_tabname) RETURNING i, n
     IF i>0 THEN
        CALL __mbox_ok(cols_msgtitle,
             SFMT("This table name is used as reference table foreign key '%1' of table '%2'",
             tables[i].fkeys[n].fkeyname, tables[i].tabname), "stop")
        RETURN FALSE
     END IF
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION tabdef_check_pkeyname(d)
  DEFINE d ui.Dialog
  DEFINE t,c,p INTEGER
  IF table_has_pkeycols(curtable) AND length(pkeyname) == 0 THEN
     CALL __mbox_ok(tabdef_msgtitle, "Primary key columns defined without a constraint name", "stop")
     RETURN FALSE
  END IF
  IF length(pkeyname) == 0 THEN RETURN TRUE END IF
  IF NOT is_identifier_1(pkeyname) THEN
     CALL __mbox_ok(tabdef_msgtitle, "The name of the primary key must be an identifier", "stop")
     RETURN FALSE
  END IF
  CALL tabdef_invars_save(d)
  CALL constraint_lookup_ex(pkeyname,curtable,-1,-1,-1) RETURNING t, c, p
  IF t>0 THEN
     CALL msg_constraint_used(tabdef_msgtitle, t, c, p)
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION tables_check_unique_names(cs)
  DEFINE cs INTEGER
  DEFINE pb, tn, et, en, otn, oet, oen INTEGER
  DEFINE msg STRING
  CALL schema_check_unique_names(cs) RETURNING pb, tn, et, en, otn, oet, oen
  IF pb != 0 THEN
  END IF
  CASE pb
      WHEN -1
          LET msg = SFMT("Table name '%1' is also used by table number %2.",
                         tables[tn].tabname, otn)
      WHEN -2
          LET msg = SFMT("Primary key name '%1' is also used by %2 constraint number %3 of table %4.",
                         tables[tn].pkeyname, msg_constraint_type_name(oet), oen, tables[otn].tabname)
      WHEN -3
          LET msg = SFMT("In table %1, column name '%2' is also used by column number %3.",
                         tables[tn].tabname, tables[tn].cols[en].colname, oen)
      WHEN -4
          LET msg = SFMT("In table '%1', the '%2' secondary key constraint name is also used by %3 constraint number %4 in table '%5'.",
                         tables[tn].tabname, msg_constraint_name(tn,et,en),
                         msg_constraint_type_name(oet), oen, tables[otn].tabname)
      WHEN -5
          LET msg = SFMT("In table '%1', the '%2' foreign key constraint name is also used by %3 constraint number %4 in table '%5'.",
                         tables[tn].tabname, msg_constraint_name(tn,et,en),
                         msg_constraint_type_name(oet), oen, tables[otn].tabname)
      WHEN -6
          LET msg = SFMT("In table '%1', the '%2' check constraint name is also used by %3 constraint number %4 in table '%5'.",
                         tables[tn].tabname, msg_constraint_name(tn,et,en),
                         msg_constraint_type_name(oet), oen, tables[otn].tabname)
  END CASE
  IF msg IS NOT NULL THEN
     LET msg = "Duplicate database objet name found in current schema:\n", msg
     CALL __mbox_ok(tabdef_msgtitle, msg, "stop")
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION tabdef_move_to(d, dir, row)
  DEFINE d ui.Dialog
  DEFINE dir CHAR(1)
  DEFINE row INTEGER
  IF NOT tabdef_check(d) THEN RETURN END IF
  IF dir == "a" THEN
     LET curtable = row
  ELSE -- "r"
     LET curtable = curtable + row
  END IF
  IF curtable < 1 THEN
     LET curtable = 1
  END IF
  IF curtable > tables.getLength() THEN
     LET curtable = tables.getLength()
  END IF
  CALL tabdef_invars_read(d)
  CALL d.setCurrentRow("sa_cols",1)
  CALL d.setCurrentRow("sa_topts",1)
  CALL d.setCurrentRow("sa_skeys",1)
  CALL tabdef_setup(d)
END FUNCTION

FUNCTION check_temprow(d,sa)
  DEFINE d ui.Dialog
  DEFINE sa STRING
--display SFMT("check_temprow: curr = %1  count = %2 (arr_count()=%3)", d.getCurrentRow(sa), d.getArrayLength(sa), arr_count())
  IF d.getCurrentRow(sa) <= d.getArrayLength(sa) AND d.getCurrentRow(sa) >= 1 THEN
     RETURN d.getCurrentRow(sa)
  ELSE
     RETURN -1
  END IF
END FUNCTION

FUNCTION cols_get_defaults(t,i)
  DEFINE t,i INTEGER
  DEFINE typel1, typel2 INTEGER
  CALL cols_get_deflengths(glb_params.newcoltp) RETURNING typel1, typel2
  RETURN i, cols_get_newname(t, i),
         glb_params.newcoltp, typel1, typel2,
         glb_params.newcolnn, FALSE, NULL
END FUNCTION

FUNCTION cols_get_newname(tabnum, colnum)
  DEFINE tabnum, colnum INTEGER
  DEFINE name STRING
  DEFINE id INTEGER
  LET id = colnum
  WHILE TRUE
     LET name = SFMT(glb_params.newcolname,tables[tabnum].tabname,id)
     IF column_lookup(tabnum,name) == 0 THEN
        EXIT WHILE
     END IF
     LET id = id + 1
  END WHILE
  RETURN name
END FUNCTION

FUNCTION cols_check_colname_not_used(colname)
  DEFINE colname STRING
  DEFINE p INTEGER
  IF colname IS NULL THEN RETURN TRUE END IF
  LET p = skeys_lookup_column_usage(curtable,colname)
  IF p>0 THEN
     CALL msg_column_used(cols_msgtitle, colname, curtable, constype_skey, p)
     RETURN FALSE
  END IF
  LET p = fkeys_lookup_column_usage(curtable,colname)
  IF p>0 THEN
     CALL msg_column_used(cols_msgtitle, colname, curtable, constype_fkey, p)
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_colname(d)
  DEFINE d ui.Dialog
  DEFINE r, i INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_cols.colname")==0 THEN RETURN TRUE END IF
  IF NOT is_identifier_1(columns[r].colname) THEN
     CALL __mbox_ok(cols_msgtitle,"The name of the column must be an identifier.","stop")
     RETURN FALSE
  END IF
  FOR i = 1 TO columns.getLength()
      IF i != r AND names_match(columns[i].colname, columns[r].colname) THEN
         CALL __mbox_ok(cols_msgtitle,"The name of the column must be unique.","stop")
         RETURN FALSE
      END IF
  END FOR
  IF old_colname IS NOT NULL AND columns[r].colname != old_colname THEN
     IF NOT cols_check_colname_not_used(old_colname) THEN
         LET columns[r].colname = old_colname
         RETURN FALSE
     END IF
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_typenum(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_cols.typenum")==0 THEN RETURN TRUE END IF
  CALL cols_get_deflengths(columns[r].typenum)
       RETURNING columns[r].typel1,
                 columns[r].typel2
  LET columns[r].defvalue = NULL
  CALL tabdef_setup(d)
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_typel1(d)
  DEFINE d ui.Dialog
  DEFINE r, s INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_cols.typel1")==0 THEN RETURN TRUE END IF
  LET s = cols_verify_coltype(
               columns[r].typenum,
               columns[r].typel1,
               columns[r].typel2
          )
  IF s == -1 THEN
     CALL __mbox_ok(cols_msgtitle,"The type definition is wrong, check first length.","stop")
     RETURN FALSE
  END IF
  IF s == 1 THEN
     CALL __mbox_ok(cols_msgtitle,"The type definition is wrong, first length cannot be specified.","stop")
     LET columns[r].typel1 = NULL
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_typel2(d)
  DEFINE d ui.Dialog
  DEFINE r, s INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_cols.typel2")==0 THEN RETURN TRUE END IF
  LET s = cols_verify_coltype(
               columns[r].typenum,
               columns[r].typel1,
               columns[r].typel2
          )
  IF s == -2 THEN
     CALL __mbox_ok(cols_msgtitle,"The type definition is wrong, check second length.","stop")
     RETURN FALSE
  END IF
  IF s == 2 THEN
     CALL __mbox_ok(cols_msgtitle,"The type definition is wrong, second length cannot be specified.","stop")
     LET columns[r].typel2 = NULL
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_typenn(d)
  DEFINE d ui.Dialog
  DEFINE r, s INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_cols.typenn")==0 THEN RETURN TRUE END IF
  IF columns[r].typenn == TRUE THEN
     RETURN TRUE
  END IF
  IF columns[r].pkeycol THEN
     CALL __mbox_ok(cols_msgtitle,"This column cannot be NULL, it is used for the primary key","stop")
     LET columns[r].typenn = TRUE
     RETURN FALSE
  END IF
  LET s = skeys_lookup_column_usage(curtable,columns[r].colname)
  IF s > 0 THEN
     CALL __mbox_ok(cols_msgtitle,
                    SFMT("This column cannot be NULL, it is used in secondary key '%1'",
                              tables[curtable].skeys[s].skeyname),
                    "stop")
     LET columns[r].typenn = TRUE
     RETURN FALSE
  END IF
{ Fkey cols can be NULL!
  LET s = fkeys_lookup_column_usage(curtable,columns[r].colname)
  IF s > 0 THEN
     CALL __mbox_ok(cols_msgtitle,
                    SFMT("This column cannot be NULL, it is used in foreign key '%1'",
                              tables[curtable].fkeys[s].fkeyname),
                    "stop")
     LET columns[r].typenn = TRUE
     RETURN FALSE
  END IF
}
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_defvalue(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_cols.defvalue")==0 THEN RETURN TRUE END IF
  IF NOT cols_verify_defvalue(
              columns[r].typenum,
              columns[r].typel1,
              columns[r].typel2,
              columns[r].typenn,
              columns[r].defvalue ) THEN
     CALL __mbox_ok(cols_msgtitle,"Invalid default value specified for this data type.","stop")
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION cols_check_pkeycol(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  IF (r:=check_temprow(d,"sa_cols"))==-1 THEN RETURN TRUE END IF
  IF columns[r].pkeycol THEN
     LET columns[r].typenn = TRUE
     IF pkeyname IS NULL THEN
        LET pkeyname = constraint_get_unique_name(constype_pkey,curtable)
     END IF
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION table_delete(d)
  DEFINE d ui.Dialog
  IF curtable == 0 THEN RETURN END IF
  IF __mbox_yn(tooltitle,
               "Are you sure you want to delete current table definition?",
               "question") THEN
     CALL tables.deleteElement(curtable)
     IF curtable > tables.getLength() THEN
        LET curtable = tables.getLength()
     END IF
     CALL tabdef_invars_read(d)
     CALL tabdef_touch_setup(d)
  END IF
END FUNCTION

FUNCTION table_create(d,ct)
  DEFINE d ui.Dialog
  DEFINE ct,i INTEGER
  IF NOT tabdef_check(d) THEN RETURN FALSE END IF
  CALL tables.appendElement()
  LET curtable = tables.getLength()
  LET old_tabname = NULL
  LET old_colname = NULL
  LET tables[curtable].tabname = table_get_newname(curtable)
  IF ct == -1 THEN
     LET tables[curtable].pkeyname = NULL
     CALL tables[curtable].cols.clear()
     CALL tables[curtable].skeys.clear()
     CALL tables[curtable].fkeys.clear()
     CALL tables[curtable].checks.clear()
     CALL tables[curtable].tabopts.clear()
     CALL tables[curtable].cols.appendElement()
     CALL cols_get_defaults(curtable,1) RETURNING tables[curtable].cols[1].*
  ELSE
     IF table_has_pkeycols(ct) THEN
        LET tables[curtable].pkeyname = constraint_get_unique_name(constype_pkey,curtable)
     END IF
     FOR i=1 TO tables[ct].cols.getLength()
         LET tables[curtable].cols[i].* = tables[ct].cols[i].*
     END FOR
     FOR i=1 TO tables[ct].skeys.getLength()
         LET tables[curtable].skeys[i].* = tables[ct].skeys[i].*
         LET tables[curtable].skeys[i].skeyname = constraint_get_unique_name(constype_skey,curtable)
     END FOR
     FOR i=1 TO tables[ct].fkeys.getLength()
         LET tables[curtable].fkeys[i].* = tables[ct].fkeys[i].*
         LET tables[curtable].fkeys[i].fkeyname = constraint_get_unique_name(constype_fkey,curtable)
     END FOR
     FOR i=1 TO tables[ct].checks.getLength()
         LET tables[curtable].checks[i].* = tables[ct].checks[i].*
         LET tables[curtable].checks[i].checkname = constraint_get_unique_name(constype_chck,curtable)
     END FOR
     FOR i=1 TO tables[ct].tabopts.getLength()
         LET tables[curtable].tabopts[i].* = tables[ct].tabopts[i].*
     END FOR
  END IF
  CALL d.setCurrentRow("sa_cols",1)
  CALL d.setCurrentRow("sa_skeys", (skeys.getLength()>0) )
  CALL d.setCurrentRow("sa_fkeys", (fkeys.getLength()>0) )
  CALL d.setCurrentRow("sa_checks", (checks.getLength()>0) )
  CALL d.setCurrentRow("sa_topts", (tabopts.getLength()>0) )
  CALL tabdef_invars_read(d)
  CALL tabdef_touch_setup(d)
  RETURN TRUE
END FUNCTION

FUNCTION tabdef_close(d)
  DEFINE d ui.Dialog
  DEFINE r CHAR(1)
  IF NOT tabdef_saved THEN
     LET r = __mbox_ync(tooltitle,
             "Do you want to save modifications before leaving?",
             "question")
     CASE r
       WHEN "y"
         CALL tabdef_invars_save(d)
         CALL schema_save(curschema)
         RETURN TRUE
       WHEN "n"
         RETURN TRUE
       WHEN "c"
         RETURN FALSE
     END CASE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION tabdef_edit(_saved)
  DEFINE _saved INTEGER
  DEFINE prow, s INTEGER
  DEFINE tn, tmp STRING

  LET tabdef_saved = _saved
  LET curtable = 1

  CALL tabdef_invars_read(NULL)

  DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)

      INPUT BY NAME curschema, tabname, pkeyname ATTRIBUTES(WITHOUT DEFAULTS, HELP=100)
       AFTER FIELD tabname
         IF NOT tabdef_check_tabname(DIALOG) THEN NEXT FIELD tabname END IF
       AFTER FIELD pkeyname
         IF NOT tabdef_check_pkeyname(DIALOG) THEN NEXT FIELD pkeyname END IF
       AFTER INPUT
         CALL tabdef_invars_save(DIALOG)
      END INPUT

      INPUT ARRAY columns FROM sa_cols.* ATTRIBUTES(WITHOUT DEFAULTS, AUTO APPEND=FALSE, HELP=101)
       BEFORE ROW
         LET cols_inserting = FALSE
         LET old_colname = columns[arr_curr()].colname
         CALL cols_setup(DIALOG)
       BEFORE INSERT
         LET cols_inserting = TRUE
         CALL tabdef_invars_save(DIALOG)
         CALL cols_get_defaults(curtable,arr_curr()) RETURNING columns[arr_curr()].*
         CALL cols_reorder_indexes(arr_curr())
         CALL cols_setup(DIALOG)
         CALL tabdef_touch_setup(DIALOG)
       BEFORE DELETE
         IF NOT cols_check_colname_not_used(columns[arr_curr()].colname) THEN CANCEL DELETE END IF
       AFTER DELETE
         CALL cols_reorder_indexes(arr_curr())
         CALL tabdef_touch_setup(DIALOG)
       AFTER FIELD colname
         IF NOT cols_check_colname(DIALOG) THEN NEXT FIELD colname END IF
       ON CHANGE typenum
         IF NOT cols_check_typenum(DIALOG) THEN NEXT FIELD typenum END IF
         CALL cols_setup(DIALOG)
       AFTER FIELD typel1
         IF NOT cols_check_typel1(DIALOG) THEN NEXT FIELD typel1 END IF
       AFTER FIELD typel2
         IF NOT cols_check_typel2(DIALOG) THEN NEXT FIELD typel2 END IF
       ON CHANGE typenn
         IF NOT cols_check_typenn(DIALOG) THEN NEXT FIELD typenn END IF
       ON CHANGE pkeycol
         IF NOT cols_check_pkeycol(DIALOG) THEN NEXT FIELD pkeycol END IF
       AFTER FIELD defvalue
         IF NOT cols_check_defvalue(DIALOG) THEN NEXT FIELD defvalue END IF
       AFTER INPUT
         CALL tabdef_invars_save(DIALOG)
       ON ACTION move_up
         CALL cols_exchange(DIALOG,"U")
       ON ACTION move_down
         CALL cols_exchange(DIALOG,"D")
      END INPUT

      INPUT ARRAY skeys FROM sa_skeys.* ATTRIBUTES(WITHOUT DEFAULTS, AUTO APPEND=FALSE, HELP=102)
       BEFORE INSERT
         LET skeys[arr_curr()].skeyname = constraint_get_unique_name(constype_skey,curtable)
         CALL tabdef_touch_setup(DIALOG)
       BEFORE ROW
         CALL dialog.setActionActive("skeys_edit_colset",(arr_curr()>0))
       AFTER DELETE
         CALL tabdef_touch_setup(DIALOG)
       AFTER FIELD skeyname
         IF NOT skeys_check_skeyname(DIALOG) THEN NEXT FIELD skeyname END IF
       AFTER FIELD skeycols
         IF NOT skeys_check_skeycols(DIALOG) THEN NEXT FIELD skeycols END IF
       AFTER ROW
         IF NOT skeys_verify_column_usage(DIALOG) THEN NEXT FIELD skeyname END IF
       AFTER INPUT
        CALL tabdef_invars_save(DIALOG)
      END INPUT

      INPUT ARRAY fkeys FROM sa_fkeys.* ATTRIBUTES(WITHOUT DEFAULTS, AUTO APPEND=FALSE, HELP=103)
       BEFORE ROW
         CALL dialog.setActionActive("fkeys_edit_colset",(arr_curr()>0))
         CALL dialog.setActionActive("zoom_refkey", fkeys[arr_curr()].reftabname IS NOT NULL )
       BEFORE INSERT
         LET fkeys[arr_curr()].fkeyname = constraint_get_unique_name(constype_fkey,curtable)
         LET fkeys[arr_curr()].delrule = udrule_restrict
         LET fkeys[arr_curr()].updrule = udrule_restrict
         CALL tabdef_touch_setup(DIALOG)
       AFTER DELETE
         CALL tabdef_touch_setup(DIALOG)
       AFTER FIELD fkeyname
         IF NOT fkeys_check_fkeyname(DIALOG) THEN NEXT FIELD fkeyname END IF
       AFTER FIELD fkeycols
         IF NOT fkeys_check_fkeycols(DIALOG) THEN NEXT FIELD fkeycols END IF
       ON CHANGE reftabname
         LET fkeys[arr_curr()].refconstname = fkey_get_first_keyconst(fkeys[arr_curr()].reftabname)
       AFTER FIELD reftabname
         IF NOT fkeys_check_reftabname(DIALOG) THEN NEXT FIELD reftabname END IF
       BEFORE FIELD refconstname
         CALL dialog.setActionActive("zoom_refkey", fkeys[arr_curr()].reftabname IS NOT NULL )
       AFTER FIELD refconstname
         IF NOT fkeys_check_refconstname(DIALOG) THEN NEXT FIELD refconstname END IF
       AFTER ROW
--display SFMT("after row: curr = %1  count = %2", DIALOG.getCurrentRow("sa_fkeys"), DIALOG.getArrayLength("sa_fkeys"))
         IF NOT fkeys_verify(DIALOG) THEN NEXT FIELD fkeycols END IF
       AFTER INPUT
         CALL tabdef_invars_save(DIALOG)
      END INPUT

      INPUT ARRAY checks FROM sa_checks.* ATTRIBUTES(WITHOUT DEFAULTS, AUTO APPEND=FALSE, HELP=104)
       BEFORE INSERT
         LET checks[arr_curr()].checkname = constraint_get_unique_name(constype_chck,curtable)
         CALL tabdef_touch_setup(DIALOG)
       AFTER DELETE
         CALL tabdef_touch_setup(DIALOG)
       AFTER FIELD checkname
         IF NOT checks_check_checkname(DIALOG) THEN NEXT FIELD checkname END IF
       AFTER INPUT
         CALL tabdef_invars_save(DIALOG)
      END INPUT

      DISPLAY ARRAY tabopts TO sa_topts.* ATTRIBUTES(HELP=105)
       BEFORE ROW
         CALL tabopts_sync_fields(DIALOG)
       ON ACTION insert
         LET prow = DIALOG.getCurrentRow("sa_topts")
         CALL DIALOG.insertRow("sa_topts", arr_curr())
         IF prow == 0 THEN LET prow = 1 END IF
         GOTO tabopts_init
       ON ACTION append
         CALL DIALOG.appendRow("sa_topts")
         LET prow = DIALOG.getArrayLength("sa_topts")
         GOTO tabopts_init
LABEL tabopts_init:
         CALL DIALOG.setCurrentRow("sa_topts",prow)
         LET tabopts[prow].dbtype = "_?_"
         LET tabopts[prow].sqltext = "-- DB specific CREATE TABLE clause"
         CALL tabopts_sync_fields(DIALOG)
         CALL tabdef_touch_setup(DIALOG)
       ON ACTION delete
         CALL DIALOG.deleteRow("sa_topts", arr_curr())
         IF DIALOG.getCurrentRow("sa_topts") > DIALOG.getArrayLength("sa_topts") THEN
            CALL DIALOG.setCurrentRow("sa_topts", DIALOG.getArrayLength("sa_topts"))
         END IF
         CALL tabopts_sync_fields(DIALOG)
         CALL tabdef_touch_setup(DIALOG)
       ON ACTION delall
         CALL DIALOG.deleteAllRows("sa_topts")
         CALL tabopts_sync_fields(DIALOG)
         CALL tabdef_touch_setup(DIALOG)
      END DISPLAY
      INPUT BY NAME tabopts_dbtype, tabopts_sqltext ATTRIBUTES(WITHOUT DEFAULTS)
       BEFORE INPUT
         CALL tabopts_sync_fields(DIALOG) -- disables fields if no rows in list!
       ON CHANGE tabopts_dbtype
         LET prow = DIALOG.getCurrentRow("sa_topts")
         LET tabopts[prow].dbtype = tabopts_dbtype
       AFTER FIELD tabopts_dbtype
         LET prow = DIALOG.getCurrentRow("sa_topts")
         IF prow>0 THEN -- Might be executed after move to other table
            LET tabopts[prow].dbtype = tabopts_dbtype
            LET prow = topts_check_dbtype_unicity(DIALOG)
            IF prow > 0 THEN NEXT FIELD tabopts_dbtype END IF
            CALL tabopts_sync_fields(DIALOG)
         END IF
       AFTER FIELD tabopts_sqltext
         GOTO sqltext_check
       ON ACTION to_check_sqltext
         GOTO sqltext_check
LABEL sqltext_check:
         LET prow = DIALOG.getCurrentRow("sa_topts")
         IF prow>0 THEN -- Might be executed after move to other table
            LET tabopts[prow].sqltext = tabopts_sqltext
            IF NOT topts_check_sqltext(DIALOG) THEN NEXT FIELD tabopts_sqltext END IF
            CALL tabdef_touch_setup(DIALOG)
         END IF
      END INPUT

      BEFORE DIALOG
        CALL tabdef_setup(DIALOG)
        CALL cols_setup(DIALOG)

      ON ACTION about
        CALL show_about()

      ON ACTION edit_options
        CALL options_edit()

      ON ACTION create_sql
        LET s = sql_create_script()

      ON ACTION edit_hprms
        IF hprms_edit(DIALOG) THEN
           CALL tabdef_touch_setup(DIALOG)
        END IF

      ON ACTION conv_names_upper
        IF __mbox_yn("Schema", "Convert all DB object names to uppercase?", "question") THEN
           IF NOT tables_check_unique_names(FALSE) THEN CONTINUE DIALOG END IF
           CALL tabdef_invars_save(DIALOG)
           CALL schema_convert_names("U")
           CALL tabdef_invars_read(DIALOG)
           CALL tabdef_touch_setup(DIALOG)
        END IF

      ON ACTION conv_names_lower
        IF __mbox_yn("Schema", "Convert all DB object names to lowercase?", "question") THEN
           IF NOT tables_check_unique_names(FALSE) THEN CONTINUE DIALOG END IF
           CALL tabdef_invars_save(DIALOG)
           CALL schema_convert_names("L")
           CALL tabdef_invars_read(DIALOG)
           CALL tabdef_touch_setup(DIALOG)
        END IF

      ON ACTION first_table CALL tabdef_move_to(DIALOG, "a", 1)                   NEXT FIELD tabname
      ON ACTION prev_table  CALL tabdef_move_to(DIALOG, "r", -1)                  NEXT FIELD tabname
      ON ACTION next_table  CALL tabdef_move_to(DIALOG, "r", +1)                  NEXT FIELD tabname
      ON ACTION last_table  CALL tabdef_move_to(DIALOG, "a", tables.getLength())  NEXT FIELD tabname

      ON ACTION save
         IF NOT tabdef_save(DIALOG, FALSE) THEN CONTINUE DIALOG END IF

      ON ACTION saveas
         IF NOT tabdef_save(DIALOG, TRUE) THEN CONTINUE DIALOG END IF

      ON ACTION close
         IF tabdef_close(DIALOG) THEN EXIT DIALOG END IF

      ON ACTION select_table
         LET tn = table_select()
         IF tn IS NOT NULL THEN
           LET curtable = table_lookup(tn)
           CALL tabdef_invars_read(DIALOG)
         END IF

      ON ACTION delete_table
         CALL table_delete(DIALOG)

      ON ACTION new_table
         IF NOT table_create(DIALOG,-1) THEN CONTINUE DIALOG END IF

      ON ACTION copy_table
         IF NOT table_create(DIALOG,curtable) THEN CONTINUE DIALOG END IF

      ON ACTION load
         CALL tabdef_load(DIALOG, tabdef_saved)

      ON ACTION extract_sql
         IF NOT tabdef_extract(DIALOG) THEN EXIT DIALOG END IF

      ON ACTION pkn_default
         LET pkeyname = constraint_get_unique_name(constype_pkey,curtable)
         CALL DIALOG.setFieldTouched("pkeyname", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION cn_default
         LET prow = DIALOG.getCurrentRow("sa_cols")
         LET columns[prow].colname = cols_get_newname(curtable, prow)
         CALL DIALOG.setFieldTouched("colname", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION dialogtouched
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION topts_append
         CALL DIALOG.appendRow("sa_topts")
         LET prow = DIALOG.getArrayLength("sa_topts")
         LET tabopts[prow].sqltext = "<enter the SQL clauses here>"
         CALL DIALOG.setCurrentRow("sa_topts", prow)
         CALL tabdef_touch_setup(DIALOG)
         NEXT FIELD dbtype

      ON ACTION topts_delete
         LET prow = DIALOG.getCurrentRow("sa_topts")
         CALL DIALOG.deleteRow("sa_topts", prow)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION skn_default
         LET prow = DIALOG.getCurrentRow("sa_skeys")
         LET skeys[prow].skeyname = constraint_get_unique_name(constype_skey,curtable)
         CALL DIALOG.setFieldTouched("skeyname", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION skeys_edit_colset
         LET prow = DIALOG.getCurrentRow("sa_skeys")
         LET skeys[prow].skeycols = colset_edit(curtable,skeys[prow].skeycols)
         CALL DIALOG.setFieldTouched("skeycols", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION zoom_reftab
         LET tn = table_select()
         IF tn IS NULL THEN CONTINUE DIALOG END IF
         LET prow = DIALOG.getCurrentRow("sa_fkeys")
         CALL dialog.setActionActive("zoom_refkey", TRUE)
         LET fkeys[prow].reftabname = tn
         LET fkeys[prow].refconstname = fkey_get_first_keyconst( tn )
         CALL DIALOG.setFieldTouched("reftabname", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION zoom_refkey
         LET prow = DIALOG.getCurrentRow("sa_fkeys")
         IF length(fkeys[prow].reftabname) == 0 THEN CONTINUE DIALOG END IF
         LET tmp = fkey_constraint_select(fkeys[prow].reftabname)
         IF tmp IS NOT NULL THEN
            LET fkeys[prow].refconstname = tmp
            CALL DIALOG.setFieldTouched("refconstname", TRUE)
            CALL tabdef_touch_setup(DIALOG)
         END IF

      ON ACTION fkeys_edit_colset
         LET prow = DIALOG.getCurrentRow("sa_fkeys")
         LET fkeys[prow].fkeycols = colset_edit(curtable,fkeys[prow].fkeycols)
         CALL DIALOG.setFieldTouched("fkeycols", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION fkn_default
         LET prow = DIALOG.getCurrentRow("sa_fkeys")
         LET fkeys[prow].fkeyname = constraint_get_unique_name(constype_fkey,curtable)
         CALL DIALOG.setFieldTouched("fkeyname", TRUE)
         CALL tabdef_touch_setup(DIALOG)

      ON ACTION ckn_default
         LET prow = DIALOG.getCurrentRow("sa_checks")
         LET checks[prow].checkname = constraint_get_unique_name(constype_chck,curtable)
         CALL DIALOG.setFieldTouched("checkname", TRUE)
         CALL tabdef_touch_setup(DIALOG)

  END DIALOG

END FUNCTION

FUNCTION cols_reorder_indexes(start)
  DEFINE start, i SMALLINT
  FOR i=start TO columns.getLength()
      LET columns[i].colindex = i
  END FOR
END FUNCTION

FUNCTION tabopts_sync_fields(d)
  DEFINE d ui.Dialog
  DEFINE r, e INTEGER
  LET r = d.getCurrentRow("sa_topts")
  LET e = (r > 0 AND d.getArrayLength("sa_topts") > 0 )
  IF e THEN
     LET tabopts_dbtype  = tabopts[r].dbtype
     LET tabopts_sqltext = tabopts[r].sqltext
  ELSE
     LET tabopts_dbtype  = NULL
     LET tabopts_sqltext = NULL
  END IF
  CALL d.setFieldActive("tabopts_dbtype", e)
  CALL d.setFieldActive("tabopts_sqltext", e)
END FUNCTION

FUNCTION tabdef_load(d, s)
  DEFINE d ui.Dialog
  DEFINE s INTEGER
  DEFINE r CHAR(1)
  IF s THEN
     LET r = "y"
  ELSE
     LET r = __mbox_ync(tooltitle,
             "Do you want to save modifications before loading a the schema?",
             "question")
     IF r == "y" THEN
        CALL schema_save(curschema)
     END IF
  END IF
  IF r != "c" THEN
     WHILE TRUE
       IF NOT ask_schema_to_load() THEN
          EXIT WHILE
       ELSE
          IF schema_load(curschema) == 0 THEN
             LET curtable = 1
             CALL tabdef_invars_read(d)
             EXIT WHILE
          END IF
       END IF
     END WHILE
  END IF
END FUNCTION

FUNCTION tabdef_extract(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  LET r = sql_extract_schema()
  CASE r
     WHEN 0 -- ok
       CALL tabdef_invars_read(d)
       CALL tabdef_touch_setup(d)
       RETURN TRUE
     WHEN 1 -- canceled by user
       RETURN TRUE
     OTHERWISE -- error, must load again
       IF NOT ask_schema_to_load() THEN
          RETURN FALSE
       ELSE
          IF schema_load(curschema) == 0 THEN
             LET curtable = 1
             CALL tabdef_invars_read(d)
             RETURN TRUE
          END IF
       END IF
  END CASE
  RETURN FALSE
END FUNCTION

FUNCTION topts_lookup_dbtype_usage(dbtype, r)
  DEFINE dbtype STRING
  DEFINE r, i INTEGER
  FOR i=1 TO tabopts.getLength()
      IF i!=r AND tabopts[i].dbtype == dbtype THEN
         RETURN i
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION topts_check_dbtype_unicity(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  LET d = NULL
  -- Check all lines!
  FOR r=1 TO tabopts.getLength()
      IF tabopts[r].dbtype == "_?_" THEN CONTINUE FOR END IF
      IF topts_lookup_dbtype_usage(tabopts[r].dbtype, r) > 0 THEN
         CALL __mbox_ok(topts_msgtitle, "A DB Type is used multiple times", "stop")
         RETURN r
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION topts_check_sqltext(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  LET r = d.getCurrentRow("sa_topts")
  IF r <= 0 THEN RETURN TRUE END IF
  IF tabopts_sqltext MATCHES "*\"*" THEN
     CALL __mbox_ok(topts_msgtitle, "SQL Text must not contain double-quotes", "stop")
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION msg_constraint_type_name(ctype)
  DEFINE ctype INTEGER
  CASE ctype
     WHEN constype_pkey RETURN "primary key"
     WHEN constype_skey RETURN "secondary key"
     WHEN constype_fkey RETURN "foreign key"
     WHEN constype_chck RETURN "check"
  END CASE
  RETURN "?"
END FUNCTION

FUNCTION msg_constraint_used(title, tabnum, ctype, i)
  DEFINE title STRING
  DEFINE tabnum, ctype, i INTEGER
  DEFINE msg STRING
  LET msg = SFMT("This constraint name is already used in table '%1' as %2 constraint #%3",
                 tables[tabnum].tabname, msg_constraint_type_name(ctype), i)
  CALL __mbox_ok(title, msg, "stop")
END FUNCTION

FUNCTION msg_constraint_name(tabnum, ctype, idx)
  DEFINE tabnum, ctype, idx INTEGER
  CASE ctype
     WHEN constype_pkey RETURN tables[tabnum].pkeyname
     WHEN constype_skey RETURN tables[tabnum].skeys[idx].skeyname
     WHEN constype_fkey RETURN tables[tabnum].fkeys[idx].fkeyname
     WHEN constype_chck RETURN tables[tabnum].checks[idx].checkname
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION msg_column_used(title, colname, tabnum, ctype, i)
  DEFINE title, colname STRING
  DEFINE tabnum, ctype, i INTEGER
  DEFINE msg STRING
  LET msg = SFMT("Column name '%1' is used in %2 constraint '%3' of table '%4'",
                 colname, msg_constraint_type_name(ctype),
                 msg_constraint_name(tabnum, ctype, i), tables[tabnum].tabname)
  CALL __mbox_ok(title, msg, "stop")
END FUNCTION

FUNCTION skeys_check_skeyname(d)
  DEFINE d ui.Dialog
  DEFINE r, t, c, p INTEGER
  IF (r:=check_temprow(d,"sa_skeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_skeys.skeyname")==0 THEN RETURN TRUE END IF
  IF NOT is_identifier_1(skeys[r].skeyname) THEN
     CALL __mbox_ok(skeys_msgtitle, "The name of the secondary key must be an identifier", "stop")
     RETURN FALSE
  END IF
  CALL tabdef_invars_save(d)
  CALL constraint_lookup_ex(skeys[r].skeyname,curtable,r,-1,-1) RETURNING t, c, p
  IF t>0 THEN
     CALL msg_constraint_used(skeys_msgtitle, t, c, p)
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION skeys_check_skeycols(d)
  DEFINE d ui.Dialog
  DEFINE r, s INTEGER
  IF (r:=check_temprow(d,"sa_skeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_skeys.skeycols")==0 THEN RETURN TRUE END IF
  CALL tabdef_invars_save(d)
  LET s = key_verify_colset(curtable,skeys[r].skeycols)
  CASE s
  WHEN -1
     CALL __mbox_ok(skeys_msgtitle, "Column list contains invalid column name", "stop")
     RETURN FALSE
  WHEN -2
     IF __mbox_yn(skeys_msgtitle,
          "Columns of a secondary key cannot accept NULL.\n"
          ||"Do you want to set a NOT NULL constraint on the columns?",
          "exclamation") THEN
        CALL key_set_notnull(curtable,skeys[r].skeycols)
        RETURN TRUE
     END IF
     RETURN FALSE
  WHEN -3
     CALL __mbox_ok(skeys_msgtitle, "No columns specified.", "stop")
     RETURN FALSE
  END CASE
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_check_fkeyname(d)
  DEFINE d ui.Dialog
  DEFINE r, t, c, p INTEGER
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.fkeyname")==0 THEN RETURN TRUE END IF
  IF NOT is_identifier_1(fkeys[r].fkeyname) THEN
     CALL __mbox_ok(fkeys_msgtitle, "The name of the foreign key must be an identifier", "stop")
     RETURN FALSE
  END IF
  CALL tabdef_invars_save(d)
  CALL constraint_lookup_ex(fkeys[r].fkeyname,curtable,-1,r,-1) RETURNING t, c, p
  IF t>0 THEN
     CALL msg_constraint_used(fkeys_msgtitle, t, c, p)
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_check_fkeycols(d)
  DEFINE d ui.Dialog
  DEFINE r, s INTEGER
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.fkeycols")==0 THEN RETURN TRUE END IF
  CALL tabdef_invars_save(d)
  LET s = key_verify_colset(curtable,fkeys[r].fkeycols)
  CASE s
  WHEN -1
     CALL __mbox_ok(fkeys_msgtitle, "Column list contains invalid column name", "stop")
     RETURN FALSE
{ Fkey cols can be NULL!
  WHEN -2
     IF __mbox_yn(fkeys_msgtitle,
          "Columns of a foreign key cannot accept NULL.\n"
          ||"Do you want to set a NOT NULL constraint on the columns?",
          "exclamation") THEN
        CALL key_set_notnull(curtable,fkeys[r].fkeycols)
        RETURN TRUE
     END IF
     RETURN FALSE
}
  WHEN -3
     CALL __mbox_ok(fkeys_msgtitle, "No columns specified.", "stop")
     RETURN FALSE
  END CASE
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_check_reftabname(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.reftabname")==0 THEN RETURN TRUE END IF
  IF table_lookup(fkeys[r].reftabname) == 0 THEN
     CALL __mbox_ok(fkeys_msgtitle, "Invalid table name", "stop")
     RETURN FALSE
  END IF
  IF NOT table_has_unique_key(fkeys[r].reftabname) THEN
     IF __mbox_yn(fkeys_msgtitle,
          "The reference table must have a primary key or a secondary key.\n"
          ||"Do you want to define the first column as primary key?",
          "exclamation") THEN
        CALL table_set_firstcolpkey(table_lookup(fkeys[r].reftabname))
     END IF
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_check_refconstname(d)
  DEFINE d ui.Dialog
  DEFINE r,t,c,p INTEGER
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.refconstname")==0 THEN RETURN TRUE END IF
  CALL constraint_lookup(fkeys[r].refconstname) RETURNING t, c, p
  IF t==0 OR (c!=constype_pkey AND c!=constype_skey) THEN
     CALL __mbox_ok(fkeys_msgtitle, "Invalid reference constraint", "stop")
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION checks_check_checkname(d)
  DEFINE d ui.Dialog
  DEFINE r, t, c, p INTEGER
  IF (r:=check_temprow(d,"sa_checks"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_checks.checkname")==0 THEN RETURN TRUE END IF
  IF NOT is_identifier_1(checks[r].checkname) THEN
     CALL __mbox_ok(checks_msgtitle, "The name of the check constraint must be an identifier", "stop")
     RETURN FALSE
  END IF
  CALL tabdef_invars_save(d)
  CALL constraint_lookup_ex(checks[r].checkname,curtable,-1,-1,r) RETURNING t, c, p
  IF t>0 THEN
     CALL msg_constraint_used(checks_msgtitle, t, c, p)
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION tabdef_check(d)
  DEFINE d ui.Dialog

  CASE
     WHEN d.getCurrentItem() MATCHES "sa_cols.*"
          IF d.validate("sa_cols.*") < 0 THEN RETURN FALSE END IF
     WHEN d.getCurrentItem() MATCHES "sa_skeys.*"
          IF d.validate("sa_skeys.*") < 0 THEN RETURN FALSE END IF
     WHEN d.getCurrentItem() MATCHES "sa_fkeys.*"
          IF d.validate("sa_fkeys.*") < 0 THEN RETURN FALSE END IF
     WHEN d.getCurrentItem() MATCHES "sa_checks.*"
          IF d.validate("sa_checks.*") < 0 THEN RETURN FALSE END IF
     WHEN d.getCurrentItem() MATCHES "sa_topts.*"
          IF d.validate("sa_topts.*") < 0 THEN RETURN FALSE END IF
  END CASE

  CALL tabdef_invars_save(d)

  IF NOT tabdef_check_tabname(d) THEN CALL d.nextField("tabname") RETURN FALSE END IF
  IF NOT tabdef_check_pkeyname(d) THEN CALL d.nextField("pkeyname") RETURN FALSE END IF

  -- Columns
  IF NOT cols_check_colname(d) THEN  CALL d.nextField("colname")  RETURN FALSE END IF
  IF NOT cols_check_defvalue(d) THEN CALL d.nextField("defvalue") RETURN FALSE END IF
  IF NOT cols_check_typel1(d) THEN   CALL d.nextField("typel1")   RETURN FALSE END IF
  IF NOT cols_check_typel2(d) THEN   CALL d.nextField("typel2")   RETURN FALSE END IF
  IF NOT cols_check_pkeycol(d) THEN  CALL d.nextField("pkeycol")  RETURN FALSE END IF

  -- Secondary keys
  IF NOT skeys_check_skeyname(d) THEN CALL d.nextField("skeyname") RETURN FALSE END IF
  IF NOT skeys_check_skeycols(d) THEN CALL d.nextField("skeycols") RETURN FALSE END IF
  IF NOT skeys_verify_column_usage(d) THEN CALL d.nextField("skeyname") RETURN FALSE END IF 

  -- Foreign keys
  IF NOT fkeys_check_fkeyname(d) THEN CALL d.nextField("fkeyname") RETURN FALSE END IF
  IF NOT fkeys_check_fkeycols(d) THEN CALL d.nextField("fkeycols") RETURN FALSE END IF
  IF NOT fkeys_check_reftabname(d) THEN CALL d.nextField("reftabname") RETURN FALSE END IF
  IF NOT fkeys_check_refconstname(d) THEN CALL d.nextField("refconstname") RETURN FALSE END IF
  IF NOT fkeys_verify(d) THEN CALL d.nextField("fkeycols") RETURN FALSE END IF
 
  -- Checks
  IF NOT checks_check_checkname(d) THEN CALL d.nextField("checkname") RETURN FALSE END IF

  -- Table options
  IF topts_check_dbtype_unicity(d) > 0 THEN CALL d.nextField("tabopts_dbtype") RETURN FALSE END IF
  IF NOT topts_check_sqltext(d) THEN CALL d.nextField("tabopts_sqltext") RETURN FALSE END IF

  RETURN TRUE

END FUNCTION

FUNCTION tabdef_save(d, as)
  DEFINE d ui.Dialog
  DEFINE as SMALLINT
  IF NOT tabdef_check(d) THEN RETURN FALSE END IF
  IF as THEN
     IF NOT ask_schema_to_saveas() THEN RETURN FALSE END IF
  END IF
  CALL schema_save(curschema)
  CALL tabdef_clean_setup(d)
  RETURN TRUE
END FUNCTION

FUNCTION hprms_edit(d)
  DEFINE d ui.Dialog
  DEFINE r INTEGER
  DEFINE p t_hprms

  CALL tabdef_invars_save(d) -- To check unique names

  OPEN WINDOW w_hprms WITH FORM "fgldsm_info" ATTRIBUTES(STYLE="dialog")

  LET p.* = hprms.*
  LET int_flag = FALSE
  INPUT BY NAME p.* WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)
     ON CHANGE schquotedids
        IF p.schquotedids == "n" THEN
           IF NOT tables_check_unique_names(FALSE) THEN
              LET p.schquotedids = "y"
              NEXT FIELD CURRENT
           END IF
        END IF
  END INPUT

  IF int_flag THEN
     LET r = FALSE
  ELSE
     LET r = TRUE
     LET hprms.* = p.*
  END IF

  LET int_flag = FALSE

  CLOSE WINDOW w_hprms
  RETURN r

END FUNCTION

FUNCTION cmbinit_dbtype(cb)
  DEFINE cb ui.ComboBox
  CALL cb.clear()
  CALL cb.addItem( "_?_", "Not specified" )
  CALL cb.addItem( "IFX", "Informix" )
  CALL cb.addItem( "ORA", "Oracle DB" )
  CALL cb.addItem( "DB2", "IBM DB2 LUW" )
  CALL cb.addItem( "MSV", "SQL Server" )
  CALL cb.addItem( "PGS", "PostgreSQL" )
  CALL cb.addItem( "MYS", "Oracle MySQL" )
  CALL cb.addItem( "HDB", "SAP HANA" )
END FUNCTION

FUNCTION cmbinit_dbdrivers(cb)
  DEFINE cb ui.ComboBox
  DEFINE doc om.DomDocument
  DEFINE l, d, i, v om.DomNode
  DEFINE dname, dserv, dapi, dvers, dtext STRING
  LET doc = om.DomDocument.createFromXmlFile(
               base.Application.getFglDir() || os.Path.separator() ||
               "etc" || os.Path.separator() || "dbdinfo.xml" )
  IF doc IS NULL THEN
     DISPLAY "Error: Could not find dbdinfo.xml file in FGLDIR/etc"
     EXIT PROGRAM 1
  END IF
  LET l = doc.getDocumentElement() -- FGLDatabaseDriverList
  LET d = l.getFirstChild() -- FGLDatabaseDriver
  CALL cb.clear()
  WHILE d IS NOT NULL
      LET dname = d.getAttribute("name")
      IF dname IS NULL THEN
         DISPLAY "Error: Undefined driver name in dbdinfo.xml"
         EXIT PROGRAM 1
      END IF
      LET dserv = NULL
      LET dapi  = NULL
      LET dvers = NULL
      LET i = d.getFirstChild()
      WHILE i IS NOT NULL
          LET v = i.getFirstChild()
          CASE i.getTagName()
               WHEN "ServerName" LET dserv = v.getAttribute("@chars")
               WHEN "ClientApi"  LET dapi  = v.getAttribute("@chars")
               WHEN "Version"    LET dvers = v.getAttribute("@chars")
          END CASE
          LET i = i.getNext()
      END WHILE
      LET dtext = dserv," ",dapi," ",dvers," (",dname,")"
      CALL cb.addItem( dname, dtext )
      LET d = d.getNext()
  END WHILE
END FUNCTION

FUNCTION sql_extract_schema()
  DEFINE r INTEGER
  OPEN WINDOW w_extract WITH FORM "fgldsm_extract" ATTRIBUTES(STYLE="dialog2")
  WHILE TRUE 
  INPUT BY NAME ext_params.* WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED, HELP = 106)
      ON CHANGE username
         IF NOT FIELD_TOUCHED(dbowner) THEN
            LET ext_params.dbowner = ext_params.username
         END IF
      BEFORE INPUT
        CALL DIALOG.setActionHidden("help",0)
  END INPUT
  IF int_flag THEN
     CLOSE WINDOW w_extract
     LET int_flag=FALSE
     RETURN 1
  END IF
  LET r = schema_extraction()
  CASE r
       WHEN 0
            LET curschema = get_schname_from_dbname(ext_params.dbname)
            LET hprms.schname = curschema
            LET hprms.schcreat = CURRENT YEAR TO SECOND
            LET hprms.schmodif = hprms.schcreat
            LET hprms.schrevision = 0
            LET hprms.schquotedids = "y" -- Always...
            LET hprms.schcomment = "Database source: ", ext_params.dbname
            CALL __mbox_ok( tooltitle, SFMT("Database schema extracted with success.\n" ||"Found %1 tables in the database.",
                            tables.getLength()), "information")
            EXIT WHILE
       WHEN -1
            CALL __mbox_ok( tooltitle, "Could not connect to database server", "exclamation")
            CONTINUE WHILE
       WHEN -2
            CALL __mbox_ok( tooltitle, "Could not identify database type", "exclamation")
            CONTINUE WHILE
       WHEN -3
            CALL __mbox_ok( tooltitle, "Database schema extraction interrupted by user", "exclamation")
            CONTINUE WHILE
       WHEN -4
            CALL __mbox_ok( tooltitle, "Schema extraction not yet implemented for this type of database", "exclamation")
            CONTINUE WHILE
       OTHERWISE
            CALL __mbox_ok( tooltitle, "Could not extract database schema", "exclamation")
            CONTINUE WHILE
  END CASE
  END WHILE
  CLOSE WINDOW w_extract
  RETURN r
END FUNCTION

FUNCTION sql_create_script()
  DEFINE s INTEGER
  LET s = sql_script_ask_parameters()
  IF s!=0 THEN RETURN FALSE END IF
  LET s = sql_generate_script(curschema)
  IF s!=0 THEN
     CALL __mbox_ok(tooltitle,"Could not generate SQL script","stop")
     RETURN FALSE
  END IF
  CALL __mbox_ok(tooltitle,"SQL script was generated","information")
  RETURN TRUE
END FUNCTION

FUNCTION sql_script_ask_parameters()
  DEFINE params RECORD
           dbtype t_dbtype,
           filename STRING,
           namedconst INTEGER,
           createfkeys INTEGER,
           droptables INTEGER,
           quotedids INTEGER,
           upcaseids INTEGER,
           tableopts INTEGER
         END RECORD
  DEFINE tmpstr STRING
  OPEN WINDOW w_dbtype WITH FORM "fgldsm_generate" ATTRIBUTES(STYLE="dialog")
  LET int_flag = FALSE
  LET params.* = sql_params.*
  IF hprms.schquotedids == "y" THEN
     LET params.upcaseids = FALSE
  END IF
  IF params.filename IS NULL THEN
      LET tmpstr = os.Path.baseName(get_schname_from_fname(curschema))
      LET params.filename = "." || os.Path.separator() || tmpstr || ".sql"
  END IF
  INPUT BY NAME params.*
      WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)
      ON ACTION filesave
         LET tmpstr = fglt_file_savedlg(
                           NULL,
                           os.Path.dirName(params.filename),
                           os.Path.baseName(params.filename),
                           "*.sql","cd,dd,df,sh,oe")
         IF tmpstr IS NOT NULL THEN
            LET params.filename = tmpstr
         END IF
      AFTER INPUT
        IF os.Path.exists(params.filename) THEN
           IF NOT __mbox_yn(tooltitle,
               SFMT("The file '%1' exists already, do you want to overwrite?",params.filename),
               "question") THEN
              NEXT FIELD filename
           END IF
        END IF
  END INPUT
  CLOSE WINDOW w_dbtype
  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -1
  ELSE
     LET sql_params.* = params.*
     RETURN 0
  END IF
END FUNCTION

FUNCTION options_edit()
  DEFINE params t_glb_params
  LET params.* = glb_params.*
  OPEN WINDOW w_options WITH FORM "fgldsm_options" ATTRIBUTES(STYLE="dialog")
  INPUT BY NAME params.*
        WITHOUT DEFAULTS
        ATTRIBUTES(UNBUFFERED, HELP = 107)
     BEFORE INPUT
        CALL DIALOG.setActionHidden("help",0)
     AFTER INPUT
        IF NOT int_flag THEN
           LET glb_params.* = params.*
        END IF
  END INPUT
  CLOSE WINDOW w_options
END FUNCTION

FUNCTION cols_set_pkeyflags(list)
  DEFINE list STRING
  DEFINE tok base.StringTokenizer
  DEFINE i INTEGER
  LET tok = base.StringTokenizer.create(list,",")
  WHILE tok.hasMoreTokens()
    LET i = column_lookup(curtable,tok.nextToken())
    LET columns[i].pkeycol = TRUE
    LET columns[i].typenn = 1
  END WHILE
END FUNCTION

FUNCTION colset_edit(tabnum,collist)
  DEFINE tabnum INTEGER
  DEFINE collist STRING
  DEFINE cols DYNAMIC ARRAY OF RECORD
             selected SMALLINT,
             colname STRING,
             coltype STRING
         END RECORD
  DEFINE i INTEGER
  DEFINE prow,srow INTEGER
  DEFINE tok base.StringTokenizer
  DEFINE ct,tmpl STRING

  FOR i=1 TO tables[tabnum].cols.getLength()
      CALL cols.appendElement()
      LET cols[i].selected = FALSE
      LET cols[i].colname = tables[tabnum].cols[i].colname
      LET ct = cols_serialize_type(curtable,i)
      IF tables[tabnum].cols[i].typenn THEN
         LET ct = ct, " NOT NULL"
      END IF
      LET cols[i].coltype = ct
  END FOR
  LET tok = base.StringTokenizer.create(collist,",")
  WHILE tok.hasMoreTokens()
      LET i = column_lookup(tabnum,tok.nextToken())
      IF i>0 THEN
         LET cols[i].selected = TRUE
      END IF
  END WHILE

  OPEN WINDOW w_colset WITH FORM "fgldsm_colset" ATTRIBUTES(STYLE="dialog4")

  LET int_flag = FALSE
  INPUT ARRAY cols WITHOUT DEFAULTS FROM sa.*
     ATTRIBUTES(UNBUFFERED,ACCEPT=FALSE,CANCEL=FALSE,
                APPEND ROW=FALSE,INSERT ROW=FALSE,DELETE ROW=FALSE)
     BEFORE ROW
        LET prow = arr_curr()
        LET srow = scr_line()
     ON ACTION close_dialog
        ACCEPT INPUT
     ON ACTION clear_all
        FOR i=1 TO cols.getLength()
            LET cols[i].selected = FALSE
        END FOR
  END INPUT

  FOR i=1 TO cols.getLength()
      IF cols[i].selected THEN
         IF tmpl IS NULL THEN
            LET tmpl = cols[i].colname
         ELSE
            LET tmpl = tmpl, ",", cols[i].colname
         END IF
      END IF
  END FOR

  CLOSE WINDOW w_colset

  RETURN tmpl

END FUNCTION

FUNCTION skeys_verify_column_usage(d)
  DEFINE d ui.Dialog
  DEFINE pkeycols STRING
  DEFINE i,j INTEGER
  IF check_temprow(d,"sa_skeys")==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_skeys.*")==0 THEN RETURN TRUE END IF
  CALL tabdef_invars_save(d)
  LET pkeycols = cols_serialize_pkeycols(curtable)
  FOR i=1 TO skeys.getLength()
      IF pkeycols == skeys[i].skeycols THEN
             CALL __mbox_ok(fkeys_msgtitle, "Identical column list already used by primary of this table", "stop")
         RETURN FALSE
      END IF
  END FOR
  FOR i=1 TO skeys.getLength()
      FOR j=1 TO skeys.getLength()
          IF j!=i AND skeys[i].skeycols == skeys[j].skeycols THEN
             CALL __mbox_ok(fkeys_msgtitle, "Identical column list already used by secondary key of this table", "stop")
             RETURN FALSE
          END IF
      END FOR
  END FOR
  FOR i=1 TO skeys.getLength()
      FOR j=1 TO fkeys.getLength()
          IF fkeys[i].fkeycols == skeys[j].skeycols THEN
             CALL __mbox_ok(fkeys_msgtitle, "Identical column list already used by foreign key of this table", "stop")
             RETURN FALSE
          END IF
      END FOR
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION key_set_notnull(tabnum,colset)
  DEFINE tabnum INTEGER
  DEFINE colset STRING
  DEFINE colname STRING
  DEFINE i INTEGER
  DEFINE tok base.StringTokenizer
  LET tok=base.StringTokenizer.create(colset,",")
  WHILE tok.hasMoreTokens()
      LET colname = tok.nextToken()
      LET i = column_lookup(tabnum,colname)
      LET columns[i].typenn = TRUE
  END WHILE
END FUNCTION

FUNCTION fkeys_verify(d)
  DEFINE d ui.Dialog
  IF NOT fkeys_verify_column_usage(d)
  OR NOT fkeys_verify_typematch(d)
  OR NOT fkeys_verify_dupplicates(d)
  OR NOT fkeys_verify_selfref(d) THEN
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_verify_column_usage(d)
  DEFINE d ui.Dialog
  DEFINE pkeycols STRING
  DEFINE r,i,j INTEGER
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.*")==0 THEN RETURN TRUE END IF
  CALL tabdef_invars_save(d)
  LET pkeycols = cols_serialize_pkeycols(curtable)
  FOR i=1 TO fkeys.getLength()
      IF pkeycols == fkeys[i].fkeycols THEN
         CALL __mbox_ok(fkeys_msgtitle, "Identical column list already used by primary key of this table", "stop")
         RETURN FALSE
      END IF
  END FOR
  FOR i=1 TO fkeys.getLength()
      FOR j=1 TO skeys.getLength()
          IF fkeys[i].fkeycols == skeys[j].skeycols THEN
             CALL __mbox_ok(fkeys_msgtitle, "Identical column list already used by secondary key of this table", "stop")
             RETURN FALSE
          END IF
      END FOR
  END FOR
  FOR i=1 TO fkeys.getLength()
      FOR j=1 TO fkeys.getLength()
          IF j!=i AND fkeys[i].fkeycols == fkeys[j].fkeycols THEN
             CALL __mbox_ok(fkeys_msgtitle, "Identical column list already used by foreign key of this table", "stop")
             RETURN FALSE
          END IF
      END FOR
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_verify_typematch(d)
  DEFINE d ui.Dialog
  DEFINE r, i, rt, rc, rp INTEGER
  DEFINE tok base.StringTokenizer
  DEFINE cols DYNAMIC ARRAY OF STRING
  DEFINE refcols DYNAMIC ARRAY OF STRING
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.*")==0 THEN RETURN TRUE END IF
  CALL tabdef_invars_save(d)
  CALL constraint_lookup(fkeys[r].refconstname) RETURNING rt, rc, rp
  IF rt==0 OR (rc!=constype_pkey AND rc!=constype_skey) THEN
     CALL __mbox_ok(fkeys_msgtitle, "Reference constraint is not valid", "stop")
     RETURN FALSE
  END IF
  LET tok=base.StringTokenizer.create(fkeys[r].fkeycols,",")
  WHILE tok.hasMoreTokens()
      CALL cols.appendElement()
      LET cols[cols.getLength()] = tok.nextToken()
  END WHILE
  IF rc==constype_pkey THEN
     FOR i=1 TO tables[rt].cols.getLength()
         IF tables[rt].cols[i].pkeycol THEN
            CALL refcols.appendElement()
            LET refcols[refcols.getLength()] = tables[rt].cols[i].colname
         END IF
     END FOR
  ELSE -- constype_skey
     LET tok=base.StringTokenizer.create(tables[rt].skeys[rp].skeycols,",")
     WHILE tok.hasMoreTokens()
         CALL refcols.appendElement()
         LET refcols[refcols.getLength()] = tok.nextToken()
     END WHILE
  END IF
  IF cols.getLength()!=refcols.getLength() THEN
     CALL __mbox_ok(fkeys_msgtitle, "Number of columns does not match reference constraint column count", "stop")
     RETURN FALSE
  END IF
  FOR i=1 TO cols.getLength()
      IF NOT cols_verify_typematch(tabname,cols[i],fkeys[r].reftabname,refcols[i]) THEN
         CALL __mbox_ok(fkeys_msgtitle, SFMT("Type of column '%1' does not match reference column type",cols[i]), "stop")
         RETURN FALSE
      END IF
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_verify_dupplicates(d)
  DEFINE d ui.Dialog
  DEFINE r,i,j INTEGER
  IF (r:=check_temprow(d,"sa_fkeys"))==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.*")==0 THEN RETURN TRUE END IF
  FOR i=1 TO fkeys.getLength()
      FOR j=1 TO fkeys.getLength()
          IF j!=i AND fkeys[j].fkeycols == fkeys[i].fkeycols THEN
             CALL __mbox_ok(fkeys_msgtitle, "Identical foreign key already defined for this table!", "stop")
             RETURN FALSE
          END IF
      END FOR
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION fkeys_verify_selfref(d)
  DEFINE d ui.Dialog
  DEFINE i,rt,rc,rp INTEGER
  DEFINE refcols STRING
  IF check_temprow(d,"sa_fkeys")==-1 THEN RETURN TRUE END IF
  IF d.getFieldTouched("sa_fkeys.*")==0 THEN RETURN TRUE END IF
  CALL tabdef_invars_save(d)
  FOR i=1 TO fkeys.getLength()
      IF fkeys[i].reftabname = tabname THEN
         CALL constraint_lookup(fkeys[i].refconstname) RETURNING rt, rc, rp
         IF rc==constype_pkey THEN
            LET refcols = cols_serialize_pkeycols(rt)
         ELSE -- constype_skey
            LET refcols = tables[rt].skeys[rp].skeycols
         END IF
         IF refcols == fkeys[i].fkeycols THEN
            CALL __mbox_ok(fkeys_msgtitle, "Foreign key defines a recursive column reference in same table!", "stop")
            RETURN FALSE
         END IF
      END IF
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION table_select_fill(ftabname, fhaspkey, tabsel)
  DEFINE ftabname STRING
  DEFINE fhaspkey SMALLINT
  DEFINE tabsel DYNAMIC ARRAY OF RECORD
                    tabname STRING,
                    pkeyname STRING
                END RECORD
  DEFINE i,j INTEGER

  CALL tabsel.clear()
  FOR i = 1 TO tables.getLength()
      IF tables[i].tabname NOT MATCHES ftabname THEN CONTINUE FOR END IF
      IF fhaspkey IS NOT NULL THEN
         IF     fhaspkey AND tables[i].pkeyname IS     NULL THEN CONTINUE FOR END IF
         IF NOT fhaspkey AND tables[i].pkeyname IS NOT NULL THEN CONTINUE FOR END IF
      END IF
      CALL tabsel.appendElement()
      LET j = tabsel.getLength()
      LET tabsel[j].tabname = tables[i].tabname
      IF tables[i].pkeyname IS NOT NULL THEN
         LET tabsel[j].pkeyname = tables[i].pkeyname, " (", cols_serialize_pkeycols(i), ")"
      ELSE
         LET tabsel[j].pkeyname = NULL
      END IF
  END FOR

END FUNCTION

FUNCTION table_select()
  DEFINE ftabname STRING
  DEFINE fhaspkey SMALLINT
  DEFINE tabsel DYNAMIC ARRAY OF RECORD
                    tabname STRING,
                    pkeyname STRING
                END RECORD
  DEFINE tn STRING
  OPEN WINDOW w_tablist WITH FORM "fgldsm_tables" ATTRIBUTES(STYLE="dialog4")
  LET ftabname = "*"
  LET fhaspkey = NULL
  LET tn = NULL
  DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
     INPUT BY NAME ftabname, fhaspkey ATTRIBUTES(WITHOUT DEFAULTS)
     END INPUT
     DISPLAY ARRAY tabsel TO sa.*
     END DISPLAY
     BEFORE DIALOG
        CALL table_select_fill(ftabname, fhaspkey, tabsel)
     ON ACTION filter
        CALL table_select_fill(ftabname, fhaspkey, tabsel)
     ON ACTION accept
        LET tn = tabsel[arr_curr()].tabname
        EXIT DIALOG
     ON ACTION cancel
        LET tn = NULL
        EXIT DIALOG
  END DIALOG
  CLOSE WINDOW w_tablist
  RETURN tn
END FUNCTION

FUNCTION fkey_constraint_select(tabname)
  DEFINE tabname STRING
  DEFINE constraints DYNAMIC ARRAY OF RECORD
                    constname STRING,
                    constcols STRING
                END RECORD
  DEFINE i,s,tn INTEGER
  DEFINE cn STRING

  OPEN WINDOW w_keylist WITH FORM "fgldsm_keylist" ATTRIBUTES(STYLE="dialog4")

  LET tn = table_lookup(tabname)
  IF tn==0 THEN RETURN NULL END IF
  CALL constraints.clear()
  IF tables[tn].pkeyname IS NOT NULL THEN
     CALL constraints.appendElement()
     LET i = constraints.getLength()
     LET constraints[i].constname = tables[tn].pkeyname
     LET constraints[i].constcols = cols_serialize_pkeycols(tn)
  END IF
  FOR s=1 TO tables[tn].skeys.getLength()
     CALL constraints.appendElement()
     LET i = constraints.getLength()
     LET constraints[i].constname = tables[tn].skeys[s].skeyname
     LET constraints[i].constcols = tables[tn].skeys[s].skeycols
  END FOR

  LET cn = NULL
  LET int_flag=FALSE
  DISPLAY ARRAY constraints TO sa.* ATTRIBUTES(UNBUFFERED)
      AFTER DISPLAY
         IF NOT int_flag THEN
            LET cn = constraints[arr_curr()].constname
         END IF
  END DISPLAY

  CLOSE WINDOW w_keylist
  LET int_flag=FALSE
  RETURN cn

END FUNCTION

FUNCTION cmbinit_cols_typenum(cb)
  DEFINE cb ui.ComboBox
  DEFINE i INTEGER
  CALL cb.clear()
  FOR i = 1 TO dtdef.getLength()
      CALL cb.addItem( dtdef[i].num, dtdef[i].name )
  END FOR
END FUNCTION

FUNCTION cmbinit_fkeys_delupd(cb)
  DEFINE cb ui.ComboBox
  CALL cb.clear()
  CALL cb.addItem("?","Default")
  CALL cb.addItem("A","No Action")
  CALL cb.addItem("R","Restrict")
  CALL cb.addItem("C","Cascade")
  CALL cb.addItem("D","Set Default")
  CALL cb.addItem("N","Set Null")
END FUNCTION

FUNCTION show_about()
  CALL __mbox_ok( "About", tooltitle || " " || toolversion, "about" )
END FUNCTION

FUNCTION __mbox_ok(title,message,icon)
  DEFINE title, message, icon STRING
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "OK"
  END MENU
END FUNCTION

FUNCTION __mbox_yn(title,message,icon)
  DEFINE title, message, icon STRING
  DEFINE r SMALLINT
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "Yes" LET r=TRUE
     COMMAND "No"  LET r=FALSE
  END MENU
  RETURN r
END FUNCTION

FUNCTION __mbox_ync(title,message,icon)
  DEFINE title, message, icon STRING
  DEFINE r CHAR
  MENU title ATTRIBUTES(STYLE='dialog',IMAGE=icon,COMMENT=message)
     COMMAND "Yes"     LET r="y"
     COMMAND "No"      LET r="n"
     COMMAND "Cancel"  LET r="c"
  END MENU             
  RETURN r
END FUNCTION

FUNCTION display_usage()
  DISPLAY "Usage : " || toolname || " [options]"
  DISPLAY "  -V : Display version information."
  DISPLAY "  -h : Display this help."
  DISPLAY "  -ds schema : Database .dbs metaschema file name."
  DISPLAY "  -gs : Generate SQL script without interactive window."
  DISPLAY "  -of name : SQL script output file name."
  DISPLAY "  -dt type : Database type (IFX,ORA,DB2,MSV,MYS,PGS,HDB)."
  DISPLAY "  -gnc : Generate named constraints."
  DISPLAY "  -gfk : Generate foreign key creation commands."
  DISPLAY "  -gdt : Generate table deletion commands."
  DISPLAY "  -gqi : Generate quoted identifiers."
  DISPLAY "  -gui : Generate identifiers in uppercase."
  DISPLAY "  -gto : Generate with DB specific table optionts."
END FUNCTION

FUNCTION addCommonStyles()
    DEFINE r, sl, s, a om.DomNode
    DEFINE nl om.NodeList
    LET r = ui.Interface.getRootNode()
    LET nl = r.selectByTagName("StyleList")
    LET sl = nl.item(1)
    LET s = sl.createChild("Style")
    CALL s.setAttribute("name", ":query")
    CALL sl.appendChild(s)
    LET a = s.createChild("StyleAttribute")
    CALL s.appendChild(a)
    CALL a.setAttribute("name", "backgroundColor")
    CALL a.setAttribute("value", "yellow")
END FUNCTION

FUNCTION addStyleList1()
    DEFINE r, sl, s, a om.DomNode
    DEFINE nl om.NodeList
    LET r = ui.Interface.getRootNode()
    LET nl = r.selectByTagName("StyleList")
    LET sl = nl.item(1)

    LET s = sl.createChild("Style")
    CALL s.setAttribute("name", "Table.list1:odd")
    CALL sl.appendChild(s)
    LET a = s.createChild("StyleAttribute")
    CALL s.appendChild(a)
    CALL a.setAttribute("name", "backgroundColor")
    CALL a.setAttribute("value", "#FFFFDD")

    LET s = sl.createChild("Style")
    CALL s.setAttribute("name", "Table.list1:even")
    CALL sl.appendChild(s)
    LET a = s.createChild("StyleAttribute")
    CALL s.appendChild(a)
    CALL a.setAttribute("name", "backgroundColor")
    CALL a.setAttribute("value", "#EEFFFF")

    LET s = sl.createChild("Style")
    CALL s.setAttribute("name", "Table.list1")
    CALL sl.appendChild(s)
    LET a = s.createChild("StyleAttribute")
    CALL s.appendChild(a)
    CALL a.setAttribute("name", "highlightCurrentRow")
    CALL a.setAttribute("value", "1")
    LET a = s.createChild("StyleAttribute")
    CALL s.appendChild(a)
    CALL a.setAttribute("name", "highlightColor")
    CALL a.setAttribute("value", "lightGray")

END FUNCTION

