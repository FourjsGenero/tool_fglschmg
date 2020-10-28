IMPORT os
IMPORT FGL fgldbslib
IMPORT FGL fgltbasics
IMPORT FGL fgltdialogs
#IMPORT FGL fgltfiledlg

-- FIXME: Generate serial emulation objects according to the emulation type.

GLOBALS "fgldsm_globals.4gl"

DEFINE coldef RECORD
             tabname VARCHAR(200),
             colname VARCHAR(200),
             nattype STRING,
             ifxtype INTEGER,
             ifxleng INTEGER
         END RECORD

FUNCTION get_schname_from_dbname(dbname)
  DEFINE dbname STRING
  DEFINE i INTEGER
  DEFINE schname STRING
  LET schname = dbname
  LET i = schname.getIndexOf("@",1)
  IF i>0 THEN LET schname = schname.subString(1,i-1) END IF
  LET i = schname.getIndexOf("+",1)
  IF i>0 THEN LET schname = schname.subString(1,i-1) END IF
  RETURN schname || fext_schema
END FUNCTION

FUNCTION get_schname_from_fname(fname)
  DEFINE fname STRING
  DEFINE i INTEGER
  LET i = fname.getIndexOf(fext_schema,1)
  IF i>1 THEN
     RETURN fname.subString(1,i-1)
  ELSE
     RETURN NULL
  END IF
END FUNCTION

FUNCTION schema_extraction()
  DEFINE r,x INTEGER

  CALL fglt_log_open()
  CALL fglt_log_write("Database schema extraction")
  CALL fglt_log_write("")
  CALL fglt_log_write(SFMT(" Extraction date : %1", CURRENT YEAR TO SECOND))
  CALL fglt_log_write(SFMT(" Source          : %1", ext_params.dbname))
  CALL fglt_log_write(SFMT(" Driver          : %1", ext_params.dbdriver))
  CALL fglt_log_write(SFMT(" Login           : %1", ext_params.username))
  CALL fglt_log_write(SFMT(" Owner/Schema    : %1", ext_params.dbowner))
  CALL fglt_log_write(SFMT(" Conversions     : %1", ext_params.cvmeth))
  CALL fglt_log_write(SFMT(" Conversions     : %1", ext_params.cvmeth))
  CALL fglt_log_write(SFMT(" Table name      : %1", ext_params.tabname))
  CALL fglt_log_write(SFMT(" System tables   : %1", ext_params.systables))
  CALL fglt_log_write(SFMT(" Ignore errors   : %1", ext_params.ignerrors))
  CALL fglt_log_write("")

  -- Owner is mandatory, otherwise we can't distinguish unique tables to 
  -- extract constraints and other table properties....
  IF LENGTH(ext_params.dbowner) == 0 THEN
     CALL fglt_log_write("")
     CALL fglt_log_write("ERROR: Must provide db owner to extract table information...")
     LET r=-1
     GOTO ext_end
  END IF

  CALL fgldbsch_init()

  CALL fgldbsch_set_parameter("dbname", ext_params.dbname)
  CALL fgldbsch_set_parameter("dbdriver", ext_params.dbdriver)
  CALL fgldbsch_set_parameter("username", ext_params.username)
  CALL fgldbsch_set_parameter("userpswd", ext_params.password)
  CALL fgldbsch_set_parameter("dbowner", ext_params.dbowner)
  CALL fgldbsch_set_parameter("tabname", ext_params.tabname)
  CALL fgldbsch_set_parameter("cvmeth", ext_params.cvmeth)
  CALL fgldbsch_set_parameter("casens", ext_params.casens)
  IF ext_params.systables THEN
     CALL fgldbsch_set_parameter("systabs", "Y")
  ELSE
     CALL fgldbsch_set_parameter("systabs", "N")
  END IF
  IF ext_params.ignerrors THEN
     CALL fgldbsch_set_parameter("ignerrors", "Y")
  ELSE
     CALL fgldbsch_set_parameter("ignerrors", "N")
  END IF

  LET r = fgldbsch_connect()
  IF r<0 THEN
     CALL fgldbsch_fini()
     LET r=-2
     GOTO ext_end
  END IF

  LET sql_params.dbtype = fgldbsch_get_db_type()
  IF sql_params.dbtype IS NULL THEN
     LET r=-3
     GOTO ext_end
  END IF

  CALL tables.clear()

  CALL fglt_log_write("Starting extraction...")
  CALL fglt_log_write("")

  IF fgldbsch_extract() < 0 THEN
     LET r = fgldbsch_disconnect()
     CALL fgldbsch_fini()
     CALL fglt_log_write("ERROR: fgldbsch_extract() call failed.")
     CALL fglt_log_write("")
     LET r=-4
     GOTO ext_end
  END IF

  CASE sql_params.dbtype
       WHEN "IFX" LET r = sql_ext_ifx()
       WHEN "ORA" LET r = sql_ext_ora()
       WHEN "DB2" LET r = sql_ext_db2()
       WHEN "PGS" LET r = sql_ext_pgs()
       WHEN "MYS" LET r = sql_ext_mys()
       WHEN "MSV" LET r = sql_ext_msv()
       WHEN "HDB" LET r = sql_ext_hdb()
  END CASE

  IF r<0 THEN
     GOTO ext_end
  END IF

  # Replace duplicate constraint names if needed:
  # MySQL 5.1 allows duplicated constraint names (even for the same table), but
  # other databases like Informix required unique names accross ALL tables!!!
  CALL fglt_log_write("")
  CALL fglt_log_write("Checking for duplicate constraint names...")
  CALL fglt_log_write("")
  LET r = extract_check_unique_names()
  IF r<0 THEN
     GOTO ext_end
  END IF

  CALL fglt_log_write("")
  CALL fglt_log_write(SFMT("Number of tables extracted: %1",tables.getLength()))
  IF NOT int_flag THEN
     CALL fglt_log_show()
  END IF

LABEL ext_end:
  LET x = fgldbsch_disconnect()
  CALL fgldbsch_fini()
  CALL fglt_log_save("dbextract.log")
  CALL fglt_log_close()

  RETURN r

END FUNCTION

FUNCTION extract_check_unique_names()
  DEFINE pb, tn, et, en, otn, oet, oen INTEGER
  DEFINE newname STRING
  CALL schema_check_unique_names(ext_params.casens==0) RETURNING pb, tn, et, en, otn, oet, oen
  IF pb == 0 THEN RETURN 0 END IF
  CASE pb
      WHEN -1 -- Duplicated table name???
          CALL fglt_log_write(SFMT("Table name '%1' duplicated. This should not happen. Extraction stopped.",
                                   tables[tn].tabname))
          RETURN -15
      WHEN -2
          LET newname = constraint_get_unique_name(constype_pkey,tn)
          CALL fglt_log_write(SFMT("Primary key name '%1' duplicated for table %2. New name = '%3'",
                                   tables[tn].pkeyname, tables[otn].tabname, newname))
          LET tables[tn].pkeyname = newname
      WHEN -3 -- Duplicated column name???
          CALL fglt_log_write(SFMT("Column name '%1' duplicated for table '%2'. This should not happen. Extraction stopped.",
                                   tables[tn].cols[en].colname, tables[tn].tabname))
          RETURN -16
      WHEN -4
          LET newname = constraint_get_unique_name(constype_skey,tn)
          CALL fglt_log_write(SFMT("Unique constraint name '%1' duplicated for table %2. New name = '%3'",
                                   tables[tn].skeys[en].skeyname, tables[tn].tabname, newname))
          LET tables[tn].skeys[en].skeyname = newname
      WHEN -5
          LET newname = constraint_get_unique_name(constype_fkey,tn)
          CALL fglt_log_write(SFMT("Foreign key constraint name '%1' duplicated for table %2. New name = '%3'",
                                   tables[tn].fkeys[en].fkeyname, tables[tn].tabname, newname))
          LET tables[tn].fkeys[en].fkeyname = newname
      WHEN -6
          LET newname = constraint_get_unique_name(constype_chck,tn)
          CALL fglt_log_write(SFMT("Check constraint name '%1' duplicated for table %2. New name = '%3'",
                                   tables[tn].checks[en].checkname, tables[tn].tabname, newname))
          LET tables[tn].checks[en].checkname = newname
  END CASE
  RETURN 0
END FUNCTION

FUNCTION casens_name(n)
  DEFINE n STRING
  CASE ext_params.casens
       WHEN 1 RETURN UPSHIFT(n)
       WHEN 2 RETURN DOWNSHIFT(n)
  END CASE
  RETURN n
END FUNCTION

#-------------------------------------------------------------------------------

FUNCTION ifx_columnlist(tabid,columns)
  DEFINE tabid INTEGER
  DEFINE columns STRING
  DEFINE stmt, list STRING
  DEFINE colname VARCHAR(128)
  LET stmt = "SELECT colname FROM syscolumns"
          || " WHERE tabid = " || tabid
          || "   AND colno IN " || columns
  DECLARE ifx_columnlist CURSOR FROM stmt
  LET list = NULL
  FOREACH ifx_columnlist INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION

FUNCTION sql_ext_ifx()
  DEFINE tabname   VARCHAR(128)
  DEFINE colname   VARCHAR(128)
  DEFINE colnull   SMALLINT
  DEFINE coldflt   VARCHAR(2000)
  DEFINE hasdefs   INTEGER
  DEFINE deftype   CHAR(1)
  DEFINE deflitr   CHAR(256)
  DEFINE constrid  INTEGER
  DEFINE cstrname  VARCHAR(128)
  DEFINE owner     VARCHAR(32)
  DEFINE cstrtype  CHAR(1)
  DEFINE idxname   VARCHAR(128)
  DEFINE columns   VARCHAR(500)
  DEFINE primary   INTEGER
  DEFINE tabid     INTEGER
  DEFINE ptabid    INTEGER
  DEFINE rcstrname VARCHAR(128)
  DEFINE delrule   CHAR(1)
  DEFINE updrule   CHAR(1)
  DEFINE rtabname  VARCHAR(128)
  DEFINE rcolumns  VARCHAR(500)
  DEFINE tabnum    INTEGER
  DEFINE x,s,c     INTEGER
  DEFINE typenum   SMALLINT
  DEFINE typel1, typel2 INTEGER
  DEFINE curr_tabname VARCHAR(128)

  PREPARE sd FROM "SELECT t.tabname FROM informix.systables t WHERE t.tabname = 'sysdefaults'"
  EXECUTE sd INTO tabname
  LET hasdefs = (SQLCA.SQLCODE==0)
  IF hasdefs THEN
     PREPARE stcd_ifx FROM
             "SELECT D.TYPE, D.DEFAULT"
             || "  FROM SYSDEFAULTS D, SYSTABLES T, SYSCOLUMNS C"
             || " WHERE T.TABID = D.TABID"
             || "  AND C.TABID = D.TABID"
             || "  AND C.COLNO = D.COLNO"
             || "  AND T.OWNER = ? AND T.TABNAME = ? AND C.COLNAME = ?"
  END IF

  DECLARE cifx_pkey CURSOR FROM
             "SELECT "
             || " P.CONSTRID,"
             || " P.CONSTRNAME,"
             || " P.OWNER,"
             || " P.TABID,"
             || " P.CONSTRTYPE,"
             || " P.IDXNAME,"
             || " T.TABNAME,"
             || " '('||I.PART1  || ',' ||"
             || "      I.PART2  || ',' ||"
             || "      I.PART3  || ',' ||"
             || "      I.PART4  || ',' ||"
             || "      I.PART5  || ',' ||"
             || "      I.PART6  || ',' ||"
             || "      I.PART7  || ',' ||"
             || "      I.PART8  || ',' ||"
             || "      I.PART9  || ',' ||"
             || "      I.PART10 || ',' ||"
             || "      I.PART11 || ',' ||"
             || "      I.PART12 || ',' ||"
             || "      I.PART13 || ',' ||"
             || "      I.PART14 || ',' ||"
             || "      I.PART15 || ',' ||"
             || "      I.PART16 || ')'"
             || "  FROM SYSCONSTRAINTS P, SYSINDEXES I, SYSTABLES T"
             || " WHERE P.CONSTRTYPE = 'P'"
             || " AND I.IDXNAME = P.IDXNAME"
             || " AND T.TABID = P.TABID"
             || " AND T.OWNER = ? AND T.TABNAME = ?"

  DECLARE cifx_skey CURSOR FROM
             "SELECT "
             || " P.CONSTRID,"
             || " P.CONSTRNAME,"
             || " P.OWNER,"
             || " P.TABID,"
             || " P.CONSTRTYPE,"
             || " P.IDXNAME,"
             || " T.TABNAME,"
             || " '('||I.PART1 || ',' ||"
             || " I.PART2 || ',' ||"
             || " I.PART3 || ',' ||"
             || " I.PART4 || ',' ||"
             || " I.PART5 || ',' ||"
             || " I.PART6 || ',' ||"
             || " I.PART7 || ',' ||"
             || " I.PART8 || ',' ||"
             || " I.PART9 || ',' ||"
             || " I.PART10 || ',' ||"
             || " I.PART11 || ',' ||"
             || " I.PART12 || ',' ||"
             || " I.PART13 || ',' ||"
             || " I.PART14 || ',' ||"
             || " I.PART15 || ',' ||"
             || " I.PART16 || ')'"
             || " FROM SYSCONSTRAINTS P, SYSINDEXES I, SYSTABLES T"
             || " WHERE P.CONSTRTYPE = 'U'"
             || " AND I.IDXNAME = P.IDXNAME"
             || " AND T.TABID = P.TABID"
             || " AND T.OWNER = ? AND T.TABNAME = ?"

  DECLARE cifx_fkey CURSOR FROM
             "SELECT "
             || " R.CONSTRID,"
             || " R.PRIMARY,"
             || " R.PTABID,"
             || " P.CONSTRNAME,"
             || " P2.CONSTRNAME,"
             || " P.OWNER,"
             || " P.TABID,"
             || " P.CONSTRTYPE,"
             || " R.DELRULE,"
             || " R.UPDRULE,"
             || " P.IDXNAME,"
             || " T.TABNAME,"
             || " T2.TABNAME,"
             || " '('||I.PART1 || ',' ||"
             || " I.PART2 || ',' ||"
             || " I.PART3 || ',' ||"
             || " I.PART4 || ',' ||"
             || " I.PART5 || ',' ||"
             || " I.PART6 || ',' ||"
             || " I.PART7 || ',' ||"
             || " I.PART8 || ',' ||"
             || " I.PART9 || ',' ||"
             || " I.PART10 || ',' ||"
             || " I.PART11 || ',' ||"
             || " I.PART12 || ',' ||"
             || " I.PART13 || ',' ||"
             || " I.PART14 || ',' ||"
             || " I.PART15 || ',' ||"
             || " I.PART16 || ')',"
             || " '('||I2.PART1 || ',' ||"
             || " I2.PART2 || ',' ||"
             || " I2.PART3 || ',' ||"
             || " I2.PART4 || ',' ||"
             || " I2.PART5 || ',' ||"
             || " I2.PART6 || ',' ||"
             || " I2.PART7 || ',' ||"
             || " I2.PART8 || ',' ||"
             || " I2.PART9 || ',' ||"
             || " I2.PART10 || ',' ||"
             || " I2.PART11 || ',' ||"
             || " I2.PART12 || ',' ||"
             || " I2.PART13 || ',' ||"
             || " I2.PART14 || ',' ||"
             || " I2.PART15 || ',' ||"
             || " I2.PART16 || ')'"
             || " FROM SYSREFERENCES R,"
             || " SYSCONSTRAINTS P, SYSINDEXES I, SYSTABLES T,"
             || " SYSCONSTRAINTS P2, SYSINDEXES I2, SYSTABLES T2"
             || " WHERE P.CONSTRTYPE = 'R'"
             || " AND R.CONSTRID = P.CONSTRID AND I.IDXNAME = P.IDXNAME AND T.TABID = P.TABID"
             || " AND R.PRIMARY = P2.CONSTRID AND T2.TABID = R.PTABID AND I2.IDXNAME = P2.IDXNAME"
             || " AND T.OWNER = ? AND T.TABNAME = ?"

  DECLARE cifx_chk CURSOR FROM
             "SELECT DISTINCT"
             || " P.CONSTRID,"
             || " P.CONSTRNAME,"
             || " P.OWNER,"
             || " T.TABID,"
             || " T.TABNAME"
             || " FROM SYSCONSTRAINTS P, SYSTABLES T, SYSCHECKS C"
             || " WHERE P.CONSTRID = C.CONSTRID"
             || " AND T.TABID = P.TABID"
             || " AND C.TYPE = 'T'"
             || " AND T.OWNER = ? AND T.TABNAME = ?"

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_ifx
        END IF
        LET coldflt = NULL
        IF hasdefs THEN
           LET deftype = NULL
           LET deflitr = NULL
           EXECUTE stcd_ifx USING ext_params.dbowner, curr_tabname, coldef.colname INTO deftype, deflitr
           LET coldflt = ifx_coldefault(deftype, typenum, deflitr CLIPPED)
        END IF
        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     FOREACH cifx_pkey USING ext_params.dbowner, curr_tabname
        INTO constrid,
             cstrname,
             owner,
             tabid,
             cstrtype,
             idxname,
             tabname,
             columns
        LET tables[tabnum].pkeyname = casens_name(cstrname)
        DECLARE cifx_keycols CURSOR FROM
                "SELECT colname FROM syscolumns"
                || " WHERE tabid = " || tabid
                || "   AND colno IN " || columns
        FOREACH cifx_keycols INTO colname
            LET x = column_lookup(tabnum,colname)
            LET tables[tabnum].cols[x].pkeycol = 1
        END FOREACH
     END FOREACH

     FOREACH cifx_skey USING ext_params.dbowner, curr_tabname
        INTO constrid,
             cstrname,
             owner,
             tabid,
             cstrtype,
             idxname,
             tabname,
             columns
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = ifx_columnlist(tabid,columns)
     END FOREACH

     FOREACH cifx_fkey USING ext_params.dbowner, curr_tabname
        INTO constrid,
             primary,
             ptabid,
             cstrname,
             rcstrname,
             owner,
             tabid,
             cstrtype,
             delrule,
             updrule,
             idxname,
             tabname,
             rtabname,
             columns,
             rcolumns
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = ifx_columnlist(tabid,columns)
        LET tables[tabnum].fkeys[x].reftabname = casens_name(rtabname)
        LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        -- Remark: Default is 'R' (Restrict) for Informix!
        -- For most other databases the default is NO ACTION!
        CASE delrule
           WHEN "C"       LET tables[tabnum].fkeys[x].delrule = udrule_cascade
           OTHERWISE {R}  LET tables[tabnum].fkeys[x].delrule = udrule_default
        END CASE
        CASE updrule
           WHEN "C"       LET tables[tabnum].fkeys[x].updrule = udrule_cascade
           OTHERWISE {R}  LET tables[tabnum].fkeys[x].updrule = udrule_default
        END CASE

     END FOREACH

     FOREACH cifx_chk USING ext_params.dbowner, curr_tabname
        INTO constrid,
             cstrname,
             owner,
             tabid,
             tabname
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = ifx_checktext(constrid)
     END FOREACH

LABEL next_table_ifx:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0

END FUNCTION

FUNCTION ifx_coldefault(deftype, typenum, defval)
  DEFINE deftype CHAR(1)
  DEFINE typenum SMALLINT
  DEFINE defval STRING
  DEFINE x,y SMALLINT
  DEFINE v STRING

  CASE deftype
      WHEN "N"  RETURN "NULL"
      WHEN "C"  RETURN "CURRENT"
      WHEN "T"  RETURN "TODAY"
      WHEN "U"  RETURN "USER"
  END CASE

  -- deftype == L (literal)

  CASE typenum
      WHEN dtn_char      LET v = "char"
      WHEN dtn_varchar   LET v = "char"
      WHEN dtn_nchar     LET v = "char"
      WHEN dtn_nvarchar  LET v = "char"
      WHEN dtn_text      LET v = "char"
      OTHERWISE          LET v = "coded"
  END CASE

  IF v = "char" THEN
     LET v = "'"||defval||"'" -- FIXME: Handle quotes in default value!
  ELSE
     LET x = defval.getIndexOf(" ",1) + 1
     LET y = defval.getLength()
     LET v = defval.subString(x,y)
  END IF

  RETURN v

END FUNCTION

FUNCTION ifx_decimal(ifxleng)
  DEFINE ifxleng SMALLINT
  DEFINE p, s SMALLINT
  LET p = (ifxleng / 256)
  LET s = (ifxleng MOD 256)
  IF s == 255 THEN LET s = NULL END IF
  RETURN p, s
END FUNCTION

FUNCTION ifx_datetime(ifxleng)
  DEFINE ifxleng SMALLINT
  DEFINE q1, q2 SMALLINT
  LET q1 = (ifxleng MOD 256) / 16
  LET q2 = (ifxleng MOD 256) MOD 16
  CASE q1
      WHEN dtiqual_year
           CASE q2
                WHEN dtiqual_year    RETURN dtn_datetime_year_to_year
                WHEN dtiqual_month   RETURN dtn_datetime_year_to_month
                WHEN dtiqual_day     RETURN dtn_datetime_year_to_day
                WHEN dtiqual_hour    RETURN dtn_datetime_year_to_hour
                WHEN dtiqual_minute  RETURN dtn_datetime_year_to_minute
                WHEN dtiqual_second  RETURN dtn_datetime_year_to_second
                WHEN dtiqual_frac1   RETURN dtn_datetime_year_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_year_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_year_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_year_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_year_to_frac5
           END CASE
      WHEN dtiqual_month
           CASE q2
                WHEN dtiqual_month   RETURN dtn_datetime_month_to_month
                WHEN dtiqual_day     RETURN dtn_datetime_month_to_day
                WHEN dtiqual_hour    RETURN dtn_datetime_month_to_hour
                WHEN dtiqual_minute  RETURN dtn_datetime_month_to_minute
                WHEN dtiqual_second  RETURN dtn_datetime_month_to_second
                WHEN dtiqual_frac1   RETURN dtn_datetime_month_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_month_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_month_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_month_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_month_to_frac5
           END CASE
      WHEN dtiqual_day
           CASE q2
                WHEN dtiqual_day     RETURN dtn_datetime_day_to_day
                WHEN dtiqual_hour    RETURN dtn_datetime_day_to_hour
                WHEN dtiqual_minute  RETURN dtn_datetime_day_to_minute
                WHEN dtiqual_second  RETURN dtn_datetime_day_to_second
                WHEN dtiqual_frac1   RETURN dtn_datetime_day_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_day_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_day_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_day_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_day_to_frac5
           END CASE
      WHEN dtiqual_hour
           CASE q2
                WHEN dtiqual_hour    RETURN dtn_datetime_hour_to_hour
                WHEN dtiqual_minute  RETURN dtn_datetime_hour_to_minute
                WHEN dtiqual_second  RETURN dtn_datetime_hour_to_second
                WHEN dtiqual_frac1   RETURN dtn_datetime_hour_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_hour_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_hour_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_hour_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_hour_to_frac5
           END CASE
      WHEN dtiqual_minute
           CASE q2
                WHEN dtiqual_minute  RETURN dtn_datetime_minute_to_minute
                WHEN dtiqual_second  RETURN dtn_datetime_minute_to_second
                WHEN dtiqual_frac1   RETURN dtn_datetime_minute_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_minute_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_minute_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_minute_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_minute_to_frac5
           END CASE
      WHEN dtiqual_second
           CASE q2
                WHEN dtiqual_second  RETURN dtn_datetime_second_to_second
                WHEN dtiqual_frac1   RETURN dtn_datetime_second_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_second_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_second_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_second_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_second_to_frac5
           END CASE
      WHEN dtiqual_fraction
           CASE q2
                WHEN dtiqual_frac1   RETURN dtn_datetime_fraction_to_frac1
                WHEN dtiqual_frac2   RETURN dtn_datetime_fraction_to_frac2
                WHEN dtiqual_frac3   RETURN dtn_datetime_fraction_to_frac3
                WHEN dtiqual_frac4   RETURN dtn_datetime_fraction_to_frac4
                WHEN dtiqual_frac5   RETURN dtn_datetime_fraction_to_frac5
           END CASE
  END CASE
  RETURN -1 
END FUNCTION

FUNCTION ifx_interval(ifxleng)
  DEFINE ifxleng SMALLINT
  DEFINE t, l, q1, q2 SMALLINT
  LET t = -1
  LET q1 = (ifxleng MOD 256) / 16
  LET q2 = (ifxleng MOD 256) MOD 16
  LET l  = (ifxleng / 256) - (q2 - q1)
  CASE q1
      WHEN dtiqual_year
           CASE q2
                WHEN dtiqual_year    LET t = dtn_interval_year_to_year
                WHEN dtiqual_month   LET t = dtn_interval_year_to_month
           END CASE
      WHEN dtiqual_month
           CASE q2
                WHEN dtiqual_month   LET t = dtn_interval_month_to_month
           END CASE
      WHEN dtiqual_day
           CASE q2
                WHEN dtiqual_day     LET t = dtn_interval_day_to_day
                WHEN dtiqual_hour    LET t = dtn_interval_day_to_hour
                WHEN dtiqual_minute  LET t = dtn_interval_day_to_minute
                WHEN dtiqual_second  LET t = dtn_interval_day_to_second
                WHEN dtiqual_frac1   LET t = dtn_interval_day_to_frac1
                WHEN dtiqual_frac2   LET t = dtn_interval_day_to_frac2
                WHEN dtiqual_frac3   LET t = dtn_interval_day_to_frac3
                WHEN dtiqual_frac4   LET t = dtn_interval_day_to_frac4
                WHEN dtiqual_frac5   LET t = dtn_interval_day_to_frac5
           END CASE
      WHEN dtiqual_hour
           CASE q2
                WHEN dtiqual_hour    LET t = dtn_interval_hour_to_hour
                WHEN dtiqual_minute  LET t = dtn_interval_hour_to_minute
                WHEN dtiqual_second  LET t = dtn_interval_hour_to_second
                WHEN dtiqual_frac1   LET t = dtn_interval_hour_to_frac1
                WHEN dtiqual_frac2   LET t = dtn_interval_hour_to_frac2
                WHEN dtiqual_frac3   LET t = dtn_interval_hour_to_frac3
                WHEN dtiqual_frac4   LET t = dtn_interval_hour_to_frac4
                WHEN dtiqual_frac5   LET t = dtn_interval_hour_to_frac5
           END CASE
      WHEN dtiqual_minute
           CASE q2
                WHEN dtiqual_minute  LET t = dtn_interval_minute_to_minute
                WHEN dtiqual_second  LET t = dtn_interval_minute_to_second
                WHEN dtiqual_frac1   LET t = dtn_interval_minute_to_frac1
                WHEN dtiqual_frac2   LET t = dtn_interval_minute_to_frac2
                WHEN dtiqual_frac3   LET t = dtn_interval_minute_to_frac3
                WHEN dtiqual_frac4   LET t = dtn_interval_minute_to_frac4
                WHEN dtiqual_frac5   LET t = dtn_interval_minute_to_frac5
           END CASE
      WHEN dtiqual_second
           CASE q2
                WHEN dtiqual_second  LET t = dtn_interval_second_to_second
                WHEN dtiqual_frac1   LET t = dtn_interval_second_to_frac1
                WHEN dtiqual_frac2   LET t = dtn_interval_second_to_frac2
                WHEN dtiqual_frac3   LET t = dtn_interval_second_to_frac3
                WHEN dtiqual_frac4   LET t = dtn_interval_second_to_frac4
                WHEN dtiqual_frac5   LET t = dtn_interval_second_to_frac5
           END CASE
      WHEN dtiqual_fraction
           CASE q2
                WHEN dtiqual_frac1   LET t = dtn_interval_fraction_to_frac1
                WHEN dtiqual_frac2   LET t = dtn_interval_fraction_to_frac2
                WHEN dtiqual_frac3   LET t = dtn_interval_fraction_to_frac3
                WHEN dtiqual_frac4   LET t = dtn_interval_fraction_to_frac4
                WHEN dtiqual_frac5   LET t = dtn_interval_fraction_to_frac5
           END CASE
  END CASE
  RETURN t, l
END FUNCTION

-- Converts Informix-style data type to schema manage data type
FUNCTION convert_datatype_ifx( ifxtype, ifxleng )
  DEFINE ifxtype SMALLINT
  DEFINE ifxleng SMALLINT
  DEFINE modv SMALLINT
  DEFINE typenum SMALLINT
  DEFINE typel1, typel2 INTEGER
  DEFINE colnull SMALLINT

  LET colnull = ( ifxtype / 256 )

  LET modv = ( ifxtype MOD 256 )

  LET typel1 = NULL
  LET typel2 = NULL

  CASE
       --LET ifxtype = ( ifxtype MOD 2048 ) -- Distinct type
       --WHEN modv=40 # LVARCHAR?
       WHEN modv=dtid_char
            LET typenum = dtn_char
            LET typel1 = ifxleng
       WHEN modv=dtid_boolean
            LET typenum = dtn_boolean
       WHEN modv=dtid_smallint
            LET typenum = dtn_smallint
       WHEN modv=dtid_integer
            LET typenum = dtn_integer
       WHEN modv=dtid_bigint
            LET typenum = dtn_bigint
       WHEN modv=dtid_int8
            LET typenum = dtn_int8
       WHEN modv=dtid_float
            LET typenum = dtn_float
       WHEN modv=dtid_smallfloat
            LET typenum = dtn_smallfloat
       WHEN modv=dtid_decimal
            LET typenum = dtn_decimal
            CALL ifx_decimal(ifxleng) RETURNING typel1, typel2
       WHEN modv=dtid_serial
            LET typenum = dtn_serial
       WHEN modv=dtid_bigserial
            LET typenum = dtn_bigserial
       WHEN modv=dtid_serial8
            LET typenum = dtn_serial8
       WHEN modv=dtid_date
            LET typenum = dtn_date
       WHEN modv=dtid_money
            LET typenum = dtn_money
            CALL ifx_decimal(ifxleng) RETURNING typel1, typel2
       WHEN modv=dtid_datetime
            LET typenum = ifx_datetime(ifxleng)
       WHEN modv=dtid_text
            LET typenum = dtn_text
       WHEN modv=dtid_byte
            LET typenum = dtn_byte
       WHEN modv=dtid_varchar
            LET typenum = dtn_varchar
            LET typel1 = ifxleng MOD 256
       WHEN modv=dtid_interval
            CALL ifx_interval(ifxleng) RETURNING typenum, typel1
       WHEN modv=dtid_nchar
            LET typenum = dtn_nchar
            LET typel1 = ifxleng
       WHEN modv=dtid_nvarchar
            LET typenum = dtn_nvarchar
            LET typel1 = ifxleng MOD 256
       WHEN modv=dtid_varchar2
            LET typenum = dtn_varchar2
            LET typel1 = ifxleng
       WHEN modv=dtid_nvarchar2
            LET typenum = dtn_nvarchar2
            LET typel1 = ifxleng
       OTHERWISE
            LET typenum = -1
  END CASE

  RETURN typenum, typel1, typel2, colnull

END FUNCTION

FUNCTION ifx_checktext(cid)
  DEFINE cid INTEGER
  DEFINE ln INTEGER
  DEFINE tp CHAR(32)
  DEFINE sqlcond STRING

  DECLARE c_ifx_check CURSOR FOR
    SELECT seqno, checktext FROM syschecks
     WHERE constrid = cid AND type = "T"
     ORDER BY seqno

  FOREACH c_ifx_check INTO ln, tp
     IF sqlcond IS NULL THEN
        LET sqlcond = tp CLIPPED
     ELSE
        LET sqlcond = sqlcond, tp CLIPPED
     END IF
  END FOREACH

  RETURN sqlcond

END FUNCTION


#-------------------------------------------------------------------------------

FUNCTION ora_columnlist(owner,constrname)
  DEFINE owner, constrname VARCHAR(100)
  DEFINE colname VARCHAR(100)
  DEFINE stmt,list STRING
  LET stmt = "SELECT COLUMN_NAME FROM ALL_CONS_COLUMNS"
          || " WHERE CONSTRAINT_NAME = '" || UPSHIFT(constrname) || "'"
  IF owner IS NOT NULL THEN
     LET stmt = stmt || " AND OWNER = '" || UPSHIFT(owner) || "'"
  END IF
  LET stmt = stmt || " ORDER BY POSITION"
  DECLARE ora_columnlist CURSOR FROM stmt
  LET list = NULL
  FOREACH ora_columnlist INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION

FUNCTION sql_ext_ora()
  DEFINE colname   VARCHAR(200)
  DEFINE colnull   INTEGER
  DEFINE coldflt   VARCHAR(2000)
  DEFINE owner     VARCHAR(100)
  DEFINE cstrname  VARCHAR(100)
  DEFINE delrule   VARCHAR(30)
  DEFINE rowner    VARCHAR(100)
  DEFINE rcstrname VARCHAR(100)
  DEFINE rtabname  VARCHAR(200)
  DEFINE sqlcond   VARCHAR(30000)
  DEFINE tabnum    INTEGER
  DEFINE x,s,c     INTEGER
  DEFINE typenum   SMALLINT
  DEFINE typel1, typel2 INTEGER
  DEFINE up_dbowner VARCHAR(200)
  DEFINE curr_tabname VARCHAR(200)

  DECLARE cora_cdef CURSOR FROM
             "SELECT"
             || "  C.DATA_DEFAULT"
             || " FROM ALL_TAB_COLUMNS C"
             || " WHERE C.TABLE_NAME = ?"
             || "   AND UPPER(C.OWNER) = ?"
             || "   AND C.COLUMN_NAME = ?"

  DECLARE cora_pkey CURSOR FROM
             "SELECT"
             || "  C.OWNER,"
             || "  C.CONSTRAINT_NAME,"
             || "  X.COLUMN_NAME"
             || " FROM ALL_CONSTRAINTS C, ALL_CONS_COLUMNS X"
             || " WHERE C.CONSTRAINT_TYPE='P'"
             || "   AND X.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND UPPER(C.OWNER) = ?"
             || "   AND C.TABLE_NAME = ?"
             || " ORDER BY X.POSITION"

  DECLARE cora_skey CURSOR FROM
             "SELECT"
             || "  C.OWNER,"
             || "  C.CONSTRAINT_NAME"
             || " FROM ALL_CONSTRAINTS C"
             || " WHERE C.CONSTRAINT_TYPE='U'"
             || "   AND UPPER(C.OWNER) = ?"
             || "   AND C.TABLE_NAME = ?"

  -- No test on R.OWNER??? are constraint names unique???
  DECLARE cora_fkey CURSOR FROM
             "SELECT"
             || "  C.OWNER,"
             || "  C.CONSTRAINT_NAME,"
             || "  C.DELETE_RULE,"
             || "  R.OWNER,"
             || "  R.CONSTRAINT_NAME,"
             || "  R.TABLE_NAME"
             || " FROM ALL_CONSTRAINTS C, ALL_CONSTRAINTS R"
             || " WHERE C.CONSTRAINT_TYPE='R'"
             || "   AND C.R_OWNER=R.OWNER"
             || "   AND C.R_CONSTRAINT_NAME=R.CONSTRAINT_NAME"
             || "   AND UPPER(C.OWNER) = ?"
             || "   AND C.TABLE_NAME = ?"

  DECLARE cora_chk CURSOR FROM
             "SELECT"
             || "  C.OWNER,"
             || "  C.CONSTRAINT_NAME,"
             || "  C.SEARCH_CONDITION"
             || " FROM ALL_CONSTRAINTS C"
             || " WHERE C.CONSTRAINT_TYPE='C'"
             || "   AND UPPER(C.OWNER) = ?"
             || "   AND C.TABLE_NAME = ?"
{ This does not work anymore with Oracle 10g...
             -- Warning: ignore the NOT NULL constraints!
             || "   AND C.SEARCH_CONDITION NOT LIKE '% IS NOT NULL'"
}

  LET up_dbowner = UPSHIFT(ext_params.dbowner)

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_ora
        END IF
        LET coldflt = NULL
        EXECUTE cora_cdef USING up_dbowner, curr_tabname, coldef.colname INTO coldflt
        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     FOREACH cora_pkey USING up_dbowner, curr_tabname
        INTO owner, cstrname, colname
        LET tables[tabnum].pkeyname = casens_name(cstrname)
        LET x = column_lookup(tabnum,colname)
        LET tables[tabnum].cols[x].pkeycol = 1
     END FOREACH

     FOREACH cora_skey USING up_dbowner, curr_tabname
        INTO owner, cstrname
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = ora_columnlist(owner,cstrname)
     END FOREACH

     FOREACH cora_fkey USING up_dbowner, curr_tabname
        INTO owner, cstrname, delrule, rowner, rcstrname, rtabname
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = ora_columnlist(owner,cstrname)
        LET tables[tabnum].fkeys[x].reftabname = casens_name(rtabname)
        LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        IF delrule=="CASCADE" THEN
           LET tables[tabnum].fkeys[x].delrule = "C"
        ELSE
           LET tables[tabnum].fkeys[x].delrule = "R"
        END IF
     END FOREACH

     FOREACH cora_chk USING up_dbowner, curr_tabname
        INTO owner, cstrname, sqlcond
        LET sqlcond = sqlcond CLIPPED
        -- Warning: ignore the NOT NULL constraints!
        IF sqlcond MATCHES "* IS NOT NULL" THEN CONTINUE FOREACH END IF
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = sqlcond
     END FOREACH

LABEL next_table_ora:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0

END FUNCTION

#-------------------------------------------------------------------------------

FUNCTION sql_ext_db2()
  DEFINE colname   VARCHAR(200)
  DEFINE tmpname   VARCHAR(200)
  DEFINE colnull   INTEGER
  DEFINE coldflt   VARCHAR(2000)
  DEFINE cstrname  VARCHAR(200)
  DEFINE rcstrname VARCHAR(200)
  DEFINE rtabname  VARCHAR(200)
  DEFINE delrule   CHAR(1)
  DEFINE updrule   CHAR(1)
  DEFINE sqlcond   TEXT
  DEFINE tabnum    INTEGER
  DEFINE x,s,c     INTEGER
  DEFINE typenum, typel1, typel2 SMALLINT
  DEFINE curr_tabname VARCHAR(200)
  DEFINE up_dbowner VARCHAR(200)

  LOCATE sqlcond IN MEMORY

  DECLARE cdb2_cdef CURSOR FROM
             "SELECT"
             || "  CAST(C.DEFAULT AS VARCHAR(2000))"
             || " FROM SYSCAT.COLUMNS C"
             || " WHERE UPPER(C.TABSCHEMA) = ? AND C.TABNAME = ? AND C.COLNAME = ?"

  DECLARE cdb2_pkey CURSOR FROM
             "SELECT"
             || "  C.CONSTNAME"
             || " FROM SYSCAT.TABCONST C"
             || " WHERE C.TYPE='P'"
             || " AND C.TABNAME = ? AND UPPER(C.TABSCHEMA) = ?"

  DECLARE cdb2_skey CURSOR FROM
             "SELECT"
             || "  C.CONSTNAME"
             || " FROM SYSCAT.TABCONST C"
             || " WHERE C.TYPE='U'"
             || " AND C.TABNAME = ? AND UPPER(C.TABSCHEMA) = ?"

  DECLARE cdb2_fkey CURSOR FROM
             "SELECT"
             || "  R.CONSTNAME,"
             || "  R.REFKEYNAME,"
             || "  R.REFTABNAME,"
             || "  R.DELETERULE,"
             || "  R.UPDATERULE"
             || " FROM SYSCAT.REFERENCES R"
             || " WHERE R.TABNAME = ? AND UPPER(R.TABSCHEMA) = ?"


  DECLARE cdb2_chk CURSOR FROM
             "SELECT"
             || "  O.COLNAME,"
             || "  C.TEXT"
             || " FROM SYSCAT.CHECKS C, SYSCAT.COLCHECKS O"
             || " WHERE C.TABNAME = ? AND UPPER(C.TABSCHEMA) = ?"
             || "   AND C.TABNAME = O.TABNAME AND C.TABSCHEMA = O.TABSCHEMA"
             || "   AND C.CONSTNAME = O.CONSTNAME"
             || "   AND O.USAGE = 'R'"

  LET up_dbowner = UPSHIFT(ext_params.dbowner)

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_db2
        END IF
        LET coldflt = NULL
        EXECUTE cdb2_cdef USING up_dbowner, curr_tabname, coldef.colname INTO coldflt
        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     EXECUTE cdb2_pkey USING curr_tabname, up_dbowner INTO cstrname
     IF STATUS==0 THEN
        LET tables[tabnum].pkeyname = casens_name(cstrname)
        LET tmpname = UPSHIFT(cstrname)
        DECLARE cdb2_keycols CURSOR FROM
                "SELECT K.COLNAME FROM SYSCAT.KEYCOLUSE K"
              ||" WHERE K.CONSTNAME = ? ORDER BY K.COLSEQ"
        FOREACH cdb2_keycols USING tmpname INTO colname
            LET x = column_lookup(tabnum,colname)
            LET tables[tabnum].cols[x].pkeycol = 1
        END FOREACH
     END IF

     FOREACH cdb2_skey USING curr_tabname, up_dbowner
        INTO cstrname
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = db2_columnlist(cstrname)
     END FOREACH

     FOREACH cdb2_fkey USING curr_tabname, up_dbowner
        INTO cstrname,
             rcstrname,
             rtabname,
             delrule,
             updrule
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = db2_columnlist(cstrname)
        LET tables[tabnum].fkeys[x].reftabname = casens_name(rtabname)
        LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        CASE delrule
          WHEN "C"        LET tables[tabnum].fkeys[x].delrule = udrule_cascade
          WHEN "R"        LET tables[tabnum].fkeys[x].delrule = udrule_restrict
          WHEN "N"        LET tables[tabnum].fkeys[x].delrule = udrule_setnull
          WHEN "D"        LET tables[tabnum].fkeys[x].delrule = udrule_setdefault
          OTHERWISE {A}   LET tables[tabnum].fkeys[x].delrule = udrule_default
        END CASE
        CASE updrule
          WHEN "C"        LET tables[tabnum].fkeys[x].updrule = udrule_cascade
          WHEN "R"        LET tables[tabnum].fkeys[x].updrule = udrule_restrict
          WHEN "N"        LET tables[tabnum].fkeys[x].updrule = udrule_setnull
          WHEN "D"        LET tables[tabnum].fkeys[x].updrule = udrule_setdefault
          OTHERWISE {A}   LET tables[tabnum].fkeys[x].updrule = udrule_default
        END CASE
     END FOREACH

     FOREACH cdb2_chk USING curr_tabname, up_dbowner INTO cstrname, sqlcond
        LET sqlcond = sqlcond CLIPPED
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = sqlcond
     END FOREACH

LABEL next_table_db2:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0
END FUNCTION

FUNCTION db2_columnlist(constrname)
  DEFINE constrname VARCHAR(200)
  DEFINE colname VARCHAR(200)
  DEFINE stmt,list STRING
  LET stmt = "SELECT K.COLNAME FROM SYSCAT.KEYCOLUSE K"
          || " WHERE K.CONSTNAME = '" || UPSHIFT(constrname) || "'"
          || " ORDER BY K.COLSEQ"
  DECLARE db2_columnlist CURSOR FROM stmt
  LET list = NULL
  FOREACH db2_columnlist INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION

#-------------------------------------------------------------------------------

FUNCTION sql_ext_hdb()
  DEFINE colname   VARCHAR(200)
  DEFINE coldflt   VARCHAR(2000)
  DEFINE colnull   INTEGER
  DEFINE cstrname  VARCHAR(200)
  DEFINE cstrsch   VARCHAR(200)
  DEFINE rcstrname VARCHAR(200)
  DEFINE rcstrsch  VARCHAR(200)
  DEFINE rtabname  VARCHAR(200)
  DEFINE delrule   VARCHAR(50)
  DEFINE updrule   VARCHAR(50)
  DEFINE sqlcond   TEXT
  DEFINE tabnum    INTEGER
  DEFINE x,s,c     INTEGER
  DEFINE typenum, typel1, typel2 SMALLINT
  DEFINE curr_tabname VARCHAR(200)
  DEFINE up_dbowner VARCHAR(200)

  LOCATE sqlcond IN MEMORY

  DECLARE chdb_cdef CURSOR FROM
             "SELECT"
             || "  C.DEFAULT_VALUE"
             || " FROM SYS.TABLE_COLUMNS C"
             || " WHERE C.TABLE_NAME = ? AND C.SCHEMA_NAME = ? AND C.COLUMN_NAME = ?"

  DECLARE chdb_pkey CURSOR FROM
             "SELECT"
             || "  C.CONSTRAINT_NAME,"
             || "  C.COLUMN_NAME"
             || " FROM SYS.CONSTRAINTS C"
             || " WHERE C.IS_PRIMARY_KEY = 'TRUE'"
             || "   AND C.TABLE_NAME = ? AND C.SCHEMA_NAME = ?"
             || " ORDER BY C.POSITION"

  DECLARE chdb_skey CURSOR FROM
             "SELECT DISTINCT"
             || "  C.CONSTRAINT_NAME, C.SCHEMA_NAME"
             || " FROM SYS.CONSTRAINTS C"
             || " WHERE C.IS_PRIMARY_KEY = 'FALSE'"
             || "   AND C.IS_UNIQUE_KEY = 'TRUE'"
             || "   AND C.TABLE_NAME = ? AND C.SCHEMA_NAME = ?"

  DECLARE chdb_fkey CURSOR FROM
             "SELECT DISTINCT"
             || " C.CONSTRAINT_NAME, C.SCHEMA_NAME,"
             || " C.REFERENCED_CONSTRAINT_NAME, C.REFERENCED_SCHEMA_NAME,"
             || " C.REFERENCED_TABLE_NAME,"
             || " C.DELETE_RULE, C.UPDATE_RULE"
             || " FROM SYS.REFERENTIAL_CONSTRAINTS C"
             || " WHERE C.TABLE_NAME = ? AND C.SCHEMA_NAME = ?"

{ Does not exist
  DECLARE chdb_chk CURSOR FROM
             "SELECT"
}

  LET up_dbowner = UPSHIFT(ext_params.dbowner)

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_ifx
        END IF
        LET coldflt = NULL
        EXECUTE chdb_cdef USING curr_tabname, up_dbowner, coldef.colname INTO coldflt
        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     FOREACH chdb_pkey USING curr_tabname, up_dbowner
                       INTO cstrname, colname
        IF tables[tabnum].pkeyname IS NULL THEN
           LET tables[tabnum].pkeyname = casens_name(cstrname)
        END IF
        LET x = column_lookup(tabnum,colname)
        LET tables[tabnum].cols[x].pkeycol = 1
     END FOREACH

     FOREACH chdb_skey USING curr_tabname, up_dbowner
                       INTO cstrname, cstrsch
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = hdb_columnlist(cstrname,cstrsch,"U")
     END FOREACH

     FOREACH chdb_fkey USING curr_tabname, up_dbowner
        INTO cstrname,
             cstrsch,
             rcstrname,
             rcstrsch,
             rtabname,
             delrule,
             updrule
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = hdb_columnlist(cstrname,cstrsch,"F")
        LET tables[tabnum].fkeys[x].reftabname = casens_name(rtabname)
        LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        CASE delrule
          WHEN "CASCADE"      LET tables[tabnum].fkeys[x].delrule = udrule_cascade
          WHEN "RESTRICT"     LET tables[tabnum].fkeys[x].delrule = udrule_restrict
          WHEN "SET NULL"     LET tables[tabnum].fkeys[x].delrule = udrule_setnull
          WHEN "SET DEFAULT"  LET tables[tabnum].fkeys[x].delrule = udrule_setdefault
          OTHERWISE           LET tables[tabnum].fkeys[x].delrule = udrule_default
        END CASE
        CASE updrule
          WHEN "CASCADE"      LET tables[tabnum].fkeys[x].updrule = udrule_cascade
          WHEN "RESTRICT"     LET tables[tabnum].fkeys[x].updrule = udrule_restrict
          WHEN "SET NULL"     LET tables[tabnum].fkeys[x].updrule = udrule_setnull
          WHEN "SET DEFAULT"  LET tables[tabnum].fkeys[x].updrule = udrule_setdefault
          OTHERWISE           LET tables[tabnum].fkeys[x].updrule = udrule_default
        END CASE
     END FOREACH

{
     FOREACH chdb_chk USING curr_tabname, up_dbowner
        INTO cstrname, sqlcond
        LET sqlcond = sqlcond CLIPPED
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = sqlcond
     END FOREACH
}

LABEL next_table_ifx:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0
END FUNCTION

FUNCTION hdb_columnlist(constrname,constrsch,constrtyp)
  DEFINE constrname VARCHAR(200)
  DEFINE constrsch  VARCHAR(200)
  DEFINE constrtyp  CHAR(1)
  DEFINE colname VARCHAR(200)
  DEFINE stmt,list STRING
  CASE constrtyp
  WHEN "U"
     LET stmt = "SELECT C.COLUMN_NAME"
             || " FROM SYS.CONSTRAINTS C"
             || " WHERE C.CONSTRAINT_NAME = '" || constrname || "'"
             || "   AND C.SCHEMA_NAME = '" || constrsch || "'"
             || "   AND C.IS_UNIQUE_KEY = 'TRUE'"
             || " ORDER BY C.POSITION"
  WHEN "F"
     LET stmt = "SELECT C.COLUMN_NAME"
             || " FROM SYS.REFERENTIAL_CONSTRAINTS C"
             || " WHERE C.CONSTRAINT_NAME = '" || constrname || "'"
             || "   AND C.SCHEMA_NAME = '" || constrsch || "'"
             || " ORDER BY C.POSITION"
  OTHERWISE
     DISPLAY "ERROR: Invalid constraint type."
     EXIT PROGRAM 1
  END CASE
  DECLARE hdb_columnlist CURSOR FROM stmt
  LET list = NULL
  FOREACH hdb_columnlist INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION


#-------------------------------------------------------------------------------

FUNCTION sql_ext_msv()
  DEFINE colname   VARCHAR(200)
  DEFINE coldflt   VARCHAR(2000)
  DEFINE colnull   INTEGER
  DEFINE cstrname  VARCHAR(200)
  DEFINE cstrsch   VARCHAR(200)
  DEFINE cstrcat   VARCHAR(200)
  DEFINE rcstrname VARCHAR(200)
  DEFINE rcstrsch  VARCHAR(200)
  DEFINE rcstrcat  VARCHAR(200)
  DEFINE rtabname  VARCHAR(200)
  DEFINE rtabsch   VARCHAR(200)
  DEFINE rtabcat   VARCHAR(200)
  DEFINE delrule   VARCHAR(50)
  DEFINE updrule   VARCHAR(50)
  DEFINE sqlcond   TEXT
  DEFINE tabnum    INTEGER
  DEFINE x,s,c     INTEGER
  DEFINE typenum, typel1, typel2 SMALLINT
  DEFINE curr_tabname VARCHAR(200)
  DEFINE up_dbowner VARCHAR(200)

  LOCATE sqlcond IN MEMORY

  PREPARE smsv_cdef FROM
             "SELECT"
             || "  C.COLUMN_DEFAULT"
             || " FROM INFORMATION_SCHEMA.COLUMNS C"
             || " WHERE C.TABLE_NAME = ? AND UPPER(C.TABLE_SCHEMA) = ? AND C.TABLE_CATALOG = DB_NAME()"
             || "   AND C.COLUMN_NAME = ?"

  DECLARE cmsv_pkey CURSOR FROM
             "SELECT"
             || "  C.CONSTRAINT_NAME,"
             || "  U.COLUMN_NAME"
             || " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C,"
             || "      INFORMATION_SCHEMA.KEY_COLUMN_USAGE U"
             || " WHERE C.CONSTRAINT_TYPE = 'PRIMARY KEY'"
             || "   AND U.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND U.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
             || "   AND U.CONSTRAINT_CATALOG = C.CONSTRAINT_CATALOG"
             || "   AND C.TABLE_NAME = ? AND UPPER(C.TABLE_SCHEMA) = ? AND C.TABLE_CATALOG = DB_NAME()"
             || " ORDER BY U.ORDINAL_POSITION"

  DECLARE cmsv_skey CURSOR FROM
             "SELECT DISTINCT"
             || "  C.CONSTRAINT_NAME, C.CONSTRAINT_SCHEMA, C.CONSTRAINT_CATALOG"
             || " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C"
             || " WHERE C.CONSTRAINT_TYPE = 'UNIQUE'"
             || "   AND C.TABLE_NAME = ? AND UPPER(C.TABLE_SCHEMA) = ? AND C.TABLE_CATALOG = DB_NAME()"
             || " ORDER BY C.CONSTRAINT_NAME"

  DECLARE cmsv_fkey CURSOR FROM
             "SELECT"
             || " TC1.CONSTRAINT_NAME, TC1.CONSTRAINT_SCHEMA, TC1.CONSTRAINT_CATALOG,"
             || " TC2.CONSTRAINT_NAME, TC2.CONSTRAINT_SCHEMA, TC2.CONSTRAINT_CATALOG,"
             || " TC2.TABLE_NAME, TC2.TABLE_SCHEMA, TC2.TABLE_CATALOG,"
             || " C.DELETE_RULE, C.UPDATE_RULE"
             || " FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS C,"
             ||      " INFORMATION_SCHEMA.TABLE_CONSTRAINTS TC1,"
             ||      " INFORMATION_SCHEMA.TABLE_CONSTRAINTS TC2"
             || " WHERE TC1.CONSTRAINT_CATALOG = C.CONSTRAINT_CATALOG"
             || "   AND TC1.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
             || "   AND TC1.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND TC2.CONSTRAINT_CATALOG = C.UNIQUE_CONSTRAINT_CATALOG"
             || "   AND TC2.CONSTRAINT_SCHEMA = C.UNIQUE_CONSTRAINT_SCHEMA"
             || "   AND TC2.CONSTRAINT_NAME = C.UNIQUE_CONSTRAINT_NAME"
             || "   AND TC1.TABLE_NAME = ? AND UPPER(TC1.TABLE_SCHEMA) = ? AND TC1.TABLE_CATALOG = DB_NAME()"
             || " ORDER BY C.CONSTRAINT_NAME"

  DECLARE cmsv_chk CURSOR FROM
             "SELECT"
             || "  C.CONSTRAINT_NAME,"
             || "  S.CHECK_CLAUSE"
             || " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C,"
             || "      INFORMATION_SCHEMA.CHECK_CONSTRAINTS S"
             || " WHERE C.CONSTRAINT_TYPE = 'CHECK'"
             || "   AND S.CONSTRAINT_CATALOG = C.CONSTRAINT_CATALOG"
             || "   AND S.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
             || "   AND S.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND C.TABLE_NAME = ? AND UPPER(C.TABLE_SCHEMA) = ? AND C.TABLE_CATALOG = DB_NAME()"
             || " ORDER BY C.CONSTRAINT_NAME"

  LET up_dbowner = UPSHIFT(ext_params.dbowner)

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_msv
        END IF

        LET coldflt = NULL
        EXECUTE smsv_cdef USING curr_tabname, up_dbowner, coldef.colname INTO coldflt

        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     FOREACH cmsv_pkey USING curr_tabname, up_dbowner
                       INTO cstrname, colname
        IF tables[tabnum].pkeyname IS NULL THEN
           LET tables[tabnum].pkeyname = casens_name(cstrname)
        END IF
        LET x = column_lookup(tabnum,colname)
        LET tables[tabnum].cols[x].pkeycol = 1
     END FOREACH

     FOREACH cmsv_skey USING curr_tabname, up_dbowner
                       INTO cstrname, cstrsch, cstrcat
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = msv_columnlist(cstrname,cstrsch,cstrcat)
     END FOREACH

     FOREACH cmsv_fkey USING curr_tabname, up_dbowner
        INTO cstrname,
             cstrsch,
             cstrcat,
             rcstrname,
             rcstrsch,
             rcstrcat,
             rtabname,
             rtabsch,
             rtabcat,
             delrule,
             updrule
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = msv_columnlist(cstrname,cstrsch,cstrcat)
        LET tables[tabnum].fkeys[x].reftabname = casens_name(rtabname)
        LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        CASE delrule
          WHEN "CASCADE"      LET tables[tabnum].fkeys[x].delrule = udrule_cascade
          WHEN "RESTRICT"     LET tables[tabnum].fkeys[x].delrule = udrule_restrict
          WHEN "SET DEFAULT"  LET tables[tabnum].fkeys[x].delrule = udrule_setdefault
          WHEN "SET NULL"     LET tables[tabnum].fkeys[x].delrule = udrule_setnull
          WHEN "NO ACTION"    LET tables[tabnum].fkeys[x].delrule = udrule_default
        END CASE
        CASE updrule
          WHEN "CASCADE"      LET tables[tabnum].fkeys[x].updrule = udrule_cascade
          WHEN "RESTRICT"     LET tables[tabnum].fkeys[x].updrule = udrule_restrict
          WHEN "SET DEFAULT"  LET tables[tabnum].fkeys[x].updrule = udrule_setdefault
          WHEN "SET NULL"     LET tables[tabnum].fkeys[x].updrule = udrule_setnull
          WHEN "NO ACTION"    LET tables[tabnum].fkeys[x].updrule = udrule_default
        END CASE
     END FOREACH

     FOREACH cmsv_chk USING curr_tabname, up_dbowner
        INTO cstrname, sqlcond
        LET sqlcond = sqlcond CLIPPED
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = sqlcond
     END FOREACH

LABEL next_table_msv:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0
END FUNCTION

FUNCTION msv_columnlist(constrname,constrsch,constrcat)
  DEFINE constrname VARCHAR(200)
  DEFINE constrsch  VARCHAR(200)
  DEFINE constrcat  VARCHAR(200)
  DEFINE colname VARCHAR(200)
  DEFINE stmt,list STRING
  LET stmt = "SELECT C.COLUMN_NAME"
          || "  FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE C"
          || " WHERE C.CONSTRAINT_NAME = '" || constrname || "'"
          || "   AND C.CONSTRAINT_SCHEMA = '" || constrsch || "'"
          || "   AND C.CONSTRAINT_CATALOG = '" || constrcat || "'"
          || " ORDER BY C.ORDINAL_POSITION"
  DECLARE msv_columnlist CURSOR FROM stmt
  LET list = NULL
  FOREACH msv_columnlist INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION

#-------------------------------------------------------------------------------

FUNCTION sql_ext_mys()
  DEFINE colname   VARCHAR(200)
  DEFINE colnull   INTEGER
  DEFINE coldflt   TEXT
  DEFINE cstrname  VARCHAR(200)
  DEFINE cstrsch   VARCHAR(200)
  DEFINE rcstrname VARCHAR(200)
  DEFINE rcstrsch  VARCHAR(200)
  DEFINE rtabname  VARCHAR(200)
  DEFINE rtabsch   VARCHAR(200)
  DEFINE delrule   VARCHAR(50)
  DEFINE updrule   VARCHAR(50)
  DEFINE sqlcond   TEXT
  DEFINE tabnum    INTEGER
  DEFINE typenum, typel2 SMALLINT
  DEFINE typel1    INTEGER
  DEFINE i,x,s,c   INTEGER
  DEFINE curr_tabname VARCHAR(200)
  DEFINE refconst  SMALLINT

  LOCATE sqlcond IN MEMORY
  LOCATE coldflt IN MEMORY

  # Warning:
  # In MySQL (6.0.4), TABLE_CATALOG is always NULL and TABLE_SCHEMA is the DB name
  # => We must use TABLE_SCHEMA = DATABASE(), and no way to filter by dbowner.

  DECLARE smys_cdef CURSOR FROM
             "SELECT"
             || "  C.COLUMN_DEFAULT"
             || " FROM INFORMATION_SCHEMA.COLUMNS C"
             || " WHERE C.TABLE_SCHEMA = DATABASE()"
             || "   AND C.TABLE_NAME = ?"
             || "   AND C.COLUMN_NAME = ?"
  
  DECLARE cmys_pkey CURSOR FROM
             "SELECT"
             || "  C.CONSTRAINT_NAME,"
             || "  U.COLUMN_NAME"
             || " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C,"
             || "      INFORMATION_SCHEMA.KEY_COLUMN_USAGE U"
             || " WHERE C.TABLE_NAME = ? AND C.TABLE_SCHEMA = DATABASE()"
             || "   AND C.CONSTRAINT_TYPE = 'PRIMARY KEY'"
             || "   AND U.TABLE_NAME = C.TABLE_NAME"
             || "   AND U.TABLE_SCHEMA = C.TABLE_SCHEMA"
             || "   AND U.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND U.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
             || " ORDER BY U.ORDINAL_POSITION"

  DECLARE cmys_skey CURSOR FROM
             "SELECT DISTINCT"
             || "  C.CONSTRAINT_NAME, C.CONSTRAINT_SCHEMA"
             || " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C"
             || " WHERE C.TABLE_NAME = ? AND C.TABLE_SCHEMA = DATABASE()"
             || "   AND C.CONSTRAINT_TYPE = 'UNIQUE'"
             || " ORDER BY C.CONSTRAINT_NAME"

  WHENEVER ERROR CONTINUE -- REFERENTIAL_CONSTRAINTS was introduced in 5.1
  DECLARE cmys_fkey CURSOR FROM
             "SELECT"
             || " TC1.CONSTRAINT_NAME, TC1.CONSTRAINT_SCHEMA,"
             || " TC2.CONSTRAINT_NAME, TC2.CONSTRAINT_SCHEMA,"
             || " TC2.TABLE_NAME, TC2.TABLE_SCHEMA,"
             || " C.DELETE_RULE, C.UPDATE_RULE"
             || " FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS C,"
             ||      " INFORMATION_SCHEMA.TABLE_CONSTRAINTS TC1,"
             ||      " INFORMATION_SCHEMA.TABLE_CONSTRAINTS TC2"
             || " WHERE TC1.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
             || "   AND TC1.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND TC2.CONSTRAINT_SCHEMA = C.UNIQUE_CONSTRAINT_SCHEMA"
             || "   AND TC2.CONSTRAINT_NAME = C.UNIQUE_CONSTRAINT_NAME"
             || "   AND TC1.TABLE_NAME = C.TABLE_NAME"
             || "   AND TC2.TABLE_NAME = C.REFERENCED_TABLE_NAME"
             || "   AND TC1.TABLE_NAME = ? AND TC1.TABLE_SCHEMA = DATABASE()"
             || " ORDER BY C.CONSTRAINT_NAME"
  WHENEVER ERROR STOP
  LET refconst = ( SQLCA.SQLCODE==0 )
  IF NOT refconst THEN
     CALL fglt_log_write("WARNING: Failed to select from INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS, no foreign keys will be found.")
     CALL fglt_log_write("")
  END IF

  # CHECK not yet supported in MySQL!
  {
  DECLARE cmys_chk CURSOR FROM
             "SELECT"
             || "  C.CONSTRAINT_NAME,"
             || "  S.CHECK_CLAUSE"
             || " FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS C,"
             || "      INFORMATION_SCHEMA.CHECK_CONSTRAINTS S"
             || " WHERE C.CONSTRAINT_TYPE = 'CHECK'"
             || "   AND S.CONSTRAINT_SCHEMA = C.CONSTRAINT_SCHEMA"
             || "   AND S.CONSTRAINT_NAME = C.CONSTRAINT_NAME"
             || "   AND C.TABLE_NAME = ? AND C.TABLE_SCHEMA = DATABASE()"
             || " ORDER BY C.CONSTRAINT_NAME"
  }

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_mys
        END IF

        LET coldflt = NULL
        EXECUTE smys_cdef USING curr_tabname, coldef.colname INTO coldflt

        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     FOREACH cmys_pkey USING curr_tabname
                       INTO cstrname, colname
        IF tables[tabnum].pkeyname IS NULL THEN
           # In MySQL, the default name of a PRIMARY KEY is always 'PRIMARY'!
           IF cstrname == "PRIMARY" THEN
              LET tables[tabnum].pkeyname = constraint_get_unique_name(constype_pkey,tabnum)
              CALL fglt_log_write(SFMT("WARNING: Primary key name 'PRIMARY' changed to %1 for table %2",
                                       tables[tabnum].pkeyname,tables[tabnum].tabname))
           ELSE
              LET tables[tabnum].pkeyname = casens_name(cstrname)
           END IF
        END IF
        LET x = column_lookup(tabnum,colname)
        LET tables[tabnum].cols[x].pkeycol = 1
     END FOREACH

     FOREACH cmys_skey USING curr_tabname
                       INTO cstrname, cstrsch
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = mys_columnlist(cstrname,cstrsch)
     END FOREACH

  # CHECK not yet supported in MySQL!
  {
     FOREACH cmys_chk USING curr_tabname
        INTO cstrname, sqlcond
        LET sqlcond = sqlcond CLIPPED
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = sqlcond
     END FOREACH
  }

LABEL next_table_mys:
  END WHILE

  -- Must use a separate loop after loading all tables to find replaced PRIMARY key name.
  LET tabnum = 1
  WHILE tabnum <= tables.getLength()

     LET curr_tabname = tables[tabnum].tabname

     IF refconst THEN
     FOREACH cmys_fkey USING curr_tabname
        INTO cstrname,
             cstrsch,
             rcstrname,
             rcstrsch,
             rtabname,
             rtabsch,
             delrule,
             updrule
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = mys_columnlist(cstrname,cstrsch)
        LET tables[tabnum].fkeys[x].reftabname = casens_name(rtabname)
        # In MySQL, the default name of a PRIMARY KEY is always 'PRIMARY'!
        IF rcstrname == "PRIMARY" THEN
            LET i = table_lookup(tables[tabnum].fkeys[x].reftabname)
            IF i < 1 THEN
               CALL fglt_log_write(SFMT("ERROR: Could not find referenced table %1 for table %2",
                                        tables[tabnum].fkeys[x].reftabname, tables[tabnum].tabname))
               CALL tables.deleteElement(tabnum)
               GOTO next_table_mys_2
            END IF
            LET tables[tabnum].fkeys[x].refconstname = tables[i].pkeyname
        ELSE
            LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        END IF
        CASE delrule
          WHEN "CASCADE"      LET tables[tabnum].fkeys[x].delrule = udrule_cascade
          WHEN "RESTRICT"     LET tables[tabnum].fkeys[x].delrule = udrule_restrict
          WHEN "SET DEFAULT"  LET tables[tabnum].fkeys[x].delrule = udrule_setdefault
          WHEN "SET NULL"     LET tables[tabnum].fkeys[x].delrule = udrule_setnull
          WHEN "NO ACTION"    LET tables[tabnum].fkeys[x].delrule = udrule_default
        END CASE
        CASE updrule
          WHEN "CASCADE"      LET tables[tabnum].fkeys[x].updrule = udrule_cascade
          WHEN "RESTRICT"     LET tables[tabnum].fkeys[x].updrule = udrule_restrict
          WHEN "SET DEFAULT"  LET tables[tabnum].fkeys[x].updrule = udrule_setdefault
          WHEN "SET NULL"     LET tables[tabnum].fkeys[x].updrule = udrule_setnull
          WHEN "NO ACTION"    LET tables[tabnum].fkeys[x].updrule = udrule_default
        END CASE
     END FOREACH
     END IF

  LET tabnum = tabnum + 1
LABEL next_table_mys_2:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0
END FUNCTION

FUNCTION mys_columnlist(constrname,constrsch)
  DEFINE constrname VARCHAR(200)
  DEFINE constrsch  VARCHAR(200)
  DEFINE colname VARCHAR(200)
  DEFINE stmt,list STRING
  LET stmt = "SELECT DISTINCT C.COLUMN_NAME"
          || "  FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE C"
          || " WHERE C.CONSTRAINT_NAME = '" || constrname || "'"
          || "   AND C.CONSTRAINT_SCHEMA = '" || constrsch || "'"
          || " ORDER BY C.ORDINAL_POSITION"
  DECLARE mys_columnlist CURSOR FROM stmt
  LET list = NULL
  FOREACH mys_columnlist INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION

#-------------------------------------------------------------------------------

FUNCTION sql_ext_pgs()
  DEFINE tabname   VARCHAR(200)
  DEFINE colname   VARCHAR(200)
  DEFINE colnull   INTEGER
  DEFINE coldflt   VARCHAR(2000)
  DEFINE cstrname  VARCHAR(200)
  DEFINE cstrsch   VARCHAR(200)
  DEFINE cstrcat   VARCHAR(200)
  DEFINE rcstrname VARCHAR(200)
  DEFINE rtabname  VARCHAR(200)
  DEFINE delrule   VARCHAR(50)
  DEFINE updrule   VARCHAR(50)
  DEFINE sqlcond   VARCHAR(65000)
  DEFINE tabnum    INTEGER
  DEFINE tmpstr    STRING
  DEFINE i,x,s,c   INTEGER
  DEFINE typenum, typel1, typel2 SMALLINT
  DEFINE curr_tabname VARCHAR(200)
  DEFINE up_dbowner VARCHAR(200)

  PREPARE spgs_cdef FROM
    "SELECT PG_GET_EXPR(AD.ADBIN,AD.ADRELID)"
    || " FROM PG_NAMESPACE NS, PG_ATTRDEF AD, PG_CLASS C, PG_ATTRIBUTE A"
    || " WHERE NS.OID = C.RELNAMESPACE"
    || "   AND C.OID = A.ATTRELID"
    || "   AND AD.ADRELID = C.OID AND AD.ADNUM = A.ATTNUM"
    || "   AND UPPER(NS.NSPNAME) = ?"
    || "   AND C.RELNAME = ?"
    || "   AND A.ATTNAME = ?"

  DECLARE cpgs_pkey CURSOR FROM
             "SELECT"
             || "  P.CONNAME,"
             || "  A.ATTNAME"
             || " FROM PG_NAMESPACE NS, PG_CONSTRAINT P, PG_ATTRIBUTE A, PG_CLASS C"
             || " WHERE NS.OID = C.RELNAMESPACE"
             || "   AND P.CONTYPE='p'"
             || "   AND P.CONRELID = A.ATTRELID"
             || "   AND A.ATTNUM = ANY ( P.CONKEY )"
             || "   AND P.CONRELID = C.OID"
             || "   AND UPPER(NS.NSPNAME) = ?"
             || "   AND C.RELNAME = ?"
             || " ORDER BY A.ATTNUM"

  DECLARE cpgs_skey CURSOR FROM
             "SELECT"
             || "  S.CONNAME"
             || " FROM PG_NAMESPACE NS, PG_CONSTRAINT S, PG_CLASS C"
             || " WHERE NS.OID = C.RELNAMESPACE"
             || "   AND S.CONTYPE='u'"
             || "   AND S.CONRELID = C.OID"
             || "   AND UPPER(NS.NSPNAME) = ?"
             || "   AND C.RELNAME = ?"
             || " ORDER BY S.CONNAME"


  DECLARE cpgs_fkey CURSOR FROM
             "SELECT"
             || "  F.CONNAME,"
             || "  K.CONNAME,"
             || "  R.RELNAME,"
             || "  F.CONFDELTYPE,"
             || "  F.CONFUPDTYPE"
             || " FROM PG_NAMESPACE NS, PG_CONSTRAINT F, PG_CLASS C, PG_CONSTRAINT K, PG_CLASS R"
             || " WHERE NS.OID = C.RELNAMESPACE"
             || "   AND F.CONTYPE='f'"
             || "   AND F.CONRELID = C.OID"
             || "   AND F.CONFRELID = R.OID"
             || "   AND F.CONFKEY = K.CONKEY"
             || "   AND F.CONFRELID = K.CONRELID"
             || "   AND UPPER(NS.NSPNAME) = ?"
             || "   AND C.RELNAME = ?"
             || " ORDER BY F.CONNAME"

  DECLARE cpgs_chk CURSOR FROM
             "SELECT"
             || "  K.CONNAME,"
             || "  pg_get_constraintdef(K.OID)"
             || " FROM PG_NAMESPACE NS, PG_CONSTRAINT K, PG_CLASS C"
             || " WHERE NS.OID = C.RELNAMESPACE"
             || "   AND K.CONTYPE='c'"
             || "   AND K.CONRELID = C.OID"
             || "   AND UPPER(NS.NSPNAME) = ?"
             || "   AND C.RELNAME = ?"
             || " ORDER BY K.CONNAME"

  LET up_dbowner = UPSHIFT(ext_params.dbowner)

  WHILE TRUE

     IF int_flag THEN
        CALL tables.clear()
        EXIT WHILE
     END IF

     LET s = fgldbsch_next_table()
     IF s <= 0 THEN EXIT WHILE END IF

     CALL tables.appendElement()
     LET tabnum = tables.getLength()
     LET curr_tabname = fgldbsch_get_table_name()
     LET tables[tabnum].tabname = casens_name(fgldbsch_get_table_name())
     LET tables[tabnum].owner = ext_params.dbowner CLIPPED
     CALL fglt_log_write(SFMT("Table: %1", fgldbsch_get_table_name()))

     FOR c=1 TO fgldbsch_get_column_count()
        CALL fgldbsch_get_column_info(c) RETURNING coldef.*
        IF coldef.ifxtype = -1 THEN
           LET typenum = -1
        ELSE
           CALL convert_datatype_ifx(coldef.ifxtype, coldef.ifxleng)
                RETURNING typenum, typel1, typel2, colnull
        END IF
        IF typenum == -1 THEN
           CALL fglt_log_write(SFMT("ERROR: Column %1.%2 has unsupported datatype %3.",coldef.tabname,coldef.colname,coldef.ifxtype))
           CALL fglt_log_write("     The whole table is ignored.")
           CALL tables.deleteElement(tabnum)
           GOTO next_table_pgs
        END IF

        LET coldflt = NULL
        EXECUTE spgs_cdef USING up_dbowner, curr_tabname, coldef.colname INTO coldflt
        IF coldflt MATCHES "nextval*" THEN
           LET coldflt = NULL
        END IF
        IF coldflt IS NOT NULL THEN
           LET tmpstr = coldflt
           LET i = tmpstr.getIndexOf("::",1)
           IF i>0 THEN
              LET coldflt = tmpstr.subString(1,i-1)
           END IF
        END IF

        CALL tables[tabnum].cols.appendElement()
        LET x = tables[tabnum].cols.getLength()
        LET tables[tabnum].cols[x].colname  = casens_name(coldef.colname)
        LET tables[tabnum].cols[x].pkeycol  = 0
        LET tables[tabnum].cols[x].defvalue = coldflt CLIPPED
        LET tables[tabnum].cols[x].typenum  = typenum
        LET tables[tabnum].cols[x].typel1   = typel1
        LET tables[tabnum].cols[x].typel2   = typel2
        LET tables[tabnum].cols[x].typenn   = colnull
     END FOR

     FOREACH cpgs_pkey USING up_dbowner, curr_tabname
                       INTO cstrname, colname
        IF tables[tabnum].pkeyname IS NULL THEN
           LET tables[tabnum].pkeyname = casens_name(cstrname)
        END IF
        LET x = column_lookup(tabnum,colname)
        LET tables[tabnum].cols[x].pkeycol = 1
     END FOREACH

     FOREACH cpgs_skey USING up_dbowner, curr_tabname
                       INTO cstrname, cstrsch, cstrcat
        CALL tables[tabnum].skeys.appendElement()
        LET x = tables[tabnum].skeys.getLength()
        LET tables[tabnum].skeys[x].skeyname = casens_name(cstrname)
        LET tables[tabnum].skeys[x].skeycols = pgs_columnlist(cstrname)
     END FOREACH

     FOREACH cpgs_fkey USING up_dbowner, curr_tabname
        INTO cstrname,
             rcstrname,
             rtabname,
             delrule,
             updrule
        CALL tables[tabnum].fkeys.appendElement()
        LET x = tables[tabnum].fkeys.getLength()
        LET tables[tabnum].fkeys[x].fkeyname = casens_name(cstrname)
        LET tables[tabnum].fkeys[x].fkeycols = pgs_columnlist(cstrname)
        LET tables[tabnum].fkeys[x].reftabname = casens_name(tabname)
        LET tables[tabnum].fkeys[x].refconstname = casens_name(rcstrname)
        CASE delrule
          WHEN "c"  LET tables[tabnum].fkeys[x].delrule = udrule_cascade
          WHEN "r"  LET tables[tabnum].fkeys[x].delrule = udrule_restrict
          WHEN "d"  LET tables[tabnum].fkeys[x].delrule = udrule_setdefault
          WHEN "n"  LET tables[tabnum].fkeys[x].delrule = udrule_setnull
          OTHERWISE LET tables[tabnum].fkeys[x].delrule = udrule_default
        END CASE
        CASE updrule
          WHEN "c"  LET tables[tabnum].fkeys[x].updrule = udrule_cascade
          WHEN "r"  LET tables[tabnum].fkeys[x].updrule = udrule_restrict
          WHEN "d"  LET tables[tabnum].fkeys[x].updrule = udrule_setdefault
          WHEN "n"  LET tables[tabnum].fkeys[x].updrule = udrule_setnull
          OTHERWISE LET tables[tabnum].fkeys[x].updrule = udrule_default
        END CASE
     END FOREACH

     FOREACH cpgs_chk USING up_dbowner, curr_tabname
        INTO cstrname, sqlcond
        LET sqlcond = sqlcond CLIPPED
        CALL tables[tabnum].checks.appendElement()
        LET x = tables[tabnum].checks.getLength()
        LET tables[tabnum].checks[x].checkname = casens_name(cstrname)
        LET tables[tabnum].checks[x].sqlcond = sqlcond
     END FOREACH

LABEL next_table_pgs:
  END WHILE

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN -3
  END IF

  RETURN 0
END FUNCTION

FUNCTION pgs_columnlist(constrname)
  DEFINE constrname VARCHAR(200)
  DEFINE colname VARCHAR(200)
  DEFINE list STRING
  DECLARE pgs_columnlist CURSOR
        FROM "SELECT A.ATTNAME"
          || " FROM PG_CONSTRAINT C, PG_ATTRIBUTE A"
          || " WHERE C.CONNAME = ?"
          || "   AND C.CONRELID = A.ATTRELID"
          || "   AND A.ATTNUM = ANY ( C.CONKEY )"
  LET list = NULL
  FOREACH pgs_columnlist USING constrname INTO colname
     IF list IS NOT NULL THEN LET list = list, "," END IF
     LET list = list, casens_name(colname)
  END FOREACH
  RETURN list
END FUNCTION


#-------------------------------------------------------------------------------

FUNCTION sql_get_db_type()
  DEFINE dbt CHAR(3)
  INITIALIZE dbt TO NULL
  IF dbt IS NULL THEN
     IF try_select_user("SELECT user FROM user_users WHERE username=user") THEN
       LET dbt = "ORA"
     END IF
  END IF
  IF dbt IS NULL THEN
     IF try_select_user("SELECT DISTINCT(@@SERVERNAME) FROM INFORMATION_SCHEMA.TABLES") THEN
       LET dbt = "MSV"
     END IF
  END IF
  IF dbt IS NULL THEN
     IF try_select_user("SELECT USER FROM sysibm.systables WHERE name='SYSTABLES'") THEN
       LET dbt = "DB2"
     END IF
  END IF
  IF dbt IS NULL THEN
     IF try_select_user("SELECT USER FROM informix.systables WHERE tabid=1") THEN
       LET dbt = "IFX"
     END IF
  END IF
  IF dbt IS NULL THEN
     IF try_select_user("SELECT USER FROM pg_type WHERE typname = 'bool'") THEN
       LET dbt = "PGS"
     END IF
  END IF
  IF dbt IS NULL THEN
     IF try_select_user("SELECT CURDATE()") THEN
       LET dbt = "MYS"
     END IF
  END IF
  IF dbt IS NULL THEN
     IF try_select_user("SELECT USER FROM SYSTEM.TABLE_OF_TABLES WHERE TABLE_NAME = 'TABLE_OF_TABLES'") THEN
       LET dbt = "ADS"
     END IF
  END IF
  RETURN dbt
END FUNCTION

FUNCTION try_select_user(stmt)
  DEFINE stmt STRING
  DEFINE uname CHAR(50)
  WHENEVER ERROR CONTINUE
  DECLARE sTSU CURSOR FROM stmt
  IF sqlca.sqlcode!=0 THEN RETURN FALSE END IF
  OPEN sTSU
  IF sqlca.sqlcode!=0 THEN RETURN FALSE END IF
  FETCH sTSU INTO uname
  IF sqlca.sqlcode!=0 THEN RETURN FALSE END IF
  CLOSE sTSU
  IF sqlca.sqlcode!=0 THEN RETURN FALSE END IF
  FREE sTSU
  IF sqlca.sqlcode!=0 THEN RETURN FALSE END IF
  WHENEVER ERROR STOP
  RETURN TRUE
END FUNCTION

FUNCTION sql_constraint(name)
  DEFINE name STRING
  IF name IS NOT NULL AND sql_params.namedconst==1 THEN
     RETURN " CONSTRAINT " || sql_identifier(name)
  END IF
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_qual1(p)
  DEFINE p INTEGER
  IF p IS NOT NULL THEN
     RETURN "("||p||")"
  END IF
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_qual2(p,s)
  DEFINE p,s INTEGER
  IF p IS NOT NULL THEN
     IF s IS NOT NULL THEN
        RETURN "("||p||","||s||")"
     ELSE
        RETURN "("||p||")"
     END IF
  END IF
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type_ads(t,c)
  DEFINE t,c INTEGER
  -- Genero db MUST be Informix compatible.
  -- While writing these lines, Genero DB (3.81) is still missing MONEY(p,s) and DECIMAL(p).
  RETURN sql_serialize_type_ifx(t,c)
END FUNCTION

FUNCTION sql_serialize_type_ifx(t,c)
  DEFINE t,c INTEGER, tmp STRING
  CASE tables[t].cols[c].typenum
     WHEN dtn_char
          RETURN sfmt("CHAR%1",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_decimal
          RETURN sfmt("DECIMAL%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_float
          RETURN sfmt("FLOAT%1",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_money
          RETURN sfmt("MONEY%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_boolean
          RETURN "BOOLEAN"
     WHEN dtn_smallint
          RETURN "SMALLINT"
     WHEN dtn_integer
          RETURN "INTEGER"
     WHEN dtn_bigint
          RETURN "BIGINT"
     WHEN dtn_int8
          RETURN "INT8"
     WHEN dtn_serial
          RETURN "SERIAL"
     WHEN dtn_bigserial
          RETURN "BIGSERIAL"
     WHEN dtn_serial8
          RETURN "SERIAL8"
     WHEN dtn_varchar
          RETURN sfmt("VARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nchar
          RETURN sfmt("NCHAR%1",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_nvarchar
          RETURN sfmt("NVARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_varchar2
          IF tables[t].cols[c].typel1 <= 255 THEN
             LET tmp = "VARCHAR%1"
          ELSE
             LET tmp = "LVARCHAR%1"
          END IF
          RETURN sfmt(tmp,sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nvarchar2
          RETURN sfmt("NVARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_interval_year_to_month
          RETURN sfmt("INTERVAL YEAR%1 TO MONTH",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_year_to_year
          RETURN sfmt("INTERVAL YEAR%1 TO YEAR",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_month_to_month
          RETURN sfmt("INTERVAL MONTH%1 TO MONTH",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_frac5
          RETURN sfmt("INTERVAL DAY%1 TO FRACTION(5)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_frac4
          RETURN sfmt("INTERVAL DAY%1 TO FRACTION(4)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_frac3
          RETURN sfmt("INTERVAL DAY%1 TO FRACTION(3)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_frac2
          RETURN sfmt("INTERVAL DAY%1 TO FRACTION(2)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_frac1
          RETURN sfmt("INTERVAL DAY%1 TO FRACTION(1)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_second
          RETURN sfmt("INTERVAL DAY%1 TO SECOND",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_minute
          RETURN sfmt("INTERVAL DAY%1 TO MINUTE",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_hour
          RETURN sfmt("INTERVAL DAY%1 TO HOUR",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_day_to_day
          RETURN sfmt("INTERVAL DAY%1 TO DAY",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_frac5
          RETURN sfmt("INTERVAL HOUR%1 TO FRACTION(5)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_frac4
          RETURN sfmt("INTERVAL HOUR%1 TO FRACTION(4)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_frac3
          RETURN sfmt("INTERVAL HOUR%1 TO FRACTION(3)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_frac2
          RETURN sfmt("INTERVAL HOUR%1 TO FRACTION(2)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_frac1
          RETURN sfmt("INTERVAL HOUR%1 TO FRACTION(1)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_second
          RETURN sfmt("INTERVAL HOUR%1 TO SECOND",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_minute
          RETURN sfmt("INTERVAL HOUR%1 TO MINUTE",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_hour_to_hour
          RETURN sfmt("INTERVAL HOUR%1 TO HOUR",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_frac5
          RETURN sfmt("INTERVAL MINUTE%1 TO FRACTION(5)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_frac4
          RETURN sfmt("INTERVAL MINUTE%1 TO FRACTION(4)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_frac3
          RETURN sfmt("INTERVAL MINUTE%1 TO FRACTION(3)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_frac2
          RETURN sfmt("INTERVAL MINUTE%1 TO FRACTION(2)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_frac1
          RETURN sfmt("INTERVAL MINUTE%1 TO FRACTION(1)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_second
          RETURN sfmt("INTERVAL MINUTE%1 TO SECOND",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_minute_to_minute
          RETURN sfmt("INTERVAL MINUTE%1 TO MINUTE",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_second_to_frac5
          RETURN sfmt("INTERVAL SECOND%1 TO FRACTION(5)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_second_to_frac4
          RETURN sfmt("INTERVAL SECOND%1 TO FRACTION(4)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_second_to_frac3
          RETURN sfmt("INTERVAL SECOND%1 TO FRACTION(3)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_second_to_frac2
          RETURN sfmt("INTERVAL SECOND%1 TO FRACTION(2)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_second_to_frac1
          RETURN sfmt("INTERVAL SECOND%1 TO FRACTION(1)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_second_to_second
          RETURN sfmt("INTERVAL SECOND%1 TO SECOND",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_fraction_to_frac5
          RETURN sfmt("INTERVAL FRACTION%1 TO FRACTION(5)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_fraction_to_frac4
          RETURN sfmt("INTERVAL FRACTION%1 TO FRACTION(4)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_fraction_to_frac3
          RETURN sfmt("INTERVAL FRACTION%1 TO FRACTION(3)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_fraction_to_frac2
          RETURN sfmt("INTERVAL FRACTION%1 TO FRACTION(2)",sql_serialize_qual1(tables[t].cols[c].typel1))
     WHEN dtn_interval_fraction_to_frac1
          RETURN sfmt("INTERVAL FRACTION%1 TO FRACTION(1)",sql_serialize_qual1(tables[t].cols[c].typel1))
     OTHERWISE
          RETURN dtdef[dtdef_lookup_by_num(tables[t].cols[c].typenum)].name
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type_ora(t,c)
  DEFINE t,c INTEGER
  CASE tables[t].cols[c].typenum
     WHEN dtn_char
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_boolean
          RETURN "CHAR(1)"
     WHEN dtn_date
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_decimal
          IF tables[t].cols[c].typel1 IS NOT NULL THEN
             IF tables[t].cols[c].typel2 IS NOT NULL THEN
                RETURN sfmt("NUMBER(%1,%2)", tables[t].cols[c].typel1, tables[t].cols[c].typel2)
             ELSE
                RETURN sfmt("FLOAT(%1)", tables[t].cols[c].typel1)
             END IF
          ELSE
             RETURN "FLOAT"
          END IF
     WHEN dtn_float
          RETURN "NUMBER"
     WHEN dtn_money
          IF tables[t].cols[c].typel1 IS NOT NULL THEN
             IF tables[t].cols[c].typel2 IS NOT NULL THEN
                RETURN sfmt("NUMBER(%1,%2)",tables[t].cols[c].typel1,tables[t].cols[c].typel2)
             ELSE
                RETURN sfmt("NUMBER(%1,2)",tables[t].cols[c].typel1)
             END IF
          ELSE
             RETURN "NUMBER(16,2)"
          END IF
     WHEN dtn_smallint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_integer
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_bigint
          RETURN "NUMBER(20,0)"
     WHEN dtn_int8
          RETURN "NUMBER(20,0)"
     WHEN dtn_smallfloat
          RETURN "NUMBER"
     WHEN dtn_serial -- FIXME: Generate trigger+sequence?
          RETURN "NUMBER(10,0)"
     WHEN dtn_bigserial -- FIXME: Generate trigger+sequence?
          RETURN "NUMBER(20,0)"
     WHEN dtn_serial8 -- FIXME: Generate trigger+sequence?
          RETURN "NUMBER(20,0)"
     WHEN dtn_text
          RETURN "CLOB"
     WHEN dtn_byte
          RETURN "BLOB"
     WHEN dtn_varchar
          RETURN sfmt("VARCHAR2%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nvarchar
          RETURN sfmt("NVARCHAR2%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_varchar2
          RETURN sfmt("VARCHAR2%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     --
     WHEN dtn_datetime_year_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_year_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_year_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_year_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_year_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_month_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_month_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_month_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_month_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_month_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_day_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_day_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_day_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_day_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_day_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_hour_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_hour_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_hour_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_hour_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_hour_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_minute_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_minute_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_minute_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_minute_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_minute_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_second_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_second_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_second_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_second_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_second_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_fraction_to_frac5
          RETURN "TIMESTAMP(5)"
     WHEN dtn_datetime_fraction_to_frac4
          RETURN "TIMESTAMP(4)"
     WHEN dtn_datetime_fraction_to_frac3
          RETURN "TIMESTAMP(3)"
     WHEN dtn_datetime_fraction_to_frac2
          RETURN "TIMESTAMP(2)"
     WHEN dtn_datetime_fraction_to_frac1
          RETURN "TIMESTAMP(1)"
     --
     WHEN dtn_datetime_year_to_second
          RETURN "DATE"
     WHEN dtn_datetime_year_to_minute
          RETURN "DATE"
     WHEN dtn_datetime_year_to_hour
          RETURN "DATE"
     WHEN dtn_datetime_year_to_day
          RETURN "DATE"
     WHEN dtn_datetime_year_to_month
          RETURN "DATE"
     WHEN dtn_datetime_year_to_year
          RETURN "DATE"
     --
     WHEN dtn_datetime_month_to_second
          RETURN "DATE"
     WHEN dtn_datetime_month_to_minute
          RETURN "DATE"
     WHEN dtn_datetime_month_to_hour
          RETURN "DATE"
     WHEN dtn_datetime_month_to_day
          RETURN "DATE"
     WHEN dtn_datetime_month_to_month
          RETURN "DATE"
     --
     WHEN dtn_datetime_day_to_second
          RETURN "DATE"
     WHEN dtn_datetime_day_to_minute
          RETURN "DATE"
     WHEN dtn_datetime_day_to_hour
          RETURN "DATE"
     WHEN dtn_datetime_day_to_day
          RETURN "DATE"
     --
     WHEN dtn_datetime_hour_to_second
          RETURN "DATE"
     WHEN dtn_datetime_hour_to_minute
          RETURN "DATE"
     WHEN dtn_datetime_hour_to_hour
          RETURN "DATE"
     --
     WHEN dtn_datetime_minute_to_second
          RETURN "DATE"
     WHEN dtn_datetime_minute_to_minute
          RETURN "DATE"
     --
     WHEN dtn_datetime_second_to_second
          RETURN "DATE"
     --
     WHEN dtn_interval_year_to_month
          RETURN sfmt("INTERVAL YEAR(%1) TO MONTH",tables[t].cols[c].typel1)
     WHEN dtn_interval_day_to_frac5
          RETURN sfmt("INTERVAL DAY(%1) TO SECOND(5)",tables[t].cols[c].typel1)
     WHEN dtn_interval_day_to_frac4
          RETURN sfmt("INTERVAL DAY(%1) TO SECOND(4)",tables[t].cols[c].typel1)
     WHEN dtn_interval_day_to_frac3
          RETURN sfmt("INTERVAL DAY(%1) TO SECOND(3)",tables[t].cols[c].typel1)
     WHEN dtn_interval_day_to_frac2
          RETURN sfmt("INTERVAL DAY(%1) TO SECOND(2)",tables[t].cols[c].typel1)
     WHEN dtn_interval_day_to_frac1
          RETURN sfmt("INTERVAL DAY(%1) TO SECOND(1)",tables[t].cols[c].typel1)
     WHEN dtn_interval_day_to_second
          RETURN sfmt("INTERVAL DAY(%1) TO SECOND(0)",tables[t].cols[c].typel1)
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type_db2(t,c)
  DEFINE t,c INTEGER
  CASE tables[t].cols[c].typenum
     WHEN dtn_char
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_boolean
          RETURN "CHAR(1)"
     WHEN dtn_date
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_decimal
          IF tables[t].cols[c].typel2 IS NOT NULL THEN
             RETURN sql_serialize_type_ifx(t,c)
          ELSE
             IF tables[t].cols[c].typel1 <= 16 THEN
                RETURN "DECFLOAT(16)"
             ELSE
                RETURN "DECFLOAT(34)"
             END IF
          END IF
     WHEN dtn_float
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_money
          IF tables[t].cols[c].typel1 IS NOT NULL THEN
             IF tables[t].cols[c].typel2 IS NOT NULL THEN
                RETURN sfmt("DECIMAL(%1,%2)",tables[t].cols[c].typel1,tables[t].cols[c].typel2)
             ELSE
                RETURN sfmt("DECIMAL(%1,2)",tables[t].cols[c].typel1)
             END IF
          ELSE
             RETURN "DECIMAL(16,2)"
          END IF
     WHEN dtn_smallint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_integer
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_bigint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_int8
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_smallfloat
          RETURN "REAL"
     WHEN dtn_serial
          RETURN "INTEGER GENERATED ALWAYS AS IDENTITY(START WITH 1,INCREMENT BY 1)"
     WHEN dtn_bigserial
          RETURN "BIGINT GENERATED ALWAYS AS IDENTITY(START WITH 1,INCREMENT BY 1)"
     WHEN dtn_serial8
          RETURN "BIGINT GENERATED ALWAYS AS IDENTITY(START WITH 1,INCREMENT BY 1)"
     WHEN dtn_text
          RETURN "CLOB(500K)"
     WHEN dtn_byte
          RETURN "BLOB(500K)"
     WHEN dtn_varchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nvarchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_varchar2
          RETURN sfmt("VARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nvarchar2
          RETURN sfmt("NVARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     --
     WHEN dtn_datetime_year_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_frac1
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_second
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_hour
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_day
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_month
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_year_to_year
          RETURN "TIMESTAMP"
     --
     WHEN dtn_datetime_month_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_frac1
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_second
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_minute
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_second
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_hour
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_day
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_month_to_month
          RETURN "TIMESTAMP"
     --
     WHEN dtn_datetime_day_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_frac1
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_second
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_minute
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_hour
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_day_to_day
          RETURN "TIMESTAMP"
     --
     WHEN dtn_datetime_hour_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_hour_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_hour_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_hour_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_hour_to_frac1
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_hour_to_second
          RETURN "TIME" -- !!!
     WHEN dtn_datetime_hour_to_minute
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_hour_to_hour
          RETURN "TIMESTAMP"
     --
     WHEN dtn_datetime_minute_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_minute_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_minute_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_minute_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_minute_to_frac1
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_minute_to_second
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_minute_to_minute
          RETURN "TIMESTAMP"
     --
     WHEN dtn_datetime_second_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_second_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_second_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_second_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_second_to_frac1
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_second_to_second
          RETURN "TIMESTAMP"
     --
     WHEN dtn_datetime_fraction_to_frac5
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_fraction_to_frac4
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_fraction_to_frac3
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_fraction_to_frac2
          RETURN "TIMESTAMP"
     WHEN dtn_datetime_fraction_to_frac1
          RETURN "TIMESTAMP"
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type_msv(t,c)
  DEFINE t,c INTEGER
  CASE tables[t].cols[c].typenum
     WHEN dtn_char
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_boolean
          RETURN "BIT"
     WHEN dtn_date
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_decimal
          IF tables[t].cols[c].typel2 IS NOT NULL THEN
             RETURN sql_serialize_type_ifx(t,c)
          ELSE
             RETURN NULL
          END IF
     WHEN dtn_float
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_money
          IF tables[t].cols[c].typel1 IS NOT NULL THEN
             IF tables[t].cols[c].typel2 IS NOT NULL THEN
                RETURN sfmt("DECIMAL(%1,%2)",tables[t].cols[c].typel1,tables[t].cols[c].typel2)
             ELSE
                RETURN sfmt("DECIMAL(%1,2)",tables[t].cols[c].typel1)
             END IF
          ELSE
             RETURN "DECIMAL(16,2)"
          END IF
     WHEN dtn_smallint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_integer
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_bigint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_int8
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_smallfloat
          RETURN "REAL"
     WHEN dtn_serial
          RETURN "INTEGER IDENTITY(1,1)"
     WHEN dtn_serial8
          RETURN "BIGINT IDENTITY(1,1)"
     WHEN dtn_bigserial
          RETURN "BIGINT IDENTITY(1,1)"
     WHEN dtn_varchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nvarchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_varchar2
          RETURN sfmt("VARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nvarchar2
          RETURN sfmt("NVARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     --
     WHEN dtn_text
          RETURN "VARCHAR(MAX)"
     WHEN dtn_byte
          RETURN "VARBINARY(MAX)"
     --
     WHEN dtn_datetime_hour_to_frac5
          RETURN "TIME(5)"
     WHEN dtn_datetime_hour_to_frac4
          RETURN "TIME(4)"
     WHEN dtn_datetime_hour_to_frac3
          RETURN "TIME(3)"
     WHEN dtn_datetime_hour_to_frac2
          RETURN "TIME(2)"
     WHEN dtn_datetime_hour_to_frac1
          RETURN "TIME(1)"
     WHEN dtn_datetime_hour_to_second
          RETURN "TIME(0)"
     --
     WHEN dtn_datetime_year_to_frac5
          RETURN "DATETIME2(5)"
     WHEN dtn_datetime_year_to_frac4
          RETURN "DATETIME2(4)"
     WHEN dtn_datetime_year_to_frac3
          RETURN "DATETIME2(3)"
     WHEN dtn_datetime_year_to_frac2
          RETURN "DATETIME2(2)"
     WHEN dtn_datetime_year_to_frac1
          RETURN "DATETIME2(1)"
     WHEN dtn_datetime_year_to_second
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_year_to_minute
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_year_to_hour
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_year_to_day
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_year_to_month
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_year_to_year
          RETURN "DATETIME2(0)"
     --
     WHEN dtn_datetime_month_to_frac5
          RETURN "DATETIME2(5)"
     WHEN dtn_datetime_month_to_frac4
          RETURN "DATETIME2(4)"
     WHEN dtn_datetime_month_to_frac3
          RETURN "DATETIME2(3)"
     WHEN dtn_datetime_month_to_frac2
          RETURN "DATETIME2(2)"
     WHEN dtn_datetime_month_to_frac1
          RETURN "DATETIME2(1)"
     WHEN dtn_datetime_month_to_second
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_month_to_minute
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_month_to_hour
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_month_to_day
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_month_to_month
          RETURN "DATETIME2(0)"
     --
     WHEN dtn_datetime_day_to_frac5
          RETURN "DATETIME2(5)"
     WHEN dtn_datetime_day_to_frac4
          RETURN "DATETIME2(4)"
     WHEN dtn_datetime_day_to_frac3
          RETURN "DATETIME2(3)"
     WHEN dtn_datetime_day_to_frac2
          RETURN "DATETIME2(2)"
     WHEN dtn_datetime_day_to_frac1
          RETURN "DATETIME2(1)"
     WHEN dtn_datetime_day_to_second
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_day_to_minute
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_day_to_hour
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_day_to_day
          RETURN "DATETIME2(0)"
     --
     WHEN dtn_datetime_minute_to_frac5
          RETURN "DATETIME2(5)"
     WHEN dtn_datetime_minute_to_frac4
          RETURN "DATETIME2(4)"
     WHEN dtn_datetime_minute_to_frac3
          RETURN "DATETIME2(3)"
     WHEN dtn_datetime_minute_to_frac2
          RETURN "DATETIME2(2)"
     WHEN dtn_datetime_minute_to_frac1
          RETURN "DATETIME2(1)"
     WHEN dtn_datetime_minute_to_second
          RETURN "DATETIME2(0)"
     WHEN dtn_datetime_minute_to_minute
          RETURN "DATETIME2(0)"
     --
     WHEN dtn_datetime_second_to_frac5
          RETURN "DATETIME2(5)"
     WHEN dtn_datetime_second_to_frac4
          RETURN "DATETIME2(4)"
     WHEN dtn_datetime_second_to_frac3
          RETURN "DATETIME2(3)"
     WHEN dtn_datetime_second_to_frac2
          RETURN "DATETIME2(2)"
     WHEN dtn_datetime_second_to_frac1
          RETURN "DATETIME2(1)"
     WHEN dtn_datetime_second_to_second
          RETURN "DATETIME2(0)"
     --
     WHEN dtn_datetime_fraction_to_frac5
          RETURN "DATETIME2(5)"
     WHEN dtn_datetime_fraction_to_frac4
          RETURN "DATETIME2(4)"
     WHEN dtn_datetime_fraction_to_frac3
          RETURN "DATETIME2(3)"
     WHEN dtn_datetime_fraction_to_frac2
          RETURN "DATETIME2(2)"
     WHEN dtn_datetime_fraction_to_frac1
          RETURN "DATETIME2(1)"
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type_pgs(t,c)
  DEFINE t,c INTEGER
  CASE tables[t].cols[c].typenum
     WHEN dtn_char
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_boolean
          RETURN "BOOLEAN"
     WHEN dtn_date
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_decimal
          IF tables[t].cols[c].typel2 IS NOT NULL THEN
             RETURN sql_serialize_type_ifx(t,c)
          ELSE
             RETURN NULL
          END IF
     WHEN dtn_float
          RETURN "FLOAT8"
     WHEN dtn_money
          IF tables[t].cols[c].typel1 IS NOT NULL THEN
             IF tables[t].cols[c].typel2 IS NOT NULL THEN
                RETURN sfmt("DECIMAL(%1,%2)",tables[t].cols[c].typel1,tables[t].cols[c].typel2)
             ELSE
                RETURN sfmt("DECIMAL(%1,2)",tables[t].cols[c].typel1)
             END IF
          ELSE
             RETURN "DECIMAL(16,2)"
          END IF
     WHEN dtn_smallint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_integer
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_bigint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_int8
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_smallfloat
          RETURN "FLOAT4"
     WHEN dtn_serial
          RETURN "SERIAL"
     WHEN dtn_bigserial
          RETURN "BIGSERIAL"
     WHEN dtn_serial8
          RETURN "BIGSERIAL"
     WHEN dtn_varchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nvarchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_varchar2
          RETURN sfmt("VARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nvarchar2
          RETURN sfmt("NVARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     --
     WHEN dtn_text
          RETURN "TEXT"
     WHEN dtn_byte
          RETURN "BYTE"
     --
     WHEN dtn_datetime_year_to_frac5
          RETURN "TIMESTAMP(5) WITHOUT TIME ZONE"
     WHEN dtn_datetime_year_to_frac4
          RETURN "TIMESTAMP(4) WITHOUT TIME ZONE"
     WHEN dtn_datetime_year_to_frac3
          RETURN "TIMESTAMP(3) WITHOUT TIME ZONE"
     WHEN dtn_datetime_year_to_frac2
          RETURN "TIMESTAMP(2) WITHOUT TIME ZONE"
     WHEN dtn_datetime_year_to_frac1
          RETURN "TIMESTAMP(1) WITHOUT TIME ZONE"
     WHEN dtn_datetime_year_to_second
          RETURN "TIMESTAMP(0) WITHOUT TIME ZONE"
     --
     WHEN dtn_datetime_hour_to_second
          RETURN "TIME(0) WITHOUT TIME ZONE"
     --
     WHEN dtn_interval_year_to_month
          RETURN "INTERVAL YEAR TO MONTH"
     WHEN dtn_interval_year_to_year
          RETURN "INTERVAL YEAR"
     WHEN dtn_interval_month_to_month
          RETURN "INTERVAL MONTH"
     --
     WHEN dtn_interval_day_to_frac5
          RETURN "INTERVAL DAY TO SECOND(5)"
     WHEN dtn_interval_day_to_frac4
          RETURN "INTERVAL DAY TO SECOND(4)"
     WHEN dtn_interval_day_to_frac3
          RETURN "INTERVAL DAY TO SECOND(3)"
     WHEN dtn_interval_day_to_frac2
          RETURN "INTERVAL DAY TO SECOND(2)"
     WHEN dtn_interval_day_to_frac1
          RETURN "INTERVAL DAY TO SECOND(1)"
     WHEN dtn_interval_day_to_second
          RETURN "INTERVAL DAY TO SECOND(0)"
     WHEN dtn_interval_day_to_minute
          RETURN "INTERVAL DAY TO MINUTE"
     WHEN dtn_interval_day_to_hour
          RETURN "INTERVAL DAY TO HOUR"
     WHEN dtn_interval_day_to_day
          RETURN "INTERVAL DAY"
     --
     WHEN dtn_interval_hour_to_frac5
          RETURN "INTERVAL HOUR TO SECOND(5)"
     WHEN dtn_interval_hour_to_frac4
          RETURN "INTERVAL HOUR TO SECOND(4)"
     WHEN dtn_interval_hour_to_frac3
          RETURN "INTERVAL HOUR TO SECOND(3)"
     WHEN dtn_interval_hour_to_frac2
          RETURN "INTERVAL HOUR TO SECOND(2)"
     WHEN dtn_interval_hour_to_frac1
          RETURN "INTERVAL HOUR TO SECOND(1)"
     WHEN dtn_interval_hour_to_second
          RETURN "INTERVAL HOUR TO SECOND(0)"
     WHEN dtn_interval_hour_to_minute
          RETURN "INTERVAL HOUR TO MINUTE"
     WHEN dtn_interval_hour_to_hour
          RETURN "INTERVAL HOUR"
     --
     WHEN dtn_interval_minute_to_frac5
          RETURN "INTERVAL MINUTE TO SECOND(5)"
     WHEN dtn_interval_minute_to_frac4
          RETURN "INTERVAL MINUTE TO SECOND(4)"
     WHEN dtn_interval_minute_to_frac3
          RETURN "INTERVAL MINUTE TO SECOND(3)"
     WHEN dtn_interval_minute_to_frac2
          RETURN "INTERVAL MINUTE TO SECOND(2)"
     WHEN dtn_interval_minute_to_frac1
          RETURN "INTERVAL MINUTE TO SECOND(1)"
     WHEN dtn_interval_minute_to_second
          RETURN "INTERVAL MINUTE TO SECOND(0)"
     WHEN dtn_interval_minute_to_minute
          RETURN "INTERVAL MINUTE"
     --
     WHEN dtn_interval_second_to_frac5
          RETURN "INTERVAL SECOND(5)"
     WHEN dtn_interval_second_to_frac4
          RETURN "INTERVAL SECOND(4)"
     WHEN dtn_interval_second_to_frac3
          RETURN "INTERVAL SECOND(3)"
     WHEN dtn_interval_second_to_frac2
          RETURN "INTERVAL SECOND(2)"
     WHEN dtn_interval_second_to_frac1
          RETURN "INTERVAL SECOND(1)"
     WHEN dtn_interval_second_to_second
          RETURN "INTERVAL SECOND(0)"
     --
     WHEN dtn_interval_fraction_to_frac5
          RETURN "INTERVAL SECOND(5)"
     WHEN dtn_interval_fraction_to_frac4
          RETURN "INTERVAL SECOND(4)"
     WHEN dtn_interval_fraction_to_frac3
          RETURN "INTERVAL SECOND(3)"
     WHEN dtn_interval_fraction_to_frac2
          RETURN "INTERVAL SECOND(2)"
     WHEN dtn_interval_fraction_to_frac1
          RETURN "INTERVAL SECOND(1)"

  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type_mys(t,c)
  DEFINE t,c INTEGER
  DEFINE tmp STRING
  CASE tables[t].cols[c].typenum
     WHEN dtn_char
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_boolean
          RETURN "BOOLEAN"
     WHEN dtn_date
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_decimal
          IF tables[t].cols[c].typel2 IS NOT NULL THEN
             RETURN sql_serialize_type_ifx(t,c)
          ELSE
             RETURN NULL
          END IF
     WHEN dtn_float
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_money
          IF tables[t].cols[c].typel1 IS NOT NULL THEN
             IF tables[t].cols[c].typel2 IS NOT NULL THEN
                RETURN sfmt("DECIMAL(%1,%2)",tables[t].cols[c].typel1,tables[t].cols[c].typel2)
             ELSE
                RETURN sfmt("DECIMAL(%1,2)",tables[t].cols[c].typel1)
             END IF
          ELSE
             RETURN "DECIMAL(16,2)"
          END IF
     WHEN dtn_smallint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_integer
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_bigint
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_int8
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_smallfloat
          RETURN "REAL"
     WHEN dtn_serial -- ! autoincrement column must be a primary key...
          LET tmp = "INTEGER AUTO_INCREMENT"
          IF NOT tables[t].cols[c].pkeycol THEN
             LET tmp = tmp, " PRIMARY KEY"
          END IF
          RETURN tmp
     WHEN dtn_bigserial -- ! autoincrement column must be a primary key...
          LET tmp = "BIGINT AUTO_INCREMENT"
          IF NOT tables[t].cols[c].pkeycol THEN
             LET tmp = tmp, " PRIMARY KEY"
          END IF
          RETURN tmp
     WHEN dtn_serial8 -- ! autoincrement column must be a primary key...
          LET tmp = "BIGINT AUTO_INCREMENT"
          IF NOT tables[t].cols[c].pkeycol THEN
             LET tmp = tmp, " PRIMARY KEY"
          END IF
          RETURN tmp
     WHEN dtn_varchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_nvarchar
          RETURN sql_serialize_type_ifx(t,c)
     WHEN dtn_varchar2
          RETURN sfmt("VARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_nvarchar2
          RETURN sfmt("NVARCHAR%1",sql_serialize_qual2(tables[t].cols[c].typel1,tables[t].cols[c].typel2))
     WHEN dtn_text
          RETURN "LONGTEXT"
     WHEN dtn_byte
          RETURN "LONGBLOB"
     --
     WHEN dtn_datetime_year_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_frac1
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_second
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_minute
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_hour
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_day
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_month
          RETURN "DATETIME"
     WHEN dtn_datetime_year_to_year
          RETURN "DATETIME"
     --
     WHEN dtn_datetime_month_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_frac1
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_second
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_minute
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_hour
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_day
          RETURN "DATETIME"
     WHEN dtn_datetime_month_to_month
          RETURN "DATETIME"
     --
     WHEN dtn_datetime_day_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_frac1
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_second
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_minute
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_hour
          RETURN "DATETIME"
     WHEN dtn_datetime_day_to_day
          RETURN "DATETIME"
     --
     WHEN dtn_datetime_hour_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_hour_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_hour_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_hour_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_hour_to_frac1
          RETURN "DATETIME"
     WHEN dtn_datetime_hour_to_second
          RETURN "TIME" -- !!!
     WHEN dtn_datetime_hour_to_minute
          RETURN "DATETIME"
     WHEN dtn_datetime_hour_to_hour
          RETURN "DATETIME"
     --
     WHEN dtn_datetime_minute_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_minute_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_minute_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_minute_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_minute_to_frac1
          RETURN "DATETIME"
     WHEN dtn_datetime_minute_to_second
          RETURN "DATETIME"
     WHEN dtn_datetime_minute_to_minute
          RETURN "DATETIME"
     --
     WHEN dtn_datetime_second_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_second_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_second_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_second_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_second_to_frac1
          RETURN "DATETIME"
     WHEN dtn_datetime_second_to_second
          RETURN "DATETIME"
     --
     WHEN dtn_datetime_fraction_to_frac5
          RETURN "DATETIME"
     WHEN dtn_datetime_fraction_to_frac4
          RETURN "DATETIME"
     WHEN dtn_datetime_fraction_to_frac3
          RETURN "DATETIME"
     WHEN dtn_datetime_fraction_to_frac2
          RETURN "DATETIME"
     WHEN dtn_datetime_fraction_to_frac1
          RETURN "DATETIME"
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_serialize_type(dbtype,t,c)
  DEFINE dbtype t_dbtype
  DEFINE t,c INTEGER
  CASE dbtype
    WHEN "IFX" RETURN sql_serialize_type_ifx(t,c)
    WHEN "ADS" RETURN sql_serialize_type_ads(t,c)
    WHEN "ORA" RETURN sql_serialize_type_ora(t,c)
    WHEN "DB2" RETURN sql_serialize_type_db2(t,c)
    WHEN "MSV" RETURN sql_serialize_type_msv(t,c)
    WHEN "PGS" RETURN sql_serialize_type_pgs(t,c)
    WHEN "MYS" RETURN sql_serialize_type_mys(t,c)
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_exec_tag(dbtype)
  DEFINE dbtype t_dbtype
  CASE dbtype
    WHEN "MSV" RETURN "go"
    OTHERWISE  RETURN ";"
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_identifier(id)
  DEFINE id STRING
  IF sql_params.upcaseids THEN
     LET id = id.toUpperCase()
  END IF
  IF sql_params.quotedids THEN
     RETURN '"'||id||'"'
  ELSE
     RETURN id
  END IF
END FUNCTION

FUNCTION sql_identlist(list)
  DEFINE list STRING
  DEFINE tmpl,ident STRING
  DEFINE tok base.StringTokenizer
  LET tok = base.StringTokenizer.create(list,",")
  WHILE tok.hasMoreTokens()
     LET ident = sql_identifier(tok.nextToken())
     IF tmpl IS NULL THEN
        LET tmpl = ident
     ELSE
        LET tmpl = tmpl, ",", ident 
     END IF
  END WHILE
  RETURN tmpl
END FUNCTION

FUNCTION sql_generate_script(schema)
  DEFINE schema STRING
  DEFINE t, c, i INTEGER
  DEFINE s, sql, cname, ctype STRING
  DEFINE ch base.Channel
  DEFINE rt, rc, rp INTEGER

  CALL fglt_log_open()
  CALL fglt_log_write(SFMT("Generating SQL script for %1 database...",sql_params.dbtype))

  LET ch = base.Channel.create()
  CALL ch.setDelimiter("")

  CALL ch.openfile(sql_params.filename,"w")

  LET sql = "-- SQL creation script\n",
            "--     Schema          : ", schema, "\n",
            "--     Date            : ", CURRENT YEAR TO SECOND, "\n",
            "--     Target database : ", sql_params.dbtype, "\n",
            "\n"
  CALL ch.write(sql)

  IF sql_params.droptables THEN
     CALL fglt_log_write("")
     CALL fglt_log_write("Generating table deletion commands...")
     FOR t=1 TO tables.getLength()
         CALL fglt_log_write("Table: "||tables[t].tabname)
         LET sql = "DROP TABLE ", sql_identifier(tables[t].tabname)
         LET sql = sql, "\n", sql_exec_tag(sql_params.dbtype), "\n"
         CALL ch.write(sql)
     END FOR
  END IF

  CALL fglt_log_write("")
  CALL fglt_log_write("Generating table creation commands...")
  FOR t=1 TO tables.getLength()
    CALL fglt_log_write("Table: "||tables[t].tabname)
    LET sql = "CREATE TABLE ", sql_identifier(tables[t].tabname), " (\n"
    FOR c=1 TO tables[t].cols.getLength()
      IF c>1 THEN LET sql = sql, ",\n" END IF
      LET ctype = sql_serialize_type(sql_params.dbtype,t,c)
      IF ctype IS NULL THEN
         CALL fglt_log_write(
           SFMT("ERROR: Cannot define native type of %1.%2, USING default type CHAR(100)...",
                 tables[t].tabname, tables[t].cols[c].colname))
         LET ctype = "CHAR(100)"
      END IF
      LET sql = sql, " ", sql_identifier(tables[t].cols[c].colname), " ", ctype
      IF LENGTH(tables[t].cols[c].defvalue) > 0 THEN
         LET sql = sql, " DEFAULT ", sql_default_value(sql_params.dbtype,t,c)
      END IF
      IF tables[t].cols[c].typenn THEN
         LET sql = sql, " NOT NULL"
      END IF
    END FOR
    IF table_has_pkeycols(t) THEN
       LET sql = sql, ",\n"
       LET cname = sql_constraint(tables[t].pkeyname)
       IF sql_params.dbtype!="IFX" AND cname IS NOT NULL THEN
          LET sql = sql, cname
       END IF
       LET sql = sql, " PRIMARY KEY ("
       LET s = NULL
       FOR i=1 TO tables[t].cols.getLength()
           IF tables[t].cols[i].pkeycol THEN
              IF s IS NOT NULL THEN LET s = s, ", " END IF
              LET s = s, sql_identifier(tables[t].cols[i].colname)
           END IF
       END FOR
       LET sql = sql, s, ")"
       IF sql_params.dbtype=="IFX" AND cname IS NOT NULL THEN
          LET sql = sql, cname
       END IF
    END IF
    IF tables[t].skeys.getLength() > 0 THEN
       FOR i=1 TO tables[t].skeys.getLength()
         LET cname = sql_constraint(tables[t].skeys[i].skeyname)
         LET sql = sql, ",\n"
         IF sql_params.dbtype!="IFX" AND cname IS NOT NULL THEN
            LET sql = sql, cname
         END IF
         LET sql = sql, " UNIQUE (", sql_identlist(tables[t].skeys[i].skeycols), ")"
         IF sql_params.dbtype=="IFX" AND cname IS NOT NULL THEN
            LET sql = sql, cname
         END IF
       END FOR
    END IF
    IF tables[t].checks.getLength() > 0 THEN
       FOR i=1 TO tables[t].checks.getLength()
         LET cname = sql_constraint(tables[t].checks[i].checkname)
         LET sql = sql, ",\n"
         IF sql_params.dbtype!="IFX" AND cname IS NOT NULL THEN
            LET sql = sql, cname
         END IF
         LET sql = sql, " CHECK ( ", tables[t].checks[i].sqlcond, " )"
         IF sql_params.dbtype=="IFX" AND cname IS NOT NULL THEN
            LET sql = sql, cname
         END IF
       END FOR
    END IF
    LET sql = sql, "\n)"
    IF sql_params.tableopts THEN
       LET i = tabopt_lookup(t,sql_params.dbtype)
       IF i>0 THEN
          LET sql = sql, "\n", tables[t].tabopts[i].sqltext
       END IF
    END IF
    LET sql = sql, "\n", sql_exec_tag(sql_params.dbtype), "\n"
    CALL ch.write(sql)
  END FOR

  IF sql_params.createfkeys THEN
 
  CALL fglt_log_write("")
  CALL fglt_log_write("Generating foreign key creation commands...")
  FOR t=1 TO tables.getLength()
    CALL fglt_log_write("Table: "||tables[t].tabname)
    FOR i=1 TO tables[t].fkeys.getLength()
        LET sql = "ALTER TABLE ", sql_identifier(tables[t].tabname), "\n"
        IF sql_params.dbtype=="IFX" THEN
           LET sql = sql, " ADD CONSTRAINT"
        ELSE
           LET sql = sql, " ADD"
        END IF
        LET cname = sql_constraint(tables[t].fkeys[i].fkeyname)
        IF sql_params.dbtype!="IFX" AND cname IS NOT NULL THEN
           LET sql = sql, cname
        END IF
        LET sql = sql, " FOREIGN KEY (", sql_identlist(tables[t].fkeys[i].fkeycols), ")"
        LET sql = sql, " REFERENCES ", sql_identifier(tables[t].fkeys[i].reftabname)
        CALL constraint_lookup(tables[t].fkeys[i].refconstname)
             RETURNING rt,rc,rp
        IF rt>0 AND rc==constype_skey THEN
                    -- no need to specify columns when pkey used...
           LET sql = sql, " (", sql_identlist(tables[rt].skeys[rp].skeycols), ")"
        END IF
        CASE tables[t].fkeys[i].delrule
             WHEN udrule_noaction
                  LET sql = sql, " ON DELETE NO ACTION"
             WHEN udrule_restrict
                  LET sql = sql, " ON DELETE RESTRICT"
             WHEN udrule_cascade
                  LET sql = sql, " ON DELETE CASCADE"
             WHEN udrule_setnull
                  LET sql = sql, " ON DELETE SET NULL"
             WHEN udrule_setdefault
                  LET sql = sql, " ON DELETE SET DEFAULT"
        END CASE
        CASE tables[t].fkeys[i].updrule
             WHEN udrule_noaction
                  LET sql = sql, " ON UPDATE NO ACTION"
             WHEN udrule_restrict
                  LET sql = sql, " ON UPDATE RESTRICT"
             WHEN udrule_cascade
                  LET sql = sql, " ON UPDATE CASCADE"
             WHEN udrule_setnull
                  LET sql = sql, " ON UPDATE SET NULL"
             WHEN udrule_setdefault
                  LET sql = sql, " ON UPDATE SET DEFAULT"
        END CASE
        IF sql_params.dbtype=="IFX" AND cname IS NOT NULL THEN
           LET sql = sql, cname
        END IF
        LET sql = sql, "\n", sql_exec_tag(sql_params.dbtype), "\n"
        CALL ch.write(sql)
    END FOR
  END FOR

  END IF

  CALL fglt_log_write("")
  CALL fglt_log_write("Generation finished.")

  CALL fglt_log_close()

  CALL ch.close()

  RETURN 0

END FUNCTION

FUNCTION sql_default_value(dbtype,t,c)
  DEFINE dbtype t_dbtype
  DEFINE t,c INTEGER
  CASE dbtype
    WHEN "ADS" RETURN sql_default_value_ads(t,c)
    WHEN "IFX" RETURN sql_default_value_ifx(t,c)
    WHEN "ORA" RETURN sql_default_value_ora(t,c)
    WHEN "DB2" RETURN sql_default_value_db2(t,c)
    WHEN "MSV" RETURN sql_default_value_msv(t,c)
    WHEN "PGS" RETURN sql_default_value_pgs(t,c)
    WHEN "MYS" RETURN sql_default_value_mys(t,c)
  END CASE
  RETURN NULL
END FUNCTION

FUNCTION sql_default_value_ads(t,c)
  DEFINE t,c INTEGER
  IF tables[t].cols[c].typenum == dtn_boolean THEN
     RETURN tables[t].cols[c].defvalue
  END IF
  RETURN sql_default_value_ifx(t,c)
END FUNCTION

FUNCTION sql_default_value_ifx(t,c)
  DEFINE t,c INTEGER
  DEFINE v STRING
  DEFINE typenum INTEGER
  LET v = tables[t].cols[c].defvalue
  LET typenum = tables[t].cols[c].typenum
  CASE
     WHEN typenum == dtn_boolean
          IF v = "TRUE" THEN LET v = "'t'" ELSE LET v = "'f'" END IF
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          IF v = "CURRENT" THEN
             LET v = sql_serialize_type_ifx(t,c) -- DATETIME x to y
             LET v = "CURRENT", v.subString(9,v.getLength())
          END IF
  END CASE
  RETURN v
END FUNCTION

FUNCTION sql_default_value_ora(t,c)
  DEFINE t,c INTEGER
  DEFINE v STRING
  DEFINE typenum INTEGER
  LET v = tables[t].cols[c].defvalue
  LET typenum = tables[t].cols[c].typenum
  CASE
     WHEN typenum == dtn_boolean
          IF v = "TRUE" THEN LET v = "'1'" ELSE LET v = "'0'" END IF
     WHEN typenum == dtn_date AND v == "TODAY"
          LET v = "TRUNC(SYSDATE)" -- only YYYY/MM/DD !
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          IF v = "CURRENT" THEN
             LET v = "SYSTIMESTAMP"
          END IF
  END CASE
  RETURN v
END FUNCTION

FUNCTION sql_default_value_db2(t,c)
  DEFINE t,c INTEGER
  DEFINE v STRING
  DEFINE typenum INTEGER
  LET v = tables[t].cols[c].defvalue
  LET typenum = tables[t].cols[c].typenum
  CASE
     WHEN typenum == dtn_boolean
          IF v = "TRUE" THEN LET v = "'1'" ELSE LET v = "'0'" END IF
     WHEN typenum == dtn_date AND v == "TODAY"
          LET v = "CURRENT DATE"
     WHEN typenum == dtn_datetime_hour_to_second 
          IF v = "CURRENT" THEN
             LET v = "CURRENT TIME"
          END IF
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          IF v = "CURRENT" THEN
             LET v = "CURRENT TIMESTAMP"
          END IF
  END CASE
  RETURN v
END FUNCTION

FUNCTION sql_default_value_pgs(t,c)
  DEFINE t,c INTEGER
  DEFINE v STRING
  DEFINE typenum INTEGER
  LET v = tables[t].cols[c].defvalue
  LET typenum = tables[t].cols[c].typenum
  CASE
     WHEN typenum == dtn_boolean
          IF v = "TRUE" THEN LET v = "'t'" ELSE LET v = "'f'" END IF
     WHEN typenum == dtn_date AND v == "TODAY"
          LET v = "CURRENT_DATE"
     WHEN typenum == dtn_datetime_hour_to_second 
          IF v = "CURRENT" THEN
             LET v = "CURRENT_TIME(0)"
          END IF
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          IF v = "CURRENT" THEN
             LET v = "CURRENT_TIMESTAMP"
          END IF
  END CASE
  RETURN v
END FUNCTION

FUNCTION sql_default_value_msv(t,c)
  DEFINE t,c INTEGER
  DEFINE v STRING
  DEFINE typenum INTEGER
  LET v = tables[t].cols[c].defvalue
  LET typenum = tables[t].cols[c].typenum
  CASE
     WHEN typenum == dtn_boolean
          IF v = "TRUE" THEN LET v = "1" ELSE LET v = "0" END IF
     WHEN typenum == dtn_date AND v == "TODAY"
          LET v = "CONVERT(DATETIME,CONVERT(VARCHAR,GETDATE(),112))"
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          IF v = "CURRENT" THEN
             LET v = "GETDATE()"
          END IF
  END CASE
  RETURN v
END FUNCTION

FUNCTION sql_default_value_mys(t,c)
  DEFINE t,c INTEGER
  DEFINE v STRING
  DEFINE typenum INTEGER
  LET v = tables[t].cols[c].defvalue
  LET typenum = tables[t].cols[c].typenum
  CASE
     WHEN typenum == dtn_date AND v == "TODAY"
          LET v = "CURDATE()"
     WHEN typenum == dtn_datetime_hour_to_second 
          IF v = "CURRENT" THEN
             LET v = "CURTIME()"
          END IF
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          IF v = "CURRENT" THEN
             LET v = "NOW()"
          END IF
  END CASE
  RETURN v
END FUNCTION

FUNCTION dtdef_init()
  CALL dtdef.clear()
  CALL dtdef_create(dtn_char,"CHAR", dtid_char, 1, 0, 0, 0)
  CALL dtdef_create(dtn_boolean, "BOOLEAN", dtid_boolean, 0, 0, 0, 0)
  CALL dtdef_create(dtn_date, "DATE", dtid_date, 0, 0, 0, 0)
  CALL dtdef_create(dtn_decimal, "DECIMAL", dtid_decimal, 2, 0, 0, 0)
  CALL dtdef_create(dtn_float, "FLOAT", dtid_float, 1, 0, 0, 0)
  CALL dtdef_create(dtn_money, "MONEY", dtid_money, 2, 0, 0, 0)
  CALL dtdef_create(dtn_smallint, "SMALLINT", dtid_smallint, 0, 0, 0, 0)
  CALL dtdef_create(dtn_integer, "INTEGER", dtid_integer, 0, 0, 0, 0)
  CALL dtdef_create(dtn_bigint, "BIGINT", dtid_bigint, 0, 0, 0, 0)
  CALL dtdef_create(dtn_int8, "INT8", dtid_int8, 0, 0, 0, 0)
  CALL dtdef_create(dtn_smallfloat, "SMALLFLOAT", dtid_smallfloat, 0, 0, 0, 0)
  CALL dtdef_create(dtn_serial, "SERIAL", dtid_serial, 0, 0, 0, 0)
  CALL dtdef_create(dtn_bigserial, "BIGSERIAL", dtid_bigserial, 0, 0, 0, 0)
  CALL dtdef_create(dtn_serial8, "SERIAL8", dtid_serial8, 0, 0, 0, 0)
  CALL dtdef_create(dtn_text, "TEXT", dtid_text, 0, 0, 0, 0)
  CALL dtdef_create(dtn_byte, "BYTE", dtid_byte, 0, 0, 0, 0)
  CALL dtdef_create(dtn_varchar, "VARCHAR", dtid_varchar, 1, 0, 0, 0)
  CALL dtdef_create(dtn_nchar, "NCHAR", dtid_nchar, 1, 0, 0, 0)
  CALL dtdef_create(dtn_nvarchar, "NVARCHAR", dtid_nvarchar, 2, 0, 0, 0)
  CALL dtdef_create(dtn_varchar2, "VARCHAR2", dtid_varchar2, 1, 0, 0, 0)
  CALL dtdef_create(dtn_nvarchar2, "NVARCHAR2", dtid_nvarchar2, 1, 0, 0, 0)
  CALL dtdef_create(dtn_datetime_year_to_frac5, "DATETIME YEAR TO FRACTION(5)", dtid_datetime, 0, dtiqual_year, dtiqual_frac5, 19)
  CALL dtdef_create(dtn_datetime_year_to_frac4, "DATETIME YEAR TO FRACTION(4)", dtid_datetime, 0, dtiqual_year, dtiqual_frac4, 18)
  CALL dtdef_create(dtn_datetime_year_to_frac3, "DATETIME YEAR TO FRACTION(3)", dtid_datetime, 0, dtiqual_year, dtiqual_frac3, 17)
  CALL dtdef_create(dtn_datetime_year_to_frac2, "DATETIME YEAR TO FRACTION(2)", dtid_datetime, 0, dtiqual_year, dtiqual_frac2, 16)
  CALL dtdef_create(dtn_datetime_year_to_frac1, "DATETIME YEAR TO FRACTION(1)", dtid_datetime, 0, dtiqual_year, dtiqual_frac1, 15)
  CALL dtdef_create(dtn_datetime_year_to_second, "DATETIME YEAR TO SECOND", dtid_datetime, 0, dtiqual_year, dtiqual_second, 14)
  CALL dtdef_create(dtn_datetime_year_to_minute, "DATETIME YEAR TO MINUTE", dtid_datetime, 0, dtiqual_year, dtiqual_minute, 12)
  CALL dtdef_create(dtn_datetime_year_to_hour, "DATETIME YEAR TO HOUR", dtid_datetime, 0, dtiqual_year, dtiqual_hour, 10)
  CALL dtdef_create(dtn_datetime_year_to_day, "DATETIME YEAR TO DAY", dtid_datetime, 0, dtiqual_year, dtiqual_day, 8)
  CALL dtdef_create(dtn_datetime_year_to_month, "DATETIME YEAR TO MONTH", dtid_datetime, 0, dtiqual_year, dtiqual_month, 6)
  CALL dtdef_create(dtn_datetime_year_to_year, "DATETIME YEAR TO YEAR", dtid_datetime, 0, dtiqual_year, dtiqual_year, 4)
  CALL dtdef_create(dtn_datetime_month_to_frac5, "DATETIME MONTH TO FRACTION(5)", dtid_datetime, 0, dtiqual_month, dtiqual_frac5, 15)
  CALL dtdef_create(dtn_datetime_month_to_frac4, "DATETIME MONTH TO FRACTION(4)", dtid_datetime, 0, dtiqual_month, dtiqual_frac4, 14)
  CALL dtdef_create(dtn_datetime_month_to_frac3, "DATETIME MONTH TO FRACTION(3)", dtid_datetime, 0, dtiqual_month, dtiqual_frac3, 13)
  CALL dtdef_create(dtn_datetime_month_to_frac2, "DATETIME MONTH TO FRACTION(2)", dtid_datetime, 0, dtiqual_month, dtiqual_frac2, 12)
  CALL dtdef_create(dtn_datetime_month_to_frac1, "DATETIME MONTH TO FRACTION(1)", dtid_datetime, 0, dtiqual_month, dtiqual_frac1, 11)
  CALL dtdef_create(dtn_datetime_month_to_second, "DATETIME MONTH TO SECOND", dtid_datetime, 0, dtiqual_month, dtiqual_second, 10)
  CALL dtdef_create(dtn_datetime_month_to_minute, "DATETIME MONTH TO MINUTE", dtid_datetime, 0, dtiqual_month, dtiqual_minute, 8)
  CALL dtdef_create(dtn_datetime_month_to_hour, "DATETIME MONTH TO HOUR", dtid_datetime, 0, dtiqual_month, dtiqual_hour, 6)
  CALL dtdef_create(dtn_datetime_month_to_day, "DATETIME MONTH TO DAY", dtid_datetime, 0, dtiqual_month, dtiqual_day, 4)
  CALL dtdef_create(dtn_datetime_month_to_month, "DATETIME MONTH TO MONTH", dtid_datetime, 0, dtiqual_month, dtiqual_month, 2)
  CALL dtdef_create(dtn_datetime_day_to_frac5, "DATETIME DAY TO FRACTION(5)", dtid_datetime, 0, dtiqual_day, dtiqual_frac5, 13)
  CALL dtdef_create(dtn_datetime_day_to_frac4, "DATETIME DAY TO FRACTION(4)", dtid_datetime, 0, dtiqual_day, dtiqual_frac4, 12)
  CALL dtdef_create(dtn_datetime_day_to_frac3, "DATETIME DAY TO FRACTION(3)", dtid_datetime, 0, dtiqual_day, dtiqual_frac3, 11)
  CALL dtdef_create(dtn_datetime_day_to_frac2, "DATETIME DAY TO FRACTION(2)", dtid_datetime, 0, dtiqual_day, dtiqual_frac2, 10)
  CALL dtdef_create(dtn_datetime_day_to_frac1, "DATETIME DAY TO FRACTION(1)", dtid_datetime, 0, dtiqual_day, dtiqual_frac1, 9)
  CALL dtdef_create(dtn_datetime_day_to_second, "DATETIME DAY TO SECOND", dtid_datetime, 0, dtiqual_day, dtiqual_second, 8)
  CALL dtdef_create(dtn_datetime_day_to_minute, "DATETIME DAY TO MINUTE", dtid_datetime, 0, dtiqual_day, dtiqual_minute, 6)
  CALL dtdef_create(dtn_datetime_day_to_hour, "DATETIME DAY TO HOUR", dtid_datetime, 0, dtiqual_day, dtiqual_hour, 4)
  CALL dtdef_create(dtn_datetime_day_to_day, "DATETIME DAY TO DAY", dtid_datetime, 0, dtiqual_day, dtiqual_day, 2)
  CALL dtdef_create(dtn_datetime_hour_to_frac5, "DATETIME HOUR TO FRACTION(5)", dtid_datetime, 0, dtiqual_hour, dtiqual_frac5, 11)
  CALL dtdef_create(dtn_datetime_hour_to_frac4, "DATETIME HOUR TO FRACTION(4)", dtid_datetime, 0, dtiqual_hour, dtiqual_frac4, 10)
  CALL dtdef_create(dtn_datetime_hour_to_frac3, "DATETIME HOUR TO FRACTION(3)", dtid_datetime, 0, dtiqual_hour, dtiqual_frac3, 9)
  CALL dtdef_create(dtn_datetime_hour_to_frac2, "DATETIME HOUR TO FRACTION(2)", dtid_datetime, 0, dtiqual_hour, dtiqual_frac2, 8)
  CALL dtdef_create(dtn_datetime_hour_to_frac1, "DATETIME HOUR TO FRACTION(1)", dtid_datetime, 0, dtiqual_hour, dtiqual_frac1, 7)
  CALL dtdef_create(dtn_datetime_hour_to_second, "DATETIME HOUR TO SECOND", dtid_datetime, 0, dtiqual_hour, dtiqual_second, 6)
  CALL dtdef_create(dtn_datetime_hour_to_minute, "DATETIME HOUR TO MINUTE", dtid_datetime, 0, dtiqual_hour, dtiqual_minute, 4)
  CALL dtdef_create(dtn_datetime_hour_to_hour, "DATETIME HOUR TO HOUR", dtid_datetime, 0, dtiqual_hour, dtiqual_hour, 2)
  CALL dtdef_create(dtn_datetime_minute_to_frac5, "DATETIME MINUTE TO FRACTION(5)", dtid_datetime, 0, dtiqual_minute, dtiqual_frac5, 9)
  CALL dtdef_create(dtn_datetime_minute_to_frac4, "DATETIME MINUTE TO FRACTION(4)", dtid_datetime, 0, dtiqual_minute, dtiqual_frac4, 8)
  CALL dtdef_create(dtn_datetime_minute_to_frac3, "DATETIME MINUTE TO FRACTION(3)", dtid_datetime, 0, dtiqual_minute, dtiqual_frac3, 7)
  CALL dtdef_create(dtn_datetime_minute_to_frac2, "DATETIME MINUTE TO FRACTION(2)", dtid_datetime, 0, dtiqual_minute, dtiqual_frac2, 6)
  CALL dtdef_create(dtn_datetime_minute_to_frac1, "DATETIME MINUTE TO FRACTION(1)", dtid_datetime, 0, dtiqual_minute, dtiqual_frac1, 5)
  CALL dtdef_create(dtn_datetime_minute_to_second, "DATETIME MINUTE TO SECOND", dtid_datetime, 0, dtiqual_minute, dtiqual_second, 4)
  CALL dtdef_create(dtn_datetime_minute_to_minute, "DATETIME MINUTE TO MINUTE", dtid_datetime, 0, dtiqual_minute, dtiqual_minute, 2)
  CALL dtdef_create(dtn_datetime_second_to_frac5, "DATETIME SECOND TO FRACTION(5)", dtid_datetime, 0, dtiqual_second, dtiqual_frac5, 7)
  CALL dtdef_create(dtn_datetime_second_to_frac4, "DATETIME SECOND TO FRACTION(4)", dtid_datetime, 0, dtiqual_second, dtiqual_frac4, 6)
  CALL dtdef_create(dtn_datetime_second_to_frac3, "DATETIME SECOND TO FRACTION(3)", dtid_datetime, 0, dtiqual_second, dtiqual_frac3, 5)
  CALL dtdef_create(dtn_datetime_second_to_frac2, "DATETIME SECOND TO FRACTION(2)", dtid_datetime, 0, dtiqual_second, dtiqual_frac2, 4)
  CALL dtdef_create(dtn_datetime_second_to_frac1, "DATETIME SECOND TO FRACTION(1)", dtid_datetime, 0, dtiqual_second, dtiqual_frac1, 3)
  CALL dtdef_create(dtn_datetime_second_to_second, "DATETIME SECOND TO SECOND", dtid_datetime, 0, dtiqual_second, dtiqual_second, 2)
  CALL dtdef_create(dtn_datetime_fraction_to_frac5, "DATETIME FRACTION TO FRACTION(5)", dtid_datetime, 0, dtiqual_fraction, dtiqual_frac5, 5)
  CALL dtdef_create(dtn_datetime_fraction_to_frac4, "DATETIME FRACTION TO FRACTION(4)", dtid_datetime, 0, dtiqual_fraction, dtiqual_frac4, 4)
  CALL dtdef_create(dtn_datetime_fraction_to_frac3, "DATETIME FRACTION TO FRACTION(3)", dtid_datetime, 0, dtiqual_fraction, dtiqual_frac3, 3)
  CALL dtdef_create(dtn_datetime_fraction_to_frac2, "DATETIME FRACTION TO FRACTION(2)", dtid_datetime, 0, dtiqual_fraction, dtiqual_frac2, 2)
  CALL dtdef_create(dtn_datetime_fraction_to_frac1, "DATETIME FRACTION TO FRACTION(1)", dtid_datetime, 0, dtiqual_fraction, dtiqual_frac1, 1)
  CALL dtdef_create(dtn_interval_year_to_month, "INTERVAL YEAR TO MONTH", dtid_interval, 1, dtiqual_year, dtiqual_month, 0)
  CALL dtdef_create(dtn_interval_year_to_year, "INTERVAL YEAR TO YEAR", dtid_interval, 1, dtiqual_year, dtiqual_year, 0)
  CALL dtdef_create(dtn_interval_month_to_month, "INTERVAL MONTH TO MONTH", dtid_interval, 1, dtiqual_month, dtiqual_month, 0)
  CALL dtdef_create(dtn_interval_day_to_frac5, "INTERVAL DAY TO FRACTION(5)", dtid_interval, 1, dtiqual_day, dtiqual_frac5, 0)
  CALL dtdef_create(dtn_interval_day_to_frac4, "INTERVAL DAY TO FRACTION(4)", dtid_interval, 1, dtiqual_day, dtiqual_frac4, 0)
  CALL dtdef_create(dtn_interval_day_to_frac3, "INTERVAL DAY TO FRACTION(3)", dtid_interval, 1, dtiqual_day, dtiqual_frac3, 0)
  CALL dtdef_create(dtn_interval_day_to_frac2, "INTERVAL DAY TO FRACTION(2)", dtid_interval, 1, dtiqual_day, dtiqual_frac2, 0)
  CALL dtdef_create(dtn_interval_day_to_frac1, "INTERVAL DAY TO FRACTION(1)", dtid_interval, 1, dtiqual_day, dtiqual_frac1, 0)
  CALL dtdef_create(dtn_interval_day_to_second, "INTERVAL DAY TO SECOND", dtid_interval, 1, dtiqual_day, dtiqual_second, 0)
  CALL dtdef_create(dtn_interval_day_to_minute, "INTERVAL DAY TO MINUTE", dtid_interval, 1, dtiqual_day, dtiqual_minute, 0)
  CALL dtdef_create(dtn_interval_day_to_hour, "INTERVAL DAY TO HOUR", dtid_interval, 1, dtiqual_day, dtiqual_hour, 0)
  CALL dtdef_create(dtn_interval_day_to_day, "INTERVAL DAY TO DAY", dtid_interval, 1, dtiqual_day, dtiqual_day, 0)
  CALL dtdef_create(dtn_interval_hour_to_frac5, "INTERVAL HOUR TO FRACTION(5)", dtid_interval, 1, dtiqual_hour, dtiqual_frac5, 0)
  CALL dtdef_create(dtn_interval_hour_to_frac4, "INTERVAL HOUR TO FRACTION(4)", dtid_interval, 1, dtiqual_hour, dtiqual_frac4, 0)
  CALL dtdef_create(dtn_interval_hour_to_frac3, "INTERVAL HOUR TO FRACTION(3)", dtid_interval, 1, dtiqual_hour, dtiqual_frac3, 0)
  CALL dtdef_create(dtn_interval_hour_to_frac2, "INTERVAL HOUR TO FRACTION(2)", dtid_interval, 1, dtiqual_hour, dtiqual_frac2, 0)
  CALL dtdef_create(dtn_interval_hour_to_frac1, "INTERVAL HOUR TO FRACTION(1)", dtid_interval, 1, dtiqual_hour, dtiqual_frac1, 0)
  CALL dtdef_create(dtn_interval_hour_to_second, "INTERVAL HOUR TO SECOND", dtid_interval, 1, dtiqual_hour, dtiqual_second, 0)
  CALL dtdef_create(dtn_interval_hour_to_minute, "INTERVAL HOUR TO MINUTE", dtid_interval, 1, dtiqual_hour, dtiqual_minute, 0)
  CALL dtdef_create(dtn_interval_hour_to_hour, "INTERVAL HOUR TO HOUR", dtid_interval, 1, dtiqual_hour, dtiqual_hour, 0)
  CALL dtdef_create(dtn_interval_minute_to_frac5, "INTERVAL MINUTE TO FRACTION(5)", dtid_interval, 1, dtiqual_minute, dtiqual_frac5, 0)
  CALL dtdef_create(dtn_interval_minute_to_frac4, "INTERVAL MINUTE TO FRACTION(4)", dtid_interval, 1, dtiqual_minute, dtiqual_frac4, 0)
  CALL dtdef_create(dtn_interval_minute_to_frac3, "INTERVAL MINUTE TO FRACTION(3)", dtid_interval, 1, dtiqual_minute, dtiqual_frac3, 0)
  CALL dtdef_create(dtn_interval_minute_to_frac2, "INTERVAL MINUTE TO FRACTION(2)", dtid_interval, 1, dtiqual_minute, dtiqual_frac2, 0)
  CALL dtdef_create(dtn_interval_minute_to_frac1, "INTERVAL MINUTE TO FRACTION(1)", dtid_interval, 1, dtiqual_minute, dtiqual_frac1, 0)
  CALL dtdef_create(dtn_interval_minute_to_second, "INTERVAL MINUTE TO SECOND", dtid_interval, 1, dtiqual_minute, dtiqual_second, 0)
  CALL dtdef_create(dtn_interval_minute_to_minute, "INTERVAL MINUTE TO MINUTE", dtid_interval, 1, dtiqual_minute, dtiqual_minute, 0)
  CALL dtdef_create(dtn_interval_second_to_frac5, "INTERVAL SECOND TO FRACTION(5)", dtid_interval, 1, dtiqual_second, dtiqual_frac5, 0)
  CALL dtdef_create(dtn_interval_second_to_frac4, "INTERVAL SECOND TO FRACTION(4)", dtid_interval, 1, dtiqual_second, dtiqual_frac4, 0)
  CALL dtdef_create(dtn_interval_second_to_frac3, "INTERVAL SECOND TO FRACTION(3)", dtid_interval, 1, dtiqual_second, dtiqual_frac3, 0)
  CALL dtdef_create(dtn_interval_second_to_frac2, "INTERVAL SECOND TO FRACTION(2)", dtid_interval, 1, dtiqual_second, dtiqual_frac2, 0)
  CALL dtdef_create(dtn_interval_second_to_frac1, "INTERVAL SECOND TO FRACTION(1)", dtid_interval, 1, dtiqual_second, dtiqual_frac1, 0)
  CALL dtdef_create(dtn_interval_second_to_second, "INTERVAL SECOND TO SECOND", dtid_interval, 1, dtiqual_second, dtiqual_second, 0)
  CALL dtdef_create(dtn_interval_fraction_to_frac5, "INTERVAL FRACTION TO FRACTION(5)", dtid_interval, 1, dtiqual_fraction, dtiqual_frac5, 0)
  CALL dtdef_create(dtn_interval_fraction_to_frac4, "INTERVAL FRACTION TO FRACTION(4)", dtid_interval, 1, dtiqual_fraction, dtiqual_frac4, 0)
  CALL dtdef_create(dtn_interval_fraction_to_frac3, "INTERVAL FRACTION TO FRACTION(3)", dtid_interval, 1, dtiqual_fraction, dtiqual_frac3, 0)
  CALL dtdef_create(dtn_interval_fraction_to_frac2, "INTERVAL FRACTION TO FRACTION(2)", dtid_interval, 1, dtiqual_fraction, dtiqual_frac2, 0)
  CALL dtdef_create(dtn_interval_fraction_to_frac1, "INTERVAL FRACTION TO FRACTION(1)", dtid_interval, 1, dtiqual_fraction, dtiqual_frac1, 0)
END FUNCTION

FUNCTION dtdef_create(num,name,id,qcode,qual1,qual2,psize)
  DEFINE num SMALLINT
  DEFINE name STRING
  DEFINE id SMALLINT
  DEFINE qcode SMALLINT
  DEFINE qual1 INTEGER
  DEFINE qual2 INTEGER
  DEFINE psize INTEGER
  DEFINE i INTEGER
  CALL dtdef.appendElement()
  LET i = dtdef.getLength()
  LET dtdef[i].num = num
  LET dtdef[i].name = name
  LET dtdef[i].typeid = id
  LET dtdef[i].qcode = qcode
  LET dtdef[i].qual1 = qual1
  LET dtdef[i].qual2 = qual2
  LET dtdef[i].psize = psize
END FUNCTION

FUNCTION dtdef_lookup_by_num(num)
  DEFINE num INTEGER
  DEFINE i INTEGER
  FOR i=1 TO dtdef.getLength()
      IF dtdef[i].num = num THEN
         RETURN i
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION dtdef_lookup_by_name(name)
  DEFINE name STRING
  DEFINE i INTEGER
  FOR i=1 TO dtdef.getLength()
      IF dtdef[i].name = name THEN
         RETURN i
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION dtdef_has_length_qual(typenum,lenqual)
  DEFINE typenum, lenqual INTEGER
  DEFINE i INTEGER
  LET i = dtdef_lookup_by_num(typenum)
  IF i>0 THEN
     IF lenqual == 1 THEN
        RETURN ( dtdef[i].qcode >= 1 )
     ELSE
        RETURN ( dtdef[i].qcode == 2 )
     END IF
  ELSE
     RETURN FALSE
  END IF
END FUNCTION


FUNCTION schema_save(filename)
  DEFINE filename STRING
  DEFINE x,y,i INTEGER
  DEFINE ddoc om.DomDocument
  DEFINE root, tnode, lnode, cnode om.DomNode

  LET ddoc = om.DomDocument.create("dbschema")
  LET root = ddoc.getDocumentElement()

  IF hprms.schname IS NULL THEN
     LET hprms.schname = get_schname_from_fname(filename)
  END IF

  IF hprms.schrevision IS NULL THEN
     LET hprms.schrevision = 1
  ELSE
     LET hprms.schrevision = hprms.schrevision + 1
  END IF

  LET hprms.schmodif = CURRENT YEAR TO SECOND

  CALL root.setAttribute( "name",           hprms.schname )
  CALL root.setAttribute( "owner",          hprms.schowner )
  CALL root.setAttribute( "version",        hprms.schversion )
  CALL root.setAttribute( "revision",       hprms.schrevision )
  CALL root.setAttribute( "quotedids",      hprms.schquotedids )
  CALL root.setAttribute( "creation",       hprms.schcreat )
  CALL root.setAttribute( "modification",   hprms.schmodif )
  CALL root.setAttribute( "comment",        hprms.schcomment )

  FOR x=1 TO tables.getLength()

     LET tnode = root.createChild("Table")
     CALL root.appendChild(tnode)

     CALL tnode.setAttribute( "name", tables[x].tabname )
     CALL tnode.setAttribute( "owner", tables[x].owner )

     IF tables[x].pkeyname IS NOT NULL THEN
        CALL tnode.setAttribute( "pKeyName", tables[x].pkeyname )
     END IF

     LET lnode = tnode.createChild("ColumnList")
     CALL tnode.appendChild(lnode)
     FOR y=1 TO tables[x].cols.getLength()
         LET cnode = lnode.createChild("Column")
         CALL lnode.appendChild(cnode)
         CALL cnode.setAttribute( "position",  y )
         CALL cnode.setAttribute( "name",      tables[x].cols[y].colname )
         LET i = dtdef_lookup_by_num(tables[x].cols[y].typenum)
         CALL cnode.setAttribute( "typeName",  dtdef[i].name )
         CALL cnode.setAttribute( "typeLen1",  tables[x].cols[y].typel1 )
         CALL cnode.setAttribute( "typeLen2",  tables[x].cols[y].typel2 )
         CALL cnode.setAttribute( "notNull",   tables[x].cols[y].typenn )
         CALL cnode.setAttribute( "pKeyCol",   tables[x].cols[y].pkeycol )
         CALL cnode.setAttribute( "defValue",  tables[x].cols[y].defvalue )
     END FOR

     LET lnode = tnode.createChild("CreateOptionList")
     CALL tnode.appendChild(lnode)
     FOR y=1 TO tables[x].tabopts.getLength()
         LET cnode = lnode.createChild("CreateOption")
         CALL lnode.appendChild(cnode)
         CALL cnode.setAttribute( "dbType",    tables[x].tabopts[y].dbtype )
         CALL cnode.setAttribute( "sqlText",   tables[x].tabopts[y].sqltext )
     END FOR

     LET lnode = tnode.createChild("SecondaryKeyConstraintList")
     CALL tnode.appendChild(lnode)
     FOR y=1 TO tables[x].skeys.getLength()
         LET cnode = lnode.createChild("SecondaryKeyConstraint")
         CALL lnode.appendChild(cnode)
         CALL cnode.setAttribute( "name",    tables[x].skeys[y].skeyname )
         CALL cnode.setAttribute( "columns", tables[x].skeys[y].skeycols )
     END FOR

     LET lnode = tnode.createChild("ForeignKeyConstraintList")
     CALL tnode.appendChild(lnode)
     FOR y=1 TO tables[x].fkeys.getLength()
         LET cnode = lnode.createChild("ForeignKeyConstraint")
         CALL lnode.appendChild(cnode)
         CALL cnode.setAttribute( "name",          tables[x].fkeys[y].fkeyname )
         CALL cnode.setAttribute( "columns",       tables[x].fkeys[y].fkeycols )
         CALL cnode.setAttribute( "refTable",      tables[x].fkeys[y].reftabname )
         CALL cnode.setAttribute( "refConstraint", tables[x].fkeys[y].refconstname )
         CALL cnode.setAttribute( "deleteRule",    tables[x].fkeys[y].delrule )
         CALL cnode.setAttribute( "updateRule",    tables[x].fkeys[y].updrule )
     END FOR

     LET lnode = tnode.createChild("CheckConstraintList")
     CALL tnode.appendChild(lnode)
     FOR y=1 TO tables[x].checks.getLength()
         LET cnode = lnode.createChild("CheckConstraint")
         CALL lnode.appendChild(cnode)
         CALL cnode.setAttribute( "name",         tables[x].checks[y].checkname )
         CALL cnode.setAttribute( "sqlCondition", tables[x].checks[y].sqlcond )
     END FOR
 
  END FOR

  CALL root.writeXml(filename)

END FUNCTION


FUNCTION schema_load(filename)
  DEFINE filename STRING
  DEFINE s STRING
  DEFINE x,y,i INTEGER
  DEFINE ddoc om.DomDocument
  DEFINE root, tnode, lnode, cnode om.DomNode
  DEFINE tlist, llist, clist om.NodeList

  IF NOT os.Path.exists(filename) THEN
     LET s = "Could not find schema '",filename,"'"
     DISPLAY "ERROR: ", s
     RETURN -1
  END IF

  LET ddoc = om.DomDocument.createFromXmlFile(filename)
  IF ddoc IS NULL THEN
     LET s = "Could not read XML file for schema '",filename,"'"
     DISPLAY "ERROR: ", s
     RETURN -2
  END IF

  LET root = ddoc.getDocumentElement()

  CALL tables.clear()

  LET hprms.schname      = root.getAttribute("name")
  LET hprms.schowner     = root.getAttribute("owner")
  LET hprms.schversion   = root.getAttribute("version")
  LET hprms.schrevision  = root.getAttribute("revision")
  LET hprms.schquotedids = root.getAttribute("quotedids")
  LET hprms.schcreat     = root.getAttribute("creation")
  LET hprms.schmodif     = root.getAttribute("modification")
  LET hprms.schcomment   = root.getAttribute("comment")

  LET tlist = root.selectByTagName("Table")
  FOR x=1 TO tlist.getLength()
      LET tnode = tlist.item(x)
      CALL tables.appendElement()
      LET tables[x].tabname  = tnode.getAttribute("name")
      LET tables[x].pkeyname = tnode.getAttribute("pKeyName")

      LET llist = tnode.selectByTagName("ColumnList")
      LET lnode = llist.item(1)
      IF lnode IS NOT NULL THEN
         LET clist = lnode.selectByTagName("Column")
         FOR y=1 TO clist.getLength()
             LET cnode = clist.item(y)
             CALL tables[x].cols.appendElement()
             LET tables[x].cols[y].colname  = cnode.getAttribute( "name" )
             LET i = dtdef_lookup_by_name(cnode.getAttribute("typeName"))
             LET tables[x].cols[y].typenum  = dtdef[i].num
             LET tables[x].cols[y].typel1   = cnode.getAttribute( "typeLen1" )  
             LET tables[x].cols[y].typel2   = cnode.getAttribute( "typeLen2" )  
             LET tables[x].cols[y].typenn   = cnode.getAttribute( "notNull" )
             LET tables[x].cols[y].pkeycol  = cnode.getAttribute( "pKeyCol" )
             LET tables[x].cols[y].defvalue = cnode.getAttribute( "defValue" )
         END FOR
      END IF

      LET llist = tnode.selectByTagName("CreateOptionList")
      LET lnode = llist.item(1)
      IF lnode IS NOT NULL THEN
         LET clist = lnode.selectByTagName("CreateOption")
         FOR y=1 TO clist.getLength()
             LET cnode = clist.item(y)
             CALL tables[x].tabopts.appendElement()
             LET tables[x].tabopts[y].dbtype  = cnode.getAttribute( "dbType" )
             LET tables[x].tabopts[y].sqltext = cnode.getAttribute( "sqlText" )
         END FOR
      END IF

      LET llist = tnode.selectByTagName("SecondaryKeyConstraintList")
      LET lnode = llist.item(1)
      IF lnode IS NOT NULL THEN
         LET clist = lnode.selectByTagName("SecondaryKeyConstraint")
         FOR y=1 TO clist.getLength()
             LET cnode = clist.item(y)
             CALL tables[x].skeys.appendElement()
             LET tables[x].skeys[y].skeyname = cnode.getAttribute( "name" )
             LET tables[x].skeys[y].skeycols = cnode.getAttribute( "columns" )
         END FOR
      END IF
   
      LET llist = tnode.selectByTagName("ForeignKeyConstraintList")
      LET lnode = llist.item(1)
      IF lnode IS NOT NULL THEN
         LET clist = lnode.selectByTagName("ForeignKeyConstraint")
         FOR y=1 TO clist.getLength()
             LET cnode = clist.item(y)
             CALL tables[x].fkeys.appendElement()
             LET tables[x].fkeys[y].fkeyname     = cnode.getAttribute( "name" )
             LET tables[x].fkeys[y].fkeycols     = cnode.getAttribute( "columns" )
             LET tables[x].fkeys[y].reftabname   = cnode.getAttribute( "refTable" )
             LET tables[x].fkeys[y].refconstname = cnode.getAttribute( "refConstraint" )
             LET tables[x].fkeys[y].delrule      = cnode.getAttribute( "deleteRule" )
             LET tables[x].fkeys[y].updrule      = cnode.getAttribute( "updateRule" )
         END FOR
      END IF

      LET llist = tnode.selectByTagName("CheckConstraintList")
      LET lnode = llist.item(1)
      IF lnode IS NOT NULL THEN
         LET clist = lnode.selectByTagName("CheckConstraint")
         FOR y=1 TO clist.getLength()
             LET cnode = clist.item(y)
             CALL tables[x].checks.appendElement()
             LET tables[x].checks[y].checkname  = cnode.getAttribute( "name" )
             LET tables[x].checks[y].sqlcond    = cnode.getAttribute( "sqlCondition" )
         END FOR
      END IF

  END FOR

  RETURN 0

END FUNCTION

FUNCTION names_match(n1, n2)
  DEFINE n1, n2 STRING
  IF hprms.schquotedids == "y" THEN
     RETURN n1.equals(n2)
  ELSE
     RETURN n1.equalsIgnoreCase(n2)
  END IF
END FUNCTION

FUNCTION table_lookup_ex(tabname, ex)
  DEFINE tabname STRING
  DEFINE ex INTEGER
  DEFINE i INTEGER
  IF tabname IS NULL THEN RETURN 0 END IF
  FOR i = 1 TO tables.getLength()
      IF names_match(tables[i].tabname, tabname) AND i != ex THEN
         RETURN i
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION table_lookup(tabname)
  DEFINE tabname STRING
  RETURN table_lookup_ex(tabname, -1)
END FUNCTION

FUNCTION column_lookup_ex(t,colname,ex)
  DEFINE t INTEGER
  DEFINE colname STRING
  DEFINE ex INTEGER
  DEFINE i INTEGER
  IF colname IS NULL THEN RETURN 0 END IF
  FOR i = 1 TO tables[t].cols.getLength()
      IF names_match(tables[t].cols[i].colname, colname) AND i != ex THEN
         RETURN i
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION column_lookup(t,colname)
  DEFINE t INTEGER
  DEFINE colname STRING
  RETURN column_lookup_ex(t,colname,-1)
END FUNCTION

FUNCTION fkeys_lookup_reftable(tabname)
  DEFINE tabname STRING
  DEFINE t,n INTEGER
  FOR t=1 TO tables.getLength()
      FOR n=1 TO tables[t].fkeys.getLength()
          IF names_match(tabname, tables[t].fkeys[n].reftabname) THEN
             RETURN t,n
          END IF
      END FOR
  END FOR
  RETURN -1,-1
END FUNCTION

FUNCTION table_get_newname(tabnum)
  DEFINE tabnum INTEGER
  RETURN SFMT(glb_params.newtabname,tabnum)
END FUNCTION

FUNCTION table_has_pkeycols(tabnum)
  DEFINE tabnum INTEGER
  DEFINE i INTEGER
  FOR i=1 TO tables[tabnum].cols.getLength()
      IF tables[tabnum].cols[i].pkeycol THEN
         RETURN TRUE
      END IF
  END FOR
  RETURN FALSE
END FUNCTION

FUNCTION table_set_firstcolpkey(tabnum)
  DEFINE tabnum INTEGER
  LET tables[tabnum].cols[1].pkeycol = TRUE
  LET tables[tabnum].cols[1].typenn = TRUE
  IF tables[tabnum].pkeyname IS NULL THEN
     LET tables[tabnum].pkeyname = constraint_get_unique_name(constype_pkey,tabnum)
  END IF
END FUNCTION

FUNCTION key_verify_colset(tabnum,colset)
  DEFINE tabnum INTEGER
  DEFINE colset STRING
  DEFINE tok base.StringTokenizer
  DEFINE i INTEGER
  IF LENGTH(colset) == 0 THEN RETURN -3 END IF
  LET tok = base.StringTokenizer.create(colset,",")
  WHILE tok.hasMoreTokens()
      LET i = column_lookup(tabnum,tok.nextToken())
      IF i == 0 THEN
         RETURN -1
      END IF
      IF NOT tables[tabnum].cols[i].typenn THEN
         RETURN -2
      END IF
  END WHILE
  RETURN 0
END FUNCTION

FUNCTION constraints_get_key_columns(tabname, constname)
  DEFINE tabname, constname STRING
  DEFINE t, n INTEGER
  LET t = table_lookup(tabname)
  IF t<=0 THEN RETURN NULL END IF
  IF constname == tables[t].pkeyname THEN
     RETURN cols_serialize_pkeycols(t)
  END IF
  FOR n=1 TO tables[t].skeys.getLength()
      IF constname == tables[t].skeys[n].skeyname THEN
         RETURN tables[t].skeys[n].skeycols
      END IF
  END FOR
  RETURN NULL
END FUNCTION

FUNCTION constraint_get_unique_name(constype,tabnum)
  DEFINE constype, tabnum INTEGER
  DEFINE fmt, name STRING
  DEFINE id,rt,rc,rp INTEGER
  CASE constype
       WHEN constype_pkey
            LET fmt = glb_params.newpkeyname
            LET id = NULL
       WHEN constype_skey
            LET fmt = glb_params.newskeyname
            LET id = tables[tabnum].skeys.getLength()
       WHEN constype_fkey
            LET fmt = glb_params.newfkeyname
            LET id = tables[tabnum].fkeys.getLength()
       WHEN constype_chck
            LET fmt = glb_params.newcheckname
            LET id = tables[tabnum].checks.getLength()
  END CASE
  WHILE TRUE
     LET name = SFMT(fmt,tables[tabnum].tabname,id)
     CALL constraint_lookup(name) RETURNING rt,rc,rp
     IF rt==0 THEN EXIT WHILE END IF
     IF id IS NULL THEN LET id = 1 ELSE LET id = id + 1 END IF
     IF fmt NOT MATCHES "*%2*" THEN
        LET fmt = fmt || "_%2"
     END IF
  END WHILE
  RETURN name
END FUNCTION

FUNCTION pkey_verify_column_usage(tabnum,pkeycols)
  DEFINE tabnum INTEGER
  DEFINE pkeycols STRING
  DEFINE i INTEGER
  FOR i=1 TO tables[tabnum].skeys.getLength()
      IF pkeycols == tables[tabnum].skeys[i].skeycols THEN
         RETURN FALSE
      END IF
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION skeys_lookup_column_usage(tabnum,colname)
  DEFINE tabnum INTEGER
  DEFINE colname STRING
  DEFINE i INTEGER
  DEFINE tok base.StringTokenizer
  FOR i=1 TO tables[tabnum].skeys.getLength()
      LET tok=base.StringTokenizer.create(tables[tabnum].skeys[i].skeycols,",")
      WHILE tok.hasMoreTokens()
          IF colname == tok.nextToken() THEN
             RETURN i
          END IF
      END WHILE
  END FOR
  RETURN 0
END FUNCTION

FUNCTION fkey_get_first_keyconst(tabname)
  DEFINE tabname STRING
  DEFINE tn INTEGER
  LET tn = table_lookup(tabname)
  IF tn > 0 THEN
     IF tables[tn].pkeyname IS NOT NULL THEN
        RETURN tables[tn].pkeyname
     END IF
     IF tables[tn].skeys.getLength() > 0 THEN
        RETURN tables[tn].skeys[1].skeyname
     END IF
  END IF
  RETURN NULL
END FUNCTION

FUNCTION fkeys_lookup_column_usage(tabnum,colname)
  DEFINE tabnum INTEGER
  DEFINE colname STRING
  DEFINE i INTEGER
  DEFINE tok base.StringTokenizer
  FOR i=1 TO tables[tabnum].fkeys.getLength()
      LET tok=base.StringTokenizer.create(tables[tabnum].fkeys[i].fkeycols,",")
      WHILE tok.hasMoreTokens()
          IF colname == tok.nextToken() THEN
             RETURN i
          END IF
      END WHILE
  END FOR
  RETURN 0
END FUNCTION

FUNCTION constraint_lookup_ex(name,ct,csk,cfk,cck)
  DEFINE name STRING
  DEFINE ct, csk, cfk, cck INTEGER
  DEFINE t, p INTEGER
  IF name IS NULL THEN RETURN 0,0,0 END IF
  FOR t = 1 TO tables.getLength()
      IF names_match(tables[t].pkeyname, name) AND t!=ct THEN
         RETURN t, constype_pkey, p
      END IF
      FOR p = 1 TO tables[t].skeys.getLength()
          IF names_match(tables[t].skeys[p].skeyname, name) AND (t!=ct OR p!=csk) THEN
             RETURN t, constype_skey, p
          END IF
      END FOR
      FOR p = 1 TO tables[t].fkeys.getLength()
          IF names_match(tables[t].fkeys[p].fkeyname, name) AND (t!=ct OR p!=cfk) THEN
             RETURN t, constype_fkey, p
          END IF
      END FOR
      FOR p = 1 TO tables[t].checks.getLength()
          IF names_match(tables[t].checks[p].checkname, name) AND (t!=ct OR p!=cck) THEN
             RETURN t, constype_chck, p
          END IF
      END FOR
  END FOR
  RETURN 0, 0, 0
END FUNCTION

FUNCTION constraint_lookup(name)
  DEFINE name STRING
  DEFINE t, c, p INTEGER
  CALL constraint_lookup_ex(name, -1, -1, -1, -1) RETURNING t,c,p
  RETURN t,c,p
END FUNCTION

FUNCTION cols_is_integer_type(tn)
  DEFINE tn INTEGER
  RETURN ( tn == dtn_integer OR tn == dtn_serial
        OR tn == dtn_bigserial OR tn == dtn_serial8 )
END FUNCTION

FUNCTION cols_verify_typematch(tab1,col1,tab2,col2)
  DEFINE tab1, col1, tab2, col2 STRING
  DEFINE it1, ic1, it2, ic2 INTEGER
  CALL tabcol_lookup(tab1,col1) RETURNING it1, ic1
  CALL tabcol_lookup(tab2,col2) RETURNING it2, ic2
  IF (it1>0 AND it2>0) THEN
     IF  cols_is_integer_type(tables[it1].cols[ic1].typenum)
     AND cols_is_integer_type(tables[it2].cols[ic2].typenum) THEN
        RETURN TRUE
     END IF
     IF tables[it1].cols[ic1].typenum == tables[it2].cols[ic2].typenum THEN
        IF ( (tables[it1].cols[ic1].typel1 IS NULL AND tables[it2].cols[ic2].typel1 IS NULL)
            OR (tables[it1].cols[ic1].typel1 == tables[it2].cols[ic2].typel1) )
        AND( (tables[it1].cols[ic1].typel2 IS NULL AND tables[it2].cols[ic2].typel2 IS NULL)
            OR (tables[it1].cols[ic1].typel2 == tables[it2].cols[ic2].typel2) ) THEN
           RETURN TRUE
        END IF
     END IF
  END IF
  RETURN FALSE
END FUNCTION

FUNCTION cols_get_deflengths(typenum)
  DEFINE typenum SMALLINT
  DEFINE l1, l2 INTEGER
  CASE
      WHEN typenum == dtn_char
           LET l1 = 32767
           LET l2 = NULL
      WHEN typenum == dtn_boolean
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_smallint
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_integer
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_bigint
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_int8
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_float
           LET l1 = 14
           LET l2 = NULL
      WHEN typenum == dtn_smallfloat
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_decimal
           LET l1 = 32
           LET l2 = 2
      WHEN typenum == dtn_serial
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_bigserial
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_serial8
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_date
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_money
           LET l1 = 32
           LET l2 = 2
      WHEN typenum == dtn_text
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_byte
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum == dtn_varchar
           LET l1 = 255
           LET l2 = NULL
      WHEN typenum >= dtn_datetime_year_to_frac5
       AND typenum <= dtn_datetime_fraction_to_frac1
           LET l1 = NULL
           LET l2 = NULL
      WHEN typenum >= dtn_interval_year_to_month
       AND typenum <= dtn_interval_fraction_to_frac1
           LET l1 = 5
           LET l2 = NULL
      WHEN typenum == dtn_nchar
           LET l1 = 32767
           LET l2 = NULL
      WHEN typenum == dtn_nvarchar
           LET l1 = 255
           LET l2 = NULL
      WHEN typenum == dtn_varchar2
           LET l1 = 32767
           LET l2 = NULL
      OTHERWISE
           DISPLAY "ERROR: Wrong typenum:", typenum
           EXIT PROGRAM 1
  END CASE
  RETURN l1, l2
END FUNCTION

FUNCTION cols_get_defitemtype(typenum)
  DEFINE typenum SMALLINT
  DEFINE it STRING
  CASE
      WHEN typenum == dtn_date
           LET it = "DateEdit"
      WHEN typenum == dtn_datetime_year_to_day
           LET it = "DateEdit"
      WHEN typenum == dtn_text
           LET it = "TextEdit"
      OTHERWISE
           LET it = "Edit"
  END CASE
  RETURN it
END FUNCTION

FUNCTION cols_verify_coltype(typenum,typel1,typel2)
  DEFINE typenum SMALLINT,
         typel1 INTEGER,
         typel2 INTEGER
  CASE
      WHEN typenum == dtn_char
           IF (typel1 < 1 OR typel1 > 32767) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_boolean
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_smallint
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_integer
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_bigint
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_int8
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_float
           IF (typel1 < 1 OR typel1 > 14) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_smallfloat
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_decimal
           IF (typel1 < 1 OR typel1 > 32) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) and
              (typel2 < 1 OR typel2 > typel1) THEN RETURN -2 END IF
      WHEN typenum == dtn_serial
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_bigserial
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_serial8
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_date
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_money
           IF (typel1 < 1 OR typel1 > 32) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) and
              (typel2 < 1 OR typel2 > typel1) THEN RETURN -2 END IF
      WHEN typenum == dtn_text
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_byte
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_varchar
           IF (typel1 < 1 OR typel1 > 255) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) and
              (typel2 < 1 OR typel2 > 255) THEN RETURN -2 END IF
      WHEN typenum >= dtn_datetime_year_to_frac5
       AND typenum <= dtn_datetime_fraction_to_frac1
           IF (typel1 IS NOT NULL) THEN RETURN 1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum >= dtn_interval_year_to_month
       AND typenum <= dtn_interval_fraction_to_frac1
           IF (typel1 < 1 OR typel1 > 9) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_nchar
           IF (typel1 < 1 OR typel1 > 32767) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      WHEN typenum == dtn_nvarchar
           IF (typel1 < 1 OR typel1 > 255) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) and
              (typel2 < 1 OR typel2 > 255) THEN RETURN -2 END IF
      WHEN typenum == dtn_varchar2
           IF (typel1 < 1 OR typel1 > 32767) THEN RETURN -1 END IF
           IF (typel2 IS NOT NULL) THEN RETURN 2 END IF
      OTHERWISE
           DISPLAY "ERROR: Wrong typenum:", typenum
           EXIT PROGRAM 1
  END CASE
  RETURN 0
END FUNCTION

FUNCTION table_has_unique_key(tabname)
  DEFINE tabname STRING
  DEFINE t, c INTEGER
  LET t = table_lookup(tabname)
  IF t>0 THEN
     IF tables[t].skeys.getLength() > 0 THEN
        RETURN TRUE
     END IF
     FOR c=1 TO tables[t].cols.getLength()
         IF tables[t].cols[c].pkeycol THEN
            RETURN TRUE
         END IF
     END FOR
  END IF
  RETURN FALSE
END FUNCTION
 
FUNCTION cols_serialize_pkeycols(tabnum)
  DEFINE tabnum INTEGER
  DEFINE c INTEGER
  DEFINE s STRING
  FOR c=1 TO tables[tabnum].cols.getLength()
      IF tables[tabnum].cols[c].pkeycol THEN
         IF s IS NOT NULL THEN LET s = s, "," END IF
         LET s = s, tables[tabnum].cols[c].colname
      END IF
  END FOR
  RETURN s
END FUNCTION

FUNCTION cols_serialize_type(tabnum,colnum)
  DEFINE tabnum, colnum INTEGER
  RETURN sql_serialize_type_ifx(tabnum,colnum)
END FUNCTION

FUNCTION pkey_set_notnull(tabnum)
  DEFINE tabnum INTEGER
  DEFINE c INTEGER
  FOR c=1 TO tables[tabnum].cols.getLength()
      IF tables[tabnum].cols[c].pkeycol THEN
         LET tables[tabnum].cols[c].typenn = TRUE
      END IF
  END FOR
END FUNCTION

FUNCTION tabopt_lookup(t,dbtype)
  DEFINE t INTEGER
  DEFINE dbtype t_dbtype
  DEFINE i INTEGER
  FOR i = 1 TO tables[t].tabopts.getLength()
      IF tables[t].tabopts[i].dbtype == dbtype THEN
         RETURN i
      END IF
  END FOR
  RETURN 0
END FUNCTION

FUNCTION tabcol_lookup(tabname,colname)
  DEFINE tabname STRING
  DEFINE colname STRING
  DEFINE i, j INTEGER
  LET i = table_lookup(tabname)
  IF i > 0 THEN
     FOR j = 1 TO tables[i].cols.getLength()
         IF names_match(tables[i].cols[j].colname, colname) THEN
            RETURN i, j
         END IF
     END FOR
  END IF
  RETURN 0, 0
END FUNCTION

FUNCTION cols_verify_defvalue_string( typel1, defvalue )
  DEFINE typel1 INTEGER
  DEFINE defvalue STRING
{ We relax to allow keywords like USER...
  IF NOT is_singlequoted(defvalue) THEN
     RETURN FALSE
  END IF
  IF defvalue.getLength()-2 > typel1 THEN
     RETURN FALSE
  END IF
}
  RETURN TRUE
END FUNCTION

FUNCTION cols_verify_defvalue_numeric( typel1, typel2, defvalue )
  DEFINE typel1, typel2 INTEGER
  DEFINE defvalue STRING
  DEFINE maxlen INTEGER
  IF NOT is_numeric(defvalue) THEN
     RETURN FALSE
  END IF
  IF typel2 IS NOT NULL THEN
     LET maxlen = typel1 + 1 + typel2
  ELSE
     LET maxlen = typel1
  END IF
  IF defvalue.getLength() > maxlen THEN
     RETURN FALSE
  END IF
  IF typel2 IS NULL AND defvalue.getIndexOf(".",1)>0 THEN
     RETURN FALSE
  END IF
  RETURN TRUE
END FUNCTION

FUNCTION cols_verify_defvalue_datetime( typenum, defvalue )
  DEFINE typenum INTEGER
  DEFINE defvalue STRING
  -- We do not accept "CURRENT x TO y", see SQL generation.
  RETURN ( defvalue == "CURRENT" )
END FUNCTION

FUNCTION cols_verify_defvalue_interval( typenum, typel1, defvalue )
  DEFINE typenum, typel1 INTEGER
  DEFINE defvalue STRING
  RETURN is_singlequoted(defvalue)
END FUNCTION

FUNCTION cols_verify_defvalue( typenum, typel1, typel2, typenn, defvalue )
  DEFINE typenum, typel1, typel2, typenn INTEGER
  DEFINE defvalue STRING
  CASE
     WHEN typenum == dtn_serial
          RETURN LENGTH(defvalue) == 0
     WHEN typenum == dtn_bigserial
          RETURN LENGTH(defvalue) == 0
     WHEN typenum == dtn_serial8
          RETURN LENGTH(defvalue) == 0
     WHEN typenum == dtn_boolean
          RETURN (defvalue=="TRUE" || defvalue=="FALSE")
     WHEN typenum == dtn_smallint
          RETURN is_integer(defvalue)
     WHEN typenum == dtn_integer
          RETURN is_integer(defvalue)
     WHEN typenum == dtn_bigint
          RETURN is_integer(defvalue)
     WHEN typenum == dtn_int8
          RETURN is_integer(defvalue)
     WHEN typenum == dtn_smallfloat
          RETURN is_numeric(defvalue)
     WHEN typenum == dtn_float
          RETURN is_numeric(defvalue)
     WHEN typenum == dtn_decimal
          RETURN cols_verify_defvalue_numeric(typel1,typel2,defvalue)
     WHEN typenum == dtn_money
          RETURN cols_verify_defvalue_numeric(typel1,typel2,defvalue)
     WHEN typenum == dtn_char
          RETURN cols_verify_defvalue_string(typel1,defvalue)
     WHEN typenum == dtn_varchar
          RETURN cols_verify_defvalue_string(typel1,defvalue)
     WHEN typenum == dtn_varchar2
          RETURN cols_verify_defvalue_string(typel1,defvalue)
     WHEN typenum == dtn_nchar -- FIXME: should use nbchars instead of length
          RETURN cols_verify_defvalue_string(typel1,defvalue)
     WHEN typenum == dtn_nvarchar -- FIXME: should use nbchars instead of length
          RETURN cols_verify_defvalue_string(typel1,defvalue)
     WHEN typenum >= dtn_datetime_year_to_frac5 
      AND typenum <= dtn_datetime_fraction_to_frac1
          RETURN cols_verify_defvalue_datetime(typenum,defvalue)
     WHEN typenum >= dtn_interval_year_to_month
      AND typenum <= dtn_interval_fraction_to_frac1
          RETURN cols_verify_defvalue_interval(typenum,typel1,defvalue)
     WHEN typenum == dtn_date
          RETURN ( defvalue=="TODAY" OR is_singlequoted(defvalue) )
     OTHERWISE
          RETURN ( defvalue=="USER" OR is_singlequoted(defvalue) )
  END CASE
  RETURN FALSE
END FUNCTION

FUNCTION name_uplow(ul, s)
  DEFINE ul CHAR(1)
  DEFINE s STRING
  IF ul == "U" THEN
     RETURN s.toUpperCase()
  ELSE
     RETURN s.toLowerCase()
  END IF
END FUNCTION

-- Converts all db object names to uppercase or lowercase
FUNCTION schema_convert_names(ul)
  DEFINE ul CHAR(1)
  DEFINE i,j INTEGER
  -- FIXME: Check first if we don't have duplicate table names when changing to upper/lower
  FOR i=1 TO tables.getLength()
      LET tables[i].tabname = name_uplow(ul, tables[i].tabname)
      LET tables[i].pkeyname = name_uplow(ul, tables[i].pkeyname)
      FOR j=1 TO tables[i].cols.getLength()
          LET tables[i].cols[j].colname = name_uplow(ul, tables[i].cols[j].colname)
      END FOR
      FOR j=1 TO tables[i].skeys.getLength()
          LET tables[i].skeys[j].skeyname = name_uplow(ul, tables[i].skeys[j].skeyname)
          LET tables[i].skeys[j].skeycols = name_uplow(ul, tables[i].skeys[j].skeycols)
      END FOR
      FOR j=1 TO tables[i].fkeys.getLength()
          LET tables[i].fkeys[j].fkeyname     = name_uplow(ul, tables[i].fkeys[j].fkeyname)
          LET tables[i].fkeys[j].fkeycols     = name_uplow(ul, tables[i].fkeys[j].fkeycols)
          LET tables[i].fkeys[j].reftabname   = name_uplow(ul, tables[i].fkeys[j].reftabname)
          LET tables[i].fkeys[j].refconstname = name_uplow(ul, tables[i].fkeys[j].refconstname)
      END FOR
      FOR j=1 TO tables[i].checks.getLength()
          LET tables[i].checks[j].checkname = name_uplow(ul, tables[i].checks[j].checkname)
          -- FIXME: Oups, can't touch check SQL text (values/colnames mixed)...
      END FOR
  END FOR
END FUNCTION

-- Returns: prob-id, tn, et, en, otn, oet, oen
--   prob-id: type of problem:
--     -1 = dup table name
--     -2 = dup pkey name
--     -3 = dup column name
--     -4 = dup skey name
--     -5 = dup fkey name
--     -6 = dup check name
--   tn = table num
--   et = element type
--      0 = column
--      1 = pkey (constype_pkey)
--      2 = skey (constype_skey)
--      3 = fkey (constype_fkey)
--      4 = check (constype_chck)
--   en = element num
--   otn= other table
--   oet= other element type (see et)
--   oen= other element num
FUNCTION schema_check_unique_names(cs)
  DEFINE cs INTEGER
  DEFINE s,i,j,x,t,c,p INTEGER
  DEFINE qids CHAR(1)
  LET qids = hprms.schquotedids
  IF NOT cs THEN
     LET hprms.schquotedids = "n" -- makes names_match() case-insensitive
  END IF
  LET s = 0
  FOR i=1 TO tables.getLength()
      LET j = 0
      LET t = table_lookup_ex(tables[i].tabname, i)
      IF t>0 THEN LET s = -1 GOTO END END IF
      IF tables[i].pkeyname IS NOT NULL THEN
         LET x = 1
         CALL constraint_lookup_ex(tables[i].pkeyname,i,-1,-1,-1) RETURNING t, c, p
      END IF
      IF t>0 THEN LET s = -2 GOTO END END IF
      FOR j=1 TO tables[i].cols.getLength()
          LET p = column_lookup_ex(i, tables[i].cols[j].colname, j)
          LET x = 0
          IF p>0 THEN LET s = -3 GOTO END END IF
      END FOR
      FOR j=1 TO tables[i].skeys.getLength()
          LET x = constype_skey
          CALL constraint_lookup_ex(tables[i].skeys[j].skeyname,i,j,-1,-1) RETURNING t, c, p
          IF p>0 THEN LET s = -4 GOTO END END IF
      END FOR
      FOR j=1 TO tables[i].fkeys.getLength()
          LET x = constype_fkey
          CALL constraint_lookup_ex(tables[i].fkeys[j].fkeyname,i,-1,j,-1) RETURNING t, c, p
          IF p>0 THEN LET s = -5 GOTO END END IF
      END FOR
      FOR j=1 TO tables[i].checks.getLength()
          LET x = constype_chck
          CALL constraint_lookup_ex(tables[i].checks[j].checkname,i,-1,-1,j) RETURNING t, c, p
          IF p>0 THEN LET s = -6 GOTO END END IF
      END FOR
  END FOR
LABEL end:
  LET hprms.schquotedids = qids
  RETURN s,i,x,j,t,c,p
END FUNCTION

FUNCTION is_identifier_1(s)
  DEFINE s STRING
  IF ( s.toUpperCase() MATCHES "[A-Z_]*" )
  AND ( s NOT MATCHES "*[ \\'\"\*!`()-+={}|:;,.<>?/]*" )
  AND ( s.getIndexOf("[",1) == 0)
  AND ( s.getIndexOf("]",1) == 0) THEN
    RETURN TRUE
  ELSE
    RETURN FALSE
  END IF
END FUNCTION

FUNCTION is_identifier_2(s)
  DEFINE s STRING
  IF NOT is_identifier_1(s) THEN
     RETURN FALSE
  END IF
  IF ( s NOT MATCHES "*[@~#$%^&]*" ) THEN
    RETURN TRUE
  ELSE
    RETURN FALSE
  END IF
END FUNCTION

FUNCTION is_integer(s)
  DEFINE s STRING
  DEFINE i INTEGER
  FOR i = 1 TO s.getLength()
      IF s.getCharAt(i) NOT MATCHES "[0-9]" THEN
         RETURN FALSE
      END IF
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION is_signed_integer(s)
  DEFINE s STRING
  DEFINE tmp STRING
  IF s.getCharAt(1) == "+" OR s.getCharAt(1) == "-" THEN
     LET tmp = s.subString(2,s.getLength())
  ELSE
     LET tmp = s
  END IF
  RETURN is_integer(tmp)
END FUNCTION

FUNCTION is_numeric(s)
  DEFINE s STRING
  DEFINE i INTEGER
  DEFINE f INTEGER
  LET f = FALSE
  FOR i = 1 TO s.getLength()
      IF s.getCharAt(i) == "." THEN
         IF f THEN
            RETURN FALSE
         ELSE
            LET f = TRUE
         END IF
      ELSE
         IF s.getCharAt(i) NOT MATCHES "[0-9]" THEN
            RETURN FALSE
         END IF
      END IF
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION is_signed_numeric(s)
  DEFINE s STRING
  DEFINE tmp STRING
  IF s.getCharAt(1) == "+" OR s.getCharAt(1) == "-" THEN
     LET tmp = s.subString(2,s.getLength())
  ELSE
     LET tmp = s
  END IF
  RETURN is_numeric(tmp)
END FUNCTION

FUNCTION is_singlequoted(s)
  DEFINE s STRING
  DEFINE i INTEGER
  IF s.getCharAt(1) != "'" OR s.getCharAt(s.getLength()) != "'"
  OR s.getLength() == 1 THEN
     RETURN FALSE
  END IF
  FOR i = 2 TO s.getLength()-1
     IF s.getCharAt(i) == "'" AND s.getCharAt(i-1) != "\\" THEN
        RETURN FALSE
     END IF
  END FOR
  RETURN TRUE
END FUNCTION

FUNCTION is_doublequoted(s)
  DEFINE s STRING
  DEFINE i INTEGER
  IF s.getCharAt(1) != '"' OR s.getCharAt(s.getLength()) != '"'
  OR s.getLength() == 1 THEN
     RETURN FALSE
  END IF
  FOR i = 2 TO s.getLength()-1
     IF s.getCharAt(i) == '"' AND s.getCharAt(i-1) != "\\" THEN
        RETURN FALSE
     END IF
  END FOR
  RETURN TRUE
END FUNCTION

