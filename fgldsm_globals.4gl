GLOBALS

CONSTANT fext_schema = ".dbs"
CONSTANT fext_dsfilt = "*.dbs"

CONSTANT udrule_default    = "?" -- Database dependant!!!
CONSTANT udrule_noaction   = "A"
CONSTANT udrule_restrict   = "R"
CONSTANT udrule_cascade    = "C"
CONSTANT udrule_setnull    = "N"
CONSTANT udrule_setdefault = "D"

TYPE t_hprms RECORD
            schname STRING,
            schowner STRING,
            schversion STRING,
            schrevision INTEGER,
            schquotedids CHAR(1),
            schcreat DATETIME YEAR TO SECOND,
            schmodif DATETIME YEAR TO SECOND,
            schcomment STRING
       END RECORD
DEFINE hprms t_hprms

CONSTANT constype_pkey = 1
CONSTANT constype_skey = 2
CONSTANT constype_fkey = 3
CONSTANT constype_chck = 4

TYPE t_glb_params RECORD
           newtabname STRING,
           newpkeyname STRING,
           newskeyname STRING,
           newfkeyname STRING,
           newcheckname STRING,
           newcolname STRING,
           newcoltp SMALLINT,
           newcolnn SMALLINT
       END RECORD
DEFINE glb_params t_glb_params

DEFINE dlg_params RECORD
           fileopendir STRING
       END RECORD

DEFINE ext_params RECORD
           dbname STRING,
           dbdriver STRING,
           username STRING,
           password STRING,
           dbowner VARCHAR(200),
           cvmeth CHAR(30),
           tabname VARCHAR(200),
           casens SMALLINT,
           systables SMALLINT,
           ignerrors SMALLINT
       END RECORD

TYPE t_dbtype CHAR(3)
DEFINE sql_params RECORD
           dbtype t_dbtype,
           filename STRING,
           namedconst INTEGER,
           createfkeys INTEGER,
           droptables INTEGER,
           quotedids INTEGER,
           upcaseids INTEGER,
           tableopts INTEGER
       END RECORD

TYPE t_column RECORD
           colindex SMALLINT,
           colname STRING,
           typenum SMALLINT,
           typel1 INTEGER, -- can be 65535
           typel2 INTEGER,
           typenn SMALLINT,
           pkeycol SMALLINT,
           defvalue STRING
     END RECORD

TYPE t_collist DYNAMIC ARRAY OF t_column

TYPE t_tabopts DYNAMIC ARRAY OF RECORD
           dbtype t_dbtype,
           sqltext STRING
     END RECORD

TYPE t_skeys DYNAMIC ARRAY OF RECORD
           skeyname STRING,
           skeycols STRING
     END RECORD

TYPE t_fkeys DYNAMIC ARRAY OF RECORD
           fkeyname STRING,
           fkeycols STRING,
           reftabname STRING,
           refconstname STRING,
           delrule CHAR(1),
           updrule CHAR(1)
     END RECORD

TYPE t_checks DYNAMIC ARRAY OF RECORD
           checkname STRING,
           sqlcond STRING
     END RECORD

TYPE t_tablist DYNAMIC ARRAY OF RECORD
           tabname STRING,
           owner STRING,
           pkeyname STRING,
           cols t_collist,
           skeys t_skeys,
           fkeys t_fkeys,
           checks t_checks,
           tabopts t_tabopts
       END RECORD

DEFINE tables t_tablist

-- Datatype ids recognized by compilers (.sch)
CONSTANT dtid_char       = 0
CONSTANT dtid_smallint   = 1
CONSTANT dtid_integer    = 2
CONSTANT dtid_float      = 3
CONSTANT dtid_smallfloat = 4
CONSTANT dtid_decimal    = 5
CONSTANT dtid_serial     = 6
CONSTANT dtid_date       = 7
CONSTANT dtid_money      = 8
-- Warning: number 9 is not used!
CONSTANT dtid_datetime   = 10
CONSTANT dtid_text       = 11
CONSTANT dtid_byte       = 12
CONSTANT dtid_varchar    = 13
CONSTANT dtid_interval   = 14
CONSTANT dtid_nchar      = 15
CONSTANT dtid_nvarchar   = 16
CONSTANT dtid_int8       = 17
CONSTANT dtid_serial8    = 18
-- gap
CONSTANT dtid_boolean    = 45
-- gap
CONSTANT dtid_bigint     = 52
CONSTANT dtid_bigserial  = 53
-- gap
CONSTANT dtid_varchar2   = 201
CONSTANT dtid_nvarchar2  = 202

-- Datetime and interval qualifiers
CONSTANT dtiqual_year     = 0
CONSTANT dtiqual_month    = 2
CONSTANT dtiqual_day      = 4
CONSTANT dtiqual_hour     = 6
CONSTANT dtiqual_minute   = 8
CONSTANT dtiqual_second   = 10
CONSTANT dtiqual_frac1    = 11
CONSTANT dtiqual_frac2    = 12
CONSTANT dtiqual_frac3    = 13
CONSTANT dtiqual_frac4    = 14
CONSTANT dtiqual_frac5    = 15
CONSTANT dtiqual_fraction = 12

-- Datatype numbers for schema manager
CONSTANT dtn_char                        = 1
CONSTANT dtn_date                        = 2
CONSTANT dtn_decimal                     = 3
CONSTANT dtn_float                       = 4
CONSTANT dtn_money                       = 5
CONSTANT dtn_smallint                    = 6
CONSTANT dtn_integer                     = 7
CONSTANT dtn_bigint                      = 8
CONSTANT dtn_boolean                     = 9
CONSTANT dtn_smallfloat                  = 10
CONSTANT dtn_serial                      = 11
CONSTANT dtn_bigserial                   = 12
CONSTANT dtn_text                        = 13
CONSTANT dtn_byte                        = 14
CONSTANT dtn_varchar                     = 15
CONSTANT dtn_nchar                       = 16
CONSTANT dtn_nvarchar                    = 17
CONSTANT dtn_int8                        = 18
CONSTANT dtn_serial8                     = 19
CONSTANT dtn_varchar2                    = 20
CONSTANT dtn_nvarchar2                   = 21
CONSTANT dtn_datetime_year_to_frac5      = 22
CONSTANT dtn_datetime_year_to_frac4      = 23
CONSTANT dtn_datetime_year_to_frac3      = 24
CONSTANT dtn_datetime_year_to_frac2      = 25
CONSTANT dtn_datetime_year_to_frac1      = 26
CONSTANT dtn_datetime_year_to_second     = 27
CONSTANT dtn_datetime_year_to_minute     = 28
CONSTANT dtn_datetime_year_to_hour       = 29
CONSTANT dtn_datetime_year_to_day        = 30
CONSTANT dtn_datetime_year_to_month      = 31
CONSTANT dtn_datetime_year_to_year       = 32
CONSTANT dtn_datetime_month_to_frac5     = 33
CONSTANT dtn_datetime_month_to_frac4     = 34
CONSTANT dtn_datetime_month_to_frac3     = 35
CONSTANT dtn_datetime_month_to_frac2     = 36
CONSTANT dtn_datetime_month_to_frac1     = 37
CONSTANT dtn_datetime_month_to_second    = 38
CONSTANT dtn_datetime_month_to_minute    = 39
CONSTANT dtn_datetime_month_to_hour      = 40
CONSTANT dtn_datetime_month_to_day       = 41
CONSTANT dtn_datetime_month_to_month     = 42
CONSTANT dtn_datetime_day_to_frac5       = 43
CONSTANT dtn_datetime_day_to_frac4       = 44
CONSTANT dtn_datetime_day_to_frac3       = 45
CONSTANT dtn_datetime_day_to_frac2       = 46
CONSTANT dtn_datetime_day_to_frac1       = 47
CONSTANT dtn_datetime_day_to_second      = 48
CONSTANT dtn_datetime_day_to_minute      = 49
CONSTANT dtn_datetime_day_to_hour        = 50
CONSTANT dtn_datetime_day_to_day         = 51
CONSTANT dtn_datetime_hour_to_frac5      = 52
CONSTANT dtn_datetime_hour_to_frac4      = 53
CONSTANT dtn_datetime_hour_to_frac3      = 54
CONSTANT dtn_datetime_hour_to_frac2      = 55
CONSTANT dtn_datetime_hour_to_frac1      = 56
CONSTANT dtn_datetime_hour_to_second     = 57
CONSTANT dtn_datetime_hour_to_minute     = 58
CONSTANT dtn_datetime_hour_to_hour       = 59
CONSTANT dtn_datetime_minute_to_frac5    = 60
CONSTANT dtn_datetime_minute_to_frac4    = 61
CONSTANT dtn_datetime_minute_to_frac3    = 62
CONSTANT dtn_datetime_minute_to_frac2    = 63
CONSTANT dtn_datetime_minute_to_frac1    = 64
CONSTANT dtn_datetime_minute_to_second   = 65
CONSTANT dtn_datetime_minute_to_minute   = 66
CONSTANT dtn_datetime_second_to_frac5    = 67
CONSTANT dtn_datetime_second_to_frac4    = 68
CONSTANT dtn_datetime_second_to_frac3    = 69
CONSTANT dtn_datetime_second_to_frac2    = 70
CONSTANT dtn_datetime_second_to_frac1    = 71
CONSTANT dtn_datetime_second_to_second   = 72
CONSTANT dtn_datetime_fraction_to_frac5  = 73
CONSTANT dtn_datetime_fraction_to_frac4  = 74
CONSTANT dtn_datetime_fraction_to_frac3  = 75
CONSTANT dtn_datetime_fraction_to_frac2  = 76
CONSTANT dtn_datetime_fraction_to_frac1  = 77
CONSTANT dtn_interval_year_to_month      = 78
CONSTANT dtn_interval_year_to_year       = 79
CONSTANT dtn_interval_month_to_month     = 80
CONSTANT dtn_interval_day_to_frac5       = 81
CONSTANT dtn_interval_day_to_frac4       = 82
CONSTANT dtn_interval_day_to_frac3       = 83
CONSTANT dtn_interval_day_to_frac2       = 84
CONSTANT dtn_interval_day_to_frac1       = 85
CONSTANT dtn_interval_day_to_second      = 86
CONSTANT dtn_interval_day_to_minute      = 87
CONSTANT dtn_interval_day_to_hour        = 88
CONSTANT dtn_interval_day_to_day         = 89
CONSTANT dtn_interval_hour_to_frac5      = 90
CONSTANT dtn_interval_hour_to_frac4      = 91
CONSTANT dtn_interval_hour_to_frac3      = 92
CONSTANT dtn_interval_hour_to_frac2      = 93
CONSTANT dtn_interval_hour_to_frac1      = 94
CONSTANT dtn_interval_hour_to_second     = 95
CONSTANT dtn_interval_hour_to_minute     = 96
CONSTANT dtn_interval_hour_to_hour       = 97
CONSTANT dtn_interval_minute_to_frac5    = 98
CONSTANT dtn_interval_minute_to_frac4    = 99
CONSTANT dtn_interval_minute_to_frac3    = 100
CONSTANT dtn_interval_minute_to_frac2    = 101
CONSTANT dtn_interval_minute_to_frac1    = 102
CONSTANT dtn_interval_minute_to_second   = 103
CONSTANT dtn_interval_minute_to_minute   = 104
CONSTANT dtn_interval_second_to_frac5    = 105
CONSTANT dtn_interval_second_to_frac4    = 106
CONSTANT dtn_interval_second_to_frac3    = 107
CONSTANT dtn_interval_second_to_frac2    = 108
CONSTANT dtn_interval_second_to_frac1    = 109
CONSTANT dtn_interval_second_to_second   = 110
CONSTANT dtn_interval_fraction_to_frac5  = 111
CONSTANT dtn_interval_fraction_to_frac4  = 112
CONSTANT dtn_interval_fraction_to_frac3  = 113
CONSTANT dtn_interval_fraction_to_frac2  = 114
CONSTANT dtn_interval_fraction_to_frac1  = 115

DEFINE dtdef DYNAMIC ARRAY OF RECORD
                 num SMALLINT,
                 name STRING,
                 typeid SMALLINT,
                 qcode SMALLINT,
                 qual1 SMALLINT,
                 qual2 SMALLINT,
                 psize SMALLINT
             END RECORD

DEFINE itdef DYNAMIC ARRAY OF RECORD
                 name STRING,
                 attrs DYNAMIC ARRAY OF INTEGER
             END RECORD

END GLOBALS

