#+ Basic utility functions
#+ Provided in the utility library.
#+

PRIVATE DEFINE currlog INTEGER
PRIVATE DEFINE logbuff DYNAMIC ARRAY OF RECORD
           log DYNAMIC ARRAY OF STRING
       END RECORD

--------------------------------------------------------------------------------

#+ Checks if a command line option is used.
#+ @returnType Integer
#+ @return TRUE if the option is used.
#+ @param optname The option, without - or / prefix.
PUBLIC FUNCTION fglt_cmdarg_option_used(optname)
    DEFINE optname STRING -- The option name without '-' or '/'
    RETURN ( fglt_cmdarg_option_index(optname)>0 )
END FUNCTION

#+ Returns the parameter of a command line option.
#+ @returnType String
#+ @return The value following the option (-option value).
#+ @param optname The option, without - or / prefix.
PUBLIC FUNCTION fglt_cmdarg_option_param(optname)
    DEFINE optname STRING -- The option name without '-' or '/'
    DEFINE optidx INTEGER
    DEFINE paramval STRING -- Can be a directory path !
    LET paramval = NULL
    LET optidx = fglt_cmdarg_option_index(optname)
    IF (optidx>0) AND (optidx<num_args()) THEN
       LET paramval = arg_val(optidx+1)
    END IF
    RETURN paramval
END FUNCTION

#+ Returns the position of a command line option.
#+ @returnType Integer
#+ @return The position of the option in the argument list.
#+ @param optname The option, without - or / prefix.
PUBLIC FUNCTION fglt_cmdarg_option_index(optname)
    DEFINE optname STRING -- The option name without '-' or '/'
    DEFINE idx, cnt, optidx INTEGER
    DEFINE fopt1, fopt2 STRING
    LET optidx = -1
    LET cnt = num_args()
    LET fopt1 = "-", optname -- UNIX convention
    LET fopt2 = "/", optname -- Windows/DOS convention
    FOR idx=1 TO cnt
        IF (arg_val(idx)=fopt1) OR (arg_val(idx)=fopt2) THEN 
           LET optidx = idx
           EXIT FOR
        END IF
    END FOR
    RETURN optidx
END FUNCTION

PUBLIC FUNCTION fglt_cmdarg_option_isopt(name)
    DEFINE name STRING
    IF fgl_getenv("WINDIR") IS NULL THEN
       IF name.getCharAt(1) == "-" THEN RETURN TRUE END IF
    ELSE
       IF name.getCharAt(1) == "/" THEN RETURN TRUE END IF
    END IF
    RETURN FALSE
END FUNCTION

#+ Checks if the command line arguments conform to the syntax passed as parameter.
#+ @returnType Integer
#+ @return The position of the option which does not match, zero if ok.
#+ @param optlist The syntax specification (ex: "(v|x|cv|of+|*)")
#+                Using opt+ = option has parameter (next arg must not use -)
#+                Using a star = simple string is allowed as argument
PUBLIC FUNCTION fglt_cmdarg_option_check(optlist)
    DEFINE optlist STRING -- The possible options (v|x|cv|of+|*)
    DEFINE optspec STRING
    DEFINE optpara STRING
    DEFINE i, j INTEGER
    DEFINE indiv, found INTEGER
    DEFINE tok base.StringTokenizer
    DEFINE optarr DYNAMIC ARRAY OF RECORD
                      optname STRING,
                      hasparam INTEGER
                  END RECORD
    LET tok = base.StringTokenizer.create(optlist, "|")
    WHILE tok.hasMoreTokens()
       LET optspec = tok.nextToken()
       IF optspec = "*" THEN
          LET indiv = TRUE
          CONTINUE WHILE
       END IF
       LET i = optspec.getIndexOf("+",1)
       CALL optarr.appendElement()
       IF i > 0 THEN
          LET optarr[optarr.getLength()].hasparam = 1
          LET optspec = optspec.subString(1,i-1)
       END IF
       LET optarr[optarr.getLength()].optname = optspec
    END WHILE
    FOR i=1 TO num_args()
        LET optspec = arg_val(i)
        IF NOT fglt_cmdarg_option_isopt(optspec) THEN
           IF indiv THEN CONTINUE FOR ELSE RETURN i END IF
        END IF
        LET optspec = optspec.subString(2,optspec.getLength())
        LET found = FALSE
        FOR j=1 TO optarr.getLength()
            IF optarr[j].optname == optspec THEN
               LET found = TRUE
               LET optpara = arg_val(i+1)
               IF optarr[j].hasparam THEN
                  IF optpara IS NULL OR fglt_cmdarg_option_isopt(optpara) THEN
                     LET found = FALSE
                  ELSE
                     LET i = i + 1
                  END IF
               ELSE
                  IF optpara IS NOT NULL AND NOT fglt_cmdarg_option_isopt(optpara) THEN
                     LET found = FALSE
                  END IF
               END IF
               EXIT FOR
            END IF
        END FOR
        IF NOT found THEN RETURN i END IF
    END FOR
    RETURN 0
END FUNCTION

--------------------------------------------------------------------------------

#+ Starts a log session.
#+ A specific log is created for each call of this function.
PUBLIC FUNCTION fglt_log_open()
  IF currlog IS NULL THEN LET currlog = 0 END IF
  LET currlog = currlog + 1
  CALL logbuff[currlog].log.clear()
END FUNCTION

#+ Writes a new line to the current log.
#+ @param thetext The line to be added to the log.
PUBLIC FUNCTION fglt_log_write(thetext)
  DEFINE thetext STRING
  DEFINE l INTEGER
  CALL logbuff[currlog].log.appendElement()
  LET l = logbuff[currlog].log.getLength()
  LET logbuff[currlog].log[l] = thetext
END FUNCTION

#+ Shows all lines of the current log.
PUBLIC FUNCTION fglt_log_show()
  DEFINE i INTEGER
  START REPORT __log_report TO SCREEN
        WITH PAGE LENGTH = 40,
             LEFT MARGIN = 0,
             RIGHT MARGIN = 200,
             TOP MARGIN = 0,
             BOTTOM margin = 0
  FOR i=1 TO logbuff[currlog].log.getLength()
    OUTPUT TO REPORT __log_report(logbuff[currlog].log[i])
  END FOR
  FINISH REPORT __log_report
  LET int_flag=FALSE
END FUNCTION

#+ Writes log lines into a file
PUBLIC FUNCTION fglt_log_save(filename)
  DEFINE filename STRING
  DEFINE ch base.Channel
  DEFINE i INTEGER
  LET ch = base.Channel.create()
  CALL ch.setDelimiter(NULL)
  CALL ch.openFile(filename,"w")
  FOR i=1 TO logbuff[currlog].log.getLength()
    CALL ch.writeLine(logbuff[currlog].log[i])
  END FOR
  CALL ch.close()
END FUNCTION

#+ Terminates the current log.
PUBLIC FUNCTION fglt_log_close()
  CALL logbuff[currlog].log.clear()
  LET currlog = currlog - 1
END FUNCTION

PRIVATE REPORT __log_report(lt)
  DEFINE lt STRING
  FORMAT
    ON EVERY ROW
       PRINT lt
END REPORT

