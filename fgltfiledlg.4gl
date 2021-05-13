#+ File open/save dialog box
#+ Provided in the utility library.
#+

IMPORT os
IMPORT FGL fgldialog
#IMPORT FGL fgltbasics
IMPORT FGL fgltdialogs

PRIVATE DEFINE dirlist DYNAMIC ARRAY OF RECORD
                    name STRING,
                    parent, abspath STRING,
                    children BOOLEAN,
                    expanded BOOLEAN
                END RECORD

PRIVATE DEFINE filelist DYNAMIC ARRAY OF RECORD
                    eimage STRING, -- Used to detect the type!!!
                    entry STRING, -- Filename or Dirname
                    esize STRING,
                    emodt STRING,
                    etype STRING -- "Directory" or "*.xxx File"
                END RECORD

PRIVATE DEFINE last_opendlg_directory STRING
PRIVATE DEFINE last_savedlg_directory STRING

PRIVATE FUNCTION __file_has_option(opt,optionlist)
  DEFINE opt, optionlist STRING
  DEFINE tok base.StringTokenizer
  LET tok = base.StringTokenizer.create(optionlist,",")
  WHILE tok.hasMoreTokens()
    IF tok.nextToken() == opt THEN RETURN TRUE END IF
  END WHILE
  RETURN FALSE
END FUNCTION

#+ Opens a file dialog to open a file.
#+ @returnType String
#+ @return The selected file path, or NULL is canceled.
#+ @param title The title of the file dialog
#+ @param defaultpath Default path to be selected
#+ @param defaultfile Default file to be displayed
#+ @param typelist Pipe separated list of extensions (*|*.*|*.4gl|*.ext|DIR)
#+ @param options Comma separated list of options (see below)
#
#+ Use DIR in the typelist to select a directory instead of a file.
#
#+ options is a comma separated list of options:
#     cd = can create directory
#     dd = can delete directory
#     df = can delete file
#     sh = can hide/show .* files
#     oe = confirm overwrite

PUBLIC FUNCTION fglt_file_opendlg(title,defaultpath,defaultfile,typelist,options)
  DEFINE title STRING
  DEFINE defaultpath STRING
  DEFINE defaultfile STRING
  DEFINE typelist STRING
  DEFINE options STRING
  DEFINE t, fn STRING
  IF defaultpath IS NULL THEN
     IF last_opendlg_directory IS NULL THEN
        LET last_opendlg_directory = os.Path.homeDir()
     END IF
     LET defaultpath = last_opendlg_directory
  END IF
  IF title IS NOT NULL THEN
     LET t = title
  ELSE
     IF typelist = "DIR" THEN
        LET t = "Open directory"
     ELSE
        LET t = "Open file"
     END IF
  END IF
  LET fn = __file_pathdlg("open",t,defaultpath,defaultfile,typelist,options)
  IF fn IS NOT NULL THEN
     LET last_opendlg_directory = os.Path.dirName(fn)
  END IF
  RETURN fn
END FUNCTION

#+ Opens a file dialog to save a file.
#+ @returnType String
#+ @return The selected file path, or NULL is canceled.
#+ @param title The title of the file dialog
#+ @param defaultpath Default path to be selected
#+ @param defaultfile Default file to be displayed
#+ @param typelist Pipe separated list of extensions (*|*.*|*.4gl|*.ext|DIR)
#+ @param options Comma separated list of options (see below)
#
#+ options is a comma separated list of options:
#     cd = can create directory
#     dd = can delete directory
#     df = can delete file
#     sh = can hide/show .* files
#     oe = confirm overwrite

PUBLIC FUNCTION fglt_file_savedlg(title,defaultpath,defaultfile,typelist,options)
  DEFINE title STRING
  DEFINE defaultpath STRING
  DEFINE defaultfile STRING
  DEFINE typelist STRING
  DEFINE options STRING
  DEFINE t, fn STRING

  IF defaultpath IS NULL THEN
     IF last_savedlg_directory IS NULL THEN
        LET last_savedlg_directory = os.Path.homeDir()
     END IF
     LET defaultpath = last_savedlg_directory
  END IF
  IF title IS NOT NULL THEN
     LET t = title
  ELSE
     LET t = "Save file"
  END IF
  LET fn = __file_pathdlg("save",t,defaultpath,defaultfile,typelist,options)
  IF fn IS NOT NULL THEN
     LET last_savedlg_directory = os.Path.dirName(fn)
  END IF
  RETURN fn
END FUNCTION

--------------------------------------------------------------------------------

PRIVATE FUNCTION __file_pathdlg_setup(d,dlgtype,currpath,filename,typelist,options)
  DEFINE d ui.Dialog
  DEFINE dlgtype, currpath, filename, typelist, options STRING
  DEFINE cr, isFolder, isValid, isFile INTEGER
  DEFINE filepath STRING
  DEFINE f ui.Form

  IF filelist.getLength()>0 THEN
     LET cr = d.getCurrentRow("sr2")
     IF dlgtype=="open" THEN
        LET filepath = os.Path.join(currpath,filelist[cr].entry)
     ELSE
        LET filepath = os.Path.join(currpath,filename)
     END IF
     LET isFolder = (filelist[cr].eimage == "folder" AND filelist[cr].entry != "..")
     LET isFile = (filelist[cr].eimage != "folder")
     LET isValid = __file_checktypeandext(typelist,filepath)
  ELSE
     LET cr = 0
     LET filepath = NULL
     LET isFolder = FALSE
     LET isValid = FALSE
  END IF

  CALL d.setActionActive("opensave", isValid)
  CALL d.setActionActive("create_dir",  __file_has_option("cd",options))
  CALL d.setActionActive("delete_dir",  __file_has_option("dd",options) AND isFolder)
  CALL d.setActionActive("delete_file", __file_has_option("df",options) AND isFile)

  LET f = d.getForm()

  CALL f.setElementHidden("create_dir",  NOT __file_has_option("cd",options))
  CALL f.setElementHidden("delete_dir",  NOT __file_has_option("dd",options))
  CALL f.setElementHidden("delete_file", NOT __file_has_option("df",options))

END FUNCTION

PRIVATE FUNCTION __file_lookup_entry(d, filename)
  DEFINE d ui.Dialog
  DEFINE filename STRING
  DEFINE i INTEGER
  FOR i=1 TO filelist.getLength()
      IF filelist[i].entry == filename THEN
         CALL d.setCurrentRow("sr2",i)
      END IF
  END FOR
END FUNCTION

PRIVATE FUNCTION __file_showdir(d, currpath, typelist, showhidden)
  DEFINE d ui.Dialog
  DEFINE currpath, typelist STRING
  DEFINE showhidden BOOLEAN
  DEFINE cur, cnt INT
  LET cur = __file_initdirs(currpath)
  IF cur <= 0 THEN RETURN FALSE END IF
  CALL d.setCurrentRow("sr1",cur)
  LET cnt = __file_getfiles(currpath,typelist,showhidden)
  CALL d.setCurrentRow("sr2",1)
  RETURN TRUE
END FUNCTION

PRIVATE FUNCTION __file_pathdlg(dlgtype,title,defaultpath,defaultfile,typelist,options)
  DEFINE dlgtype STRING
  DEFINE title STRING
  DEFINE defaultpath STRING
  DEFINE defaultfile STRING
  DEFINE typelist STRING
  DEFINE options STRING
  DEFINE filepath STRING
  DEFINE cur,cnt,cr INTEGER
  DEFINE currpath STRING
  DEFINE filename STRING
  DEFINE showhidden BOOLEAN
  DEFINE dirname CHAR(40)
  DEFINE tmpstr STRING
  DEFINE w ui.Window
  DEFINE f ui.Form
  DEFINE x INT, r BOOLEAN

  OPEN WINDOW __filedialog WITH FORM "fgltfileos"
       ATTRIBUTES(STYLE='dialog3',TEXT=title)

  LET w = ui.Window.getCurrent()
  LET f = w.getForm()
  CALL f.setElementHidden("gfn",(dlgtype=="open"))
  CALL f.setElementText("gfn","Filename ("||typelist||")")
  IF dlgtype=="open" THEN
     CALL f.setElementText("opensave","Open")
  ELSE
     CALL f.setElementText("opensave","Save")
  END IF
  CALL f.setFieldHidden("showhidden", NOT __file_has_option("sh",options))

  LET currpath = defaultpath
  LET filename = defaultfile
  LET showhidden = __file_has_option("sh",options)

  LET cur = __file_initdirs(currpath)
  IF cur <= 0 THEN RETURN NULL END IF
  LET currpath = dirlist[cur].abspath
  LET cnt = __file_getfiles(currpath,typelist,showhidden)

  CALL ui.Dialog.setDefaultUnbuffered(TRUE)

  DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)

    INPUT BY NAME currpath, filename, showhidden ATTRIBUTES(WITHOUT DEFAULTS)

       AFTER FIELD currpath
          IF DIALOG.getFieldTouched("currpath") THEN
             IF NOT __file_showdir(DIALOG,currpath,typelist,showhidden) THEN
                CALL __file_mbox_ok(title, "You must enter a valid directory name", "stop")
                NEXT FIELD currpath
             END IF
          END IF
          CALL DIALOG.setFieldTouched("currpath",FALSE)
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)

       AFTER FIELD filename
          IF filename IS NOT NULL AND filename!=""
             AND NOT __file_checktypeandext(typelist,filename) THEN
             CALL __file_mbox_ok(title,
                              "You must enter a valid file name",
                              "stop")
             NEXT FIELD filename
          END IF

       ON CHANGE showhidden
          LET x = __file_showdir(DIALOG,currpath,typelist,showhidden)
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)

    END INPUT

    DISPLAY ARRAY dirlist TO sr1.*
       BEFORE ROW 
          LET cr = DIALOG.getCurrentRow("sr1")
          LET currpath = dirlist[cr].abspath
          LET cnt = __file_getfiles(currpath,typelist,showhidden)
          CALL DIALOG.setCurrentRow("sr2",1)
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
       ON EXPAND (x)
          LET r = __file_getdirs(x)
          LET currpath = dirlist[x].abspath
          LET cnt = __file_getfiles(currpath,typelist,showhidden)
          CALL DIALOG.setCurrentRow("sr2",1)
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
       ON COLLAPSE (x)
          CALL __file_deldirs(x)
          LET currpath = dirlist[x].abspath
          LET cnt = __file_getfiles(currpath,typelist,showhidden)
          CALL DIALOG.setCurrentRow("sr2",1)
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
    END DISPLAY

    DISPLAY ARRAY filelist TO sr2.*
       BEFORE ROW 
          LET cr = DIALOG.getCurrentRow("sr2")
          LET tmpstr = os.Path.join(currpath,filelist[cr].entry)
          IF __file_checktypeandext(typelist,tmpstr) THEN
             LET filename = filelist[cr].entry
          END IF
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
    END DISPLAY

    BEFORE DIALOG
       CALL DIALOG.setCurrentRow("sr1", cur)
       CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)

    ON ACTION create_dir
       LET dirname = fglt_prompt(title,
                                 "To create the new directory, enter the directory name",
                                 "Directory name:",
                                 NULL,NULL)
       IF dirname IS NOT NULL THEN
          LET tmpstr = os.Path.join(currpath,dirname CLIPPED)
          IF os.Path.mkdir(tmpstr) THEN
             LET currpath = tmpstr
             LET x = __file_showdir(DIALOG,currpath,typelist,showhidden)
             --CALL __file_lookup_entry(DIALOG, dirname)
          ELSE
             CALL __file_mbox_ok(title, "The directory could not be created", "stop")
          END IF
       END IF

    ON ACTION delete_dir
       LET cr = DIALOG.getCurrentRow("sr2")
       LET filepath = os.Path.join(currpath,filelist[cr].entry)
       IF os.Path.type(filepath) != "directory" OR filelist[cr].entry == ".." THEN
          CONTINUE DIALOG
       END IF
       LET tmpstr = SFMT("Are you sure you want to delete directory (%1)",filelist[cr].entry)
       IF NOT __file_mbox_yn(title, tmpstr, "question") THEN
          CONTINUE DIALOG
       END IF
       IF os.Path.delete(filepath) THEN
          LET cr = DIALOG.getCurrentRow("sr2")
          LET cnt = __file_getfiles(currpath,typelist,showhidden)
          IF cr>cnt THEN CALL DIALOG.setCurrentRow("sr2",cnt) END IF
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
       ELSE
          CALL __file_mbox_ok(title, "The directory could not be deleted", "stop")
       END IF

    ON ACTION delete_file
       LET cr = DIALOG.getCurrentRow("sr2")
       LET filepath = os.Path.join(currpath,filelist[cr].entry)
       IF os.Path.type(filepath) == "directory" THEN
          CONTINUE DIALOG
       END IF
       LET tmpstr = SFMT("Are you sure you want to remove file (%1)?",filelist[cr].entry)
       IF NOT __file_mbox_yn(title, tmpstr, "question") THEN
          CONTINUE DIALOG
       END IF
       IF os.Path.delete(filepath) THEN
          LET cr = DIALOG.getCurrentRow("sr2")
          LET cnt = __file_getfiles(currpath,typelist,showhidden)
          IF DIALOG.getCurrentRow("sr2") > cnt THEN
             CALL DIALOG.setCurrentRow("sr2",cnt)
          END IF
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
       ELSE
          CALL __file_mbox_ok(title, "The file could not be deleted", "stop")
       END IF

    ON ACTION cancel
       LET filepath = NULL
       EXIT DIALOG

    ON ACTION accept
       -- Enter in path field
       IF INFIELD(currpath) THEN
          IF NOT __file_showdir(DIALOG,currpath,typelist,showhidden) THEN
             CALL __file_mbox_ok(title, "You must enter a valid directory name", "stop")
             NEXT FIELD currpath
          END IF
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
          CONTINUE DIALOG
       END IF
       -- Enter in filename field
       IF INFIELD(filename) THEN
          IF filename IS NULL OR filename=="" THEN
             CONTINUE DIALOG
          END IF
          IF NOT __file_checktypeandext(typelist,filename) THEN
             CALL __file_mbox_ok(title, "Invalid file name specified", "stop")
             NEXT FIELD filename
          ELSE
             LET filepath = __file_check_selection(DIALOG, dlgtype, title, currpath, filename, typelist, options)
             IF filepath IS NULL THEN CONTINUE DIALOG ELSE EXIT DIALOG END IF
          END IF
       END IF

    ON ACTION select
       LET cr = DIALOG.getCurrentRow("sr2")
       IF filelist[cr].eimage = "folder" THEN
          IF filelist[cr].entry = ".." THEN
             LET currpath = os.Path.dirName(currpath)
          ELSE
             LET currpath = os.Path.join(currpath,filelist[cr].entry)
          END IF
          LET x = __file_showdir(DIALOG,currpath,typelist,showhidden)
          CALL __file_pathdlg_setup(DIALOG,dlgtype,currpath,filename,typelist,options)
       ELSE
          LET filepath = __file_check_selection(DIALOG, dlgtype, title, currpath, filename, typelist, options)
          IF filepath IS NOT NULL THEN EXIT DIALOG END IF
       END IF

    ON ACTION opensave
       LET filepath = __file_check_selection(DIALOG, dlgtype, title, currpath, filename, typelist, options)
       IF filepath IS NOT NULL THEN EXIT DIALOG END IF

  END DIALOG

  CLOSE WINDOW __filedialog

  RETURN filepath
END FUNCTION

{
PRIVATE FUNCTION __file_lookupdir(path)
  DEFINE path STRING
  DEFINE i INT
  FOR i=1 TO dirlist.getLength()
    IF dirlist[i].abspath = path THEN RETURN i END IF
  END FOR
  RETURN 0 
END FUNCTION
}

PRIVATE FUNCTION __file_check_selection(d, dlgtype, title, currpath, filename, typelist, options)
  DEFINE d ui.Dialog
  DEFINE dlgtype, title, currpath, filename, typelist, options STRING
  DEFINE filepath STRING
  DEFINE cr INTEGER

  LET cr = d.getCurrentRow("sr2")
  IF cr == 0 THEN RETURN NULL END IF

  IF dlgtype=="open" THEN
     LET filepath = os.Path.join(currpath,filelist[cr].entry)
  ELSE
     LET filepath = os.Path.join(currpath,filename)
  END IF
  IF NOT __file_checktypeandext(typelist,filepath) THEN
     CALL __file_mbox_ok(title, SFMT("The selection must be one of (%1)",typelist), "stop")
     RETURN NULL
  END IF
  IF dlgtype=="save" THEN
     IF os.Path.exists(filepath) THEN
        IF __file_has_option("oe",options) THEN
           IF NOT __file_mbox_yn(title, "The selected file exists, do you want to overwrite it anyway?", "question") THEN
              RETURN NULL
           ELSE
              RETURN filepath
           END IF
        ELSE
           CALL __file_mbox_ok(title, "You are not allowed to overwrite existing files", "stop")
           RETURN NULL
        END IF
     END IF
  END IF

  RETURN filepath

END FUNCTION
 
PRIVATE FUNCTION __file_intypelist(typelist,type)
  DEFINE typelist STRING
  DEFINE type STRING
  DEFINE st base.StringTokenizer
  LET st = base.StringTokenizer.create(typelist,"|")
  WHILE st.hasMoreTokens()
    IF st.nextToken()==type THEN RETURN TRUE END IF
  END WHILE
  RETURN FALSE
END FUNCTION

PRIVATE FUNCTION __file_dirWithChildren(dirpath)
  DEFINE dirpath STRING
  DEFINE dh INTEGER
  DEFINE found BOOLEAN
  DEFINE fname, pname STRING
  LET dh = os.Path.dirOpen(dirpath)
  LET found = FALSE
  WHILE TRUE
      LET fname = os.Path.dirNext(dh)
      IF fname IS NULL THEN EXIT WHILE END IF
      IF fname == "." OR fname == ".." THEN
         CONTINUE WHILE
      END IF
      LET pname = os.Path.join(dirpath, fname)
      IF os.Path.type(pname) == "directory" THEN
         LET found = TRUE
         EXIT WHILE
      END IF
  END WHILE
  CALL os.Path.dirClose(dh)
  RETURN found
END FUNCTION

PRIVATE FUNCTION __file_initdirs(dirpath)
  DEFINE dirpath STRING
  DEFINE path STRING
  DEFINE arr DYNAMIC ARRAY OF STRING
  DEFINE i, x, c INT
  IF dirpath IS NULL THEN RETURN -1 END IF
  LET path = dirpath
  IF NOT os.Path.exists(path) THEN
     RETURN -2
  END IF
  IF os.Path.type(path) != "directory" THEN
     LET path = os.Path.dirName(path)
  END IF
  WHILE path IS NOT NULL
    CALL arr.insertElement(1)
    LET arr[1] = path
    LET path = os.Path.dirName(path)
    IF path = os.Path.dirName(path) THEN EXIT WHILE END IF
  END WHILE
  CALL dirlist.clear()
  LET path = arr[1]
  CALL dirlist.appendElement()
  IF fgl_getenv("WINDIR") THEN
     LET dirlist[1].name = path
  ELSE
     LET dirlist[1].name = os.Path.baseName(path)
  END IF
  LET dirlist[1].parent = NULL
  LET dirlist[1].abspath = path
  LET dirlist[1].children = __file_dirWithChildren(path)
  LET dirlist[1].expanded = TRUE
  LET x = 1
  FOR c = 1 TO arr.getLength() - 1
    LET dirlist[x].expanded = TRUE
    IF __file_getdirs(x) <= 0 THEN EXIT FOR END IF
    FOR i = x TO dirlist.getLength()
      IF dirlist[i].abspath == arr[c+1] THEN LET x=i EXIT FOR END IF
    END FOR
  END FOR
  RETURN x
END FUNCTION

-- ON EXPAND
PRIVATE FUNCTION __file_getdirs(index)
  DEFINE index INT
  DEFINE dh, cnt, pos INTEGER
  DEFINE dirpath, fname, pname STRING
  IF index<=0 OR dirlist.getLength()==0 THEN RETURN -2 END IF
  LET dirpath = dirlist[index].abspath
  LET dh = os.Path.dirOpen(dirpath)
  IF dh == 0 THEN RETURN -1 END IF
  LET cnt = 0
  WHILE TRUE
      LET fname = os.Path.dirNext(dh)
      IF fname IS NULL THEN EXIT WHILE END IF
      -- Skip current dir (.), parent dir (..), hidden files (.xx)
      IF fname.subString(1,1) == "." THEN
         CONTINUE WHILE
      END IF
      LET pname = os.Path.join(dirpath, fname)
      -- Skip regular files
      IF os.Path.type(pname) != "directory" THEN
         CONTINUE WHILE
      END IF
      LET cnt = cnt + 1
      LET pos = index + cnt 
      IF pos > dirlist.getLength() THEN
         CALL dirlist.appendElement()
      ELSE
         CALL dirlist.insertElement(pos)
      END IF
      LET dirlist[pos].name = fname
      LET dirlist[pos].parent = dirpath
      LET dirlist[pos].abspath = pname
      LET dirlist[pos].children = __file_dirWithChildren(pname)
      LET dirlist[pos].expanded = FALSE
  END WHILE
  CALL os.Path.dirClose(dh)
  RETURN cnt
END FUNCTION

-- ON COLLAPSE
PRIVATE FUNCTION __file_deldirs(index)
  DEFINE index INT
  DEFINE parent STRING
  LET parent = dirlist[index].abspath
  WHILE index < dirlist.getLength()
    IF dirlist[index+1].parent != parent THEN EXIT WHILE END IF
    CALL __file_deldirs(index+1)
    CALL dirlist.deleteElement(index+1)
  END WHILE
END FUNCTION

PRIVATE FUNCTION __file_getfiles(dirpath,typelist,showhidden)
  DEFINE dirpath, typelist STRING, showhidden BOOLEAN
  DEFINE dh, ns INTEGER
  DEFINE fname, pname, size, type, image, ext STRING
  CALL filelist.clear()
  CALL os.Path.dirSort("extension",1) --too slow? by type it's very slow.
  LET dh = os.Path.dirOpen(dirpath)
  IF dh == 0 THEN RETURN -1 END IF
  WHILE TRUE
      LET fname = os.Path.dirNext(dh)
      IF fname IS NULL THEN EXIT WHILE END IF
      LET pname = os.Path.join(dirpath, fname)
      IF fname == "." THEN
         CONTINUE WHILE
      END IF
      IF NOT showhidden THEN
         IF fname.getCharAt(1) == "." AND fname.getCharAt(2) != "." THEN 
            CONTINUE WHILE
         END IF
      END IF
      IF os.Path.type(pname) != "directory"
         AND NOT __file_checktypeandext(typelist,pname) THEN
            CONTINUE WHILE
      END IF
      LET ext = os.Path.extension(pname)
      CASE os.Path.type(pname)
           WHEN "directory"
                LET image = "folder"
                LET type = "Directory"
           WHEN "file"
                LET image = "file"
                IF ext.getLength()==0 THEN
                   LET ext = "???"
                ELSE
                   LET ext = ext.toUpperCase()
                END IF
                LET type = SFMT("%1 file",ext)
      END CASE
      IF os.Path.type(pname) != "file" THEN
         LET size = NULL
      ELSE
         LET ns = os.Path.size(pname)
         LET size = ns 
         IF ns>1024 THEN
            LET ns = ns/1024
            LET size = ns||" Kb"
            IF ns>1024 THEN
               LET ns = ns/1024
               LET size = ns||" Mb"
               IF ns>1024 THEN
                  LET ns = ns/1024
                  LET size = ns||" Gb"
               END IF
            END IF
         END IF
      END IF
      CALL filelist.appendElement()
      LET filelist[filelist.getLength()].entry = fname
      LET filelist[filelist.getLength()].etype = type
      LET filelist[filelist.getLength()].eimage = image
      LET filelist[filelist.getLength()].esize = size
      LET filelist[filelist.getLength()].emodt = os.Path.mtime(pname)
  END WHILE
  CALL os.Path.dirClose(dh)
  RETURN filelist.getLength()
END FUNCTION

PRIVATE FUNCTION __file_checktypeandext(typelist,fname)
  DEFINE typelist, fname STRING
  DEFINE e STRING
  IF os.Path.baseName(fname)==".." THEN
     RETURN FALSE
  END IF
  IF __file_intypelist(typelist,"*") THEN
     RETURN TRUE
  END IF
  IF __file_intypelist(typelist,"DIR") AND os.Path.type(fname) == "directory" THEN
     RETURN TRUE
  END IF
  LET e = os.Path.extension(fname)
  IF e IS NOT NULL THEN
     IF __file_intypelist(typelist,"*.*") THEN
        RETURN TRUE
     END IF
     IF __file_intypelist(typelist,"*."||e) THEN
        RETURN TRUE
     END IF
  END IF
  RETURN FALSE
END FUNCTION

PRIVATE FUNCTION __file_mbox_ok(title,message,icon)
  DEFINE title, message, icon STRING
  CALL fgl_winMessage(title,message,icon)
END FUNCTION

PRIVATE FUNCTION __file_mbox_yn(title,message,icon)
  DEFINE title, message, icon STRING
  DEFINE r STRING
  LET r = fgl_winQuestion(title,message,"yes","yes|no",icon,0)
  RETURN ( r == "yes" )
END FUNCTION

