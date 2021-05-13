#+ Typical dialog boxes
#+ Provided in the utility library.
#+

--------------------------------------------------------------------------------

#+ Starts a wait dialog session.
#+ @param title The title of the window.
#+ The wait dialog box gets a 'interrupt' button that can be pressed by the
# user to interrupt the process.
PUBLIC FUNCTION fglt_wait_open(title)
  DEFINE title STRING
  OPEN WINDOW __fgltwait WITH FORM "fgltwait"
              ATTRIBUTES(STYLE="dialog2",TEXT=title)
END FUNCTION

#+ Shows the wait dialog box.
#+ @param comment The comment to be displayed.
PUBLIC FUNCTION fglt_wait_show(comment)
  DEFINE comment STRING
  CURRENT WINDOW IS __fgltwait
  DISPLAY BY NAME comment
  CALL ui.Interface.refresh()
END FUNCTION

#+ Terminates a wait dialog session.
PUBLIC FUNCTION fglt_wait_close()
  CLOSE WINDOW __fgltwait
END FUNCTION

--------------------------------------------------------------------------------

#+ Starts a progress dialog session.
#+ @param title The title of the progress window.
#+ @param comment The comment to be displayed in the progress window.
#+ @param vmin The minimum value for the progress bar.
#+ @param vmax The maximum value for the progress bar.
PUBLIC FUNCTION fglt_progress_open(title,comment,vmin,vmax)
  DEFINE title STRING
  DEFINE comment STRING
  DEFINE vmin, vmax INTEGER
  DEFINE w ui.Window
  DEFINE f ui.Form
  DEFINE p,n om.DomNode
  DEFINE nl om.NodeList

  OPEN WINDOW __fgltprogress WITH FORM "fgltprogress"
              ATTRIBUTES(style="dialog2",text=title)

  LET w = ui.Window.getCurrent()
  LET f = w.getForm()

  IF comment IS NULL THEN
     CALL f.setFieldHidden("comment", 1)
  ELSE
     DISPLAY BY NAME comment
  END IF

  LET p = w.getNode()
  LET nl = p.selectByPath("//ProgressBar")
  LET n = nl.item(1)
  CALL n.setAttribute("valueMin",vmin)
  CALL n.setAttribute("valueMax",vmax)

END FUNCTION

#+ Sets the progressbar value in a progress dialog session.
#+ @param progress The value to be set in the progressbar.
PUBLIC FUNCTION fglt_progress_show(progress)
  DEFINE progress INTEGER
  CURRENT WINDOW IS __fgltprogress
  DISPLAY BY NAME progress
  CALL ui.Interface.refresh()
END FUNCTION

#+ Terminates a progress session.
PUBLIC FUNCTION fglt_progress_close()
  CLOSE WINDOW __fgltprogress
END FUNCTION

--------------------------------------------------------------------------------

#+ Opens a database login dialog.
#+ @returnType Integer, String, String, String
#+ @return TRUE/FALSE, dbname, username and password
#+ @param title The title of the login dialog
#+ @param comment The comment to be displayed in the login dialog
#+ @param dbname Default value for the database source
#+ @param username Default value for the database user
#+ @param password Default value for the user password
PUBLIC FUNCTION fglt_dblogin(title,comment,dbname,username,password)
  DEFINE title, comment, dbname, username, password STRING
  DEFINE w ui.Window
  DEFINE f ui.Form

  OPEN WINDOW __fgldblogin WITH FORM "fgltlogin"
              ATTRIBUTES(STYLE="dialog",TEXT=title)

  LET w = ui.Window.getCurrent()
  LET f = w.getForm()

  IF dbname == "*" THEN
     CALL f.setElementHidden("label_dbname", 1)
     CALL f.setFieldHidden("dbname", 1)
  END IF
  IF comment IS NULL THEN
     CALL f.setFieldHidden("comment", 1)
  ELSE
     DISPLAY BY NAME comment
  END IF

  LET int_flag = FALSE
  INPUT BY NAME dbname, username, password
        WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)

  CLOSE WINDOW __fgldblogin

  IF int_flag THEN
     LET int_flag = FALSE
     RETURN FALSE, NULL, NULL, NULL
  ELSE
     RETURN TRUE, dbname, username, password
  END IF

END FUNCTION

#+ Opens a simple login dialog.
#+ @returnType Integer, String, String
#+ @return 1: status (zero = ok), username and password
#+ @param title The title of the login dialog
#+ @param comment The comment to be displayed in the login dialog
#+ @param username Default value for the database user
#+ @param password Default value for the user password
PUBLIC FUNCTION fglt_login(title,comment,username,password)
  DEFINE title, comment, dbname, username, password STRING
  DEFINE s INTEGER
  CALL fglt_dblogin(title,comment,"*",username,password)
       RETURNING s, dbname, username, password
  RETURN s, username, password
END FUNCTION

--------------------------------------------------------------------------------

#+ Opens a dialog box to enter a value.
#+ @returnType String
#+ @return The value enter by the user, or NULL if canceled.
#+ @param title The title of the dialog
#+ @param comment The comment to be displayed in the dialog
#+ @param label The label to be displayed in the dialog
#+ @param defval The default value
#+ @param options No used yet
PUBLIC FUNCTION fglt_prompt(title,comment,label,defval,options)
  DEFINE title, comment, label, defval, options STRING
  DEFINE w ui.Window
  DEFINE f ui.Form
  DEFINE value STRING

  LET options = NULL

  OPEN WINDOW __fgltprompt WITH FORM "fgltprompt"
       ATTRIBUTES(STYLE="dialog",TEXT=title)

  LET w = ui.Window.getCurrent()
  LET f = w.getForm()

  IF comment IS NULL THEN
     CALL f.setFieldHidden("comment", 1)
  ELSE
     DISPLAY BY NAME comment
  END IF
  IF label IS NULL THEN
     CALL f.setFieldHidden("label", 1)
  ELSE
     DISPLAY BY NAME label
  END IF

  LET value = defval

  LET int_flag = FALSE
  INPUT BY NAME value
        WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)

  CLOSE WINDOW __fgltprompt

  IF int_flag THEN
     LET int_flag = FALSE
     LET value = NULL
  END IF

  RETURN value

END FUNCTION

--------------------------------------------------------------------------------

#+ Opens a dialog box to select an option in a list.
#+ @returnType Integer
#+ @return The ordinal of the selected option, or zero if canceled.
#+ @param title The title of the dialog
#+ @param comment The comment to be displayed in the dialog
#+ @param icon The image file to be displayed in the dialog
PUBLIC FUNCTION fglt_choice(title,comment,options,defopt)
  DEFINE title, comment, options STRING
  DEFINE defopt INTEGER
  DEFINE w ui.Window
  DEFINE f ui.Form
  DEFINE choice INTEGER

  OPEN WINDOW __fgltchoice WITH FORM "fgltchoice"
              ATTRIBUTES(STYLE="dialog",TEXT=title)

  LET w = ui.Window.getCurrent()
  LET f = w.getForm()

  IF comment IS NULL THEN
     CALL f.setFieldHidden("comment", 1)
  ELSE
     DISPLAY BY NAME comment
  END IF

  LET w = ui.Window.getCurrent()
  CALL __fgl_choice_fill(w.getForm(),options)

  LET choice = defopt

  LET int_flag = FALSE
  INPUT BY NAME choice WITHOUT DEFAULTS ATTRIBUTES(UNBUFFERED)

  CLOSE WINDOW __fgltchoice

  IF int_flag THEN
     LET int_flag = FALSE
     LET choice = 0
  END IF

  RETURN choice

END FUNCTION

PRIVATE FUNCTION __fgl_choice_fill(form,options)
  DEFINE options STRING
  DEFINE form ui.Form
  DEFINE t base.StringTokenizer
  DEFINE n, c om.DomNode
  DEFINE i INTEGER

  LET n = form.findNode("FormField","formonly.choice")
  IF n IS NULL THEN RETURN END IF
  LET n = n.getFirstChild()
  IF n IS NULL THEN RETURN END IF

  LET i = 1
  LET t = base.StringTokenizer.create(options,"|")
  WHILE t.hasMoreTokens()
    LET c = n.createChild("Item")
    CALL c.setAttribute("name",i)
    CALL c.setAttribute("text",t.nextToken())
    CALL n.appendChild(c)
    LET i = i + 1
  END WHILE

END FUNCTION

