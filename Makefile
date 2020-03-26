FORMS=$(patsubst %.per,%.42f,$(wildcard *.per))

PROGMODS=\
 fgldsm_main.42m\
 fgldsm_schema.42m\
 fgltbasics.42m\
 fgltdialogs.42m\
 fgltfiledlg.42m

all: $(PROGMODS) $(FORMS)

run: all
	fglrun fgldsm_main

%.42f: %.per
	fglform -M $<

%.42m: %.4gl
	fglcomp -M $<

clean::
	rm -f *.42?
