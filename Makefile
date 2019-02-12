FORMS=$(patsubst %.per,%.42f,$(wildcard *.per))

PROGMOD=fgldsm_main.42m

all: $(PROGMOD) $(FORMS)

run: all
	fglrun fgldsm_main

%.42f: %.per
	fglform -M $<

%.42m: %.4gl
	fglcomp -M $<

clean::
	rm -f *.42?
