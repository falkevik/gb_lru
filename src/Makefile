include ../vsn.mk

EBIN = ../ebin

RM ?= rm -f

APP_NAME = gb_lru
APP_FILE= $(APP_NAME).app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

MODULES = \
	btree_lru \
	btree_lru_test \

ERL_FILES = $(MODULES:%=%.erl)
ERL_BEAMS = $(MODULES:%=$(EBIN)/%.beam)

INCLUDE_DIRS = \
	../../gb_log/include

ERL_FLAGS = -Wall
ERL_INCLUDE = $(INCLUDE_DIRS:%=-I %)
.PHONY: all clean beams

all: $(EBIN) beams

beams: $(ERL_BEAMS)

$(EBIN)/%.beam: %.erl $(HRL_FILES)
	erlc $(ERL_FLAGS) $(ERL_INCLUDE) -o $(EBIN) $<

$(EBIN):
	mkdir $@

$(APP_TARGET): $(APP_SRC)
	@set -e; mods=`echo $(MODULES) | sed -e 's; ;,;g'` ; \
	sed -e "s;%MODULES%;$$mods;" $< | \
	sed -e 's;%VSN%;$(VSN);' > $@

clean:
	$(RM) ../ebin/*.app
	$(RM) ../ebin/*.beam
