# 
# %CopyrightBegin%
# 
# Copyright Erlang Solutions Ltd 2010. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
# 

include ../vsn.mk
VSN=$(UNSPLIT_VSN)

.SUFFIXES: .erl .beam

EBIN = ../ebin
DOC = ../doc

APP = unsplit
APP_FILE = $(EBIN)/$(APP).app

SOURCES := $(wildcard *.erl)
MODULES := $(patsubst %.erl,%,$(SOURCES))
BEAMS := $(patsubst %,$(EBIN)/%.beam,$(MODULES))

comma := ,
space := $(e) $(e)
MODULELIST := $(subst $(space),$(comma),$(MODULES))


vpath %.beam $(EBIN)
vpath %.pdf $(DOC)

ERLC = erlc -W -o $(EBIN)
ERLC += +debug_info

SOURCES = $(wildcard *.erl)

.PHONY: all clean

all : $(BEAMS) $(APP_FILE)
clean :
	rm -f  *~ $(EBIN)/*.beam 


## erlc targets
../ebin/%.beam: %.erl
	$(ERLC) $<

$(APP_FILE): $(APP).app.src ../vsn.mk
	@echo Generating $@
	@sed -e 's/@MODULES@/$(MODULELIST)/' -e 's/@VSN@/$(VSN)/' $< > $@

## test targets
