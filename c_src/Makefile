PRIV_BIN_DIR = ../priv/bin
PRIV_DIR = ../priv

DIR_TARGETS = $(PRIV_BIN_DIR) \


FILE_TARGETS = \
	       $(PRIV_DIR)/btreelru_nif.so \
	       $(PRIV_BIN_DIR)/test \


TARGETS = $(DIR_TARGETS) \
	  $(FILE_TARGETS)
OS_NAME = $(shell uname -s)

CFLAGS = -std=c++11 -Wall -fpic -g -O3 $(shell erl -noinput -eval 'io:format("-I~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]), halt(0).')

ifeq ($(OS_NAME), Darwin)
LDFLAGS = -bundle -flat_namespace -undefined suppress -fpic
else
LDFLAGS = -shared -fpic
endif

.PHONY: all

all: $(TARGETS)

show:
	@echo $(TARGETS)

clean:
	rm $(FILE_TARGETS)

$(PRIV_OBJS_DIR):
	mkdir -p $@

$(PRIV_BIN_DIR):
	mkdir -p $@

$(PRIV_DIR)/btreelru_nif.so: btreelru_nif.cpp binary.h lru.h murmurhash2.h
	$(CXX) $(CFLAGS) $< $(LDFLAGS) -o $@ 
	

$(PRIV_BIN_DIR)/test: lru.cpp binary.h murmurhash2.h lru.h
	 $(CXX) $(CFLAGS) $< -o $@ 

