include ./vsn.mk
APP_NAME = gb_lru

SUBDIRS = src c_src

.PHONY: all subdirs $(SUBDIRS) edoc eunit clean

all: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

eunit:
	erl -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

clean:
	$(MAKE) -C $(SUBDIRS) clean

realclean: clean

