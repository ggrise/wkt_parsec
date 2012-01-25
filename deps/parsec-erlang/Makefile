empty =
space = $(empty) $(empty)
comma = ,

appdir = $(subst -, ,$(notdir $(subst $(space),_,$(CURDIR))))
appname = $(firstword $(appdir))
version = $(wordlist 2,$(words $(appdir)),$(appdir))

bindir = ~/erl14/bin/
erl = $(bindir)/erl
erlc = $(bindir)/erlc
escript = $(bindir)/escript.exe
dialyzer = $(bindir)/dialyzer

erlc_options = -pa ebin -o ebin -I include

# If debug is a target, then make debug versions of files
.PHONY : debug
ifeq (debug,$(findstring debug,$(MAKECMDGOALS)))
erlc_options += +debug_info
endif

# If debug is a target, then make debug versions of files
.PHONY : verbose
ifeq (verbose,$(findstring verbose,$(MAKECMDGOALS)))
erlc_options += +verbose
endif

all_erls = $(wildcard src/*.erl)
all_hrls = $(wildcard include/*.hrl)
all_mods = $(patsubst src/%.erl,%,$(all_erls))
all_beams = $(patsubst %,ebin/%.beam,$(all_mods))

plt_apps = stdlib kernel common_test
plt_paths = $(foreach app,$(plt_apps),$(wildcard ../$(app)-*)/ebin)


.PHONY : all
all : ebin doc


.PHONY : ebin
ebin : $(all_beams)


ebin/parsec_pos.beam:
ebin/parsec_error.beam: ebin/import_enhancements.beam
ebin/parsec_error.beam: ebin/parsec_pos.beam
ebin/parsec_prim.beam: ebin/parsec_do.beam
ebin/parsec_prim.beam: ebin/import_enhancements.beam
ebin/parsec_prim.beam: ebin/parsec_pos.beam
ebin/parsec_prim.beam: ebin/parsec_error.beam
ebin/parsec_combinator.beam: ebin/parsec_prim.beam
ebin/parsec_combinator.beam: ebin/import_enhancements.beam
ebin/parsec_combinator.beam: ebin/parsec_do.beam
ebin/parsec_char.beam: ebin/parsec_prim.beam
ebin/parsec_char.beam: ebin/import_enhancements.beam

ebin/%.beam : src/%.erl $(all_hrls)
	$(erlc) $(erlc_options) $<


.PHONY : test
test : ebin
	$(escript) test\run.escript $(test_options)

ifeq (test,$(findstring test,$(MAKECMDGOALS)))
  erlc_options += +debug_info
  test_options =
  
  ifdef suite
    test_options += -S$(suite)
  endif
  
  ifdef testcases
    test_options += -T$(testcases)
  endif
endif


.PHONY : doc
doc :
	-rm doc/edoc-info
	#$(erl) -eval edoc:application\($(appname),[preprocess,todo,\{def,[\{version,\"$(version)\"\}]\}]\) -s erlang halt
	$(erl) -eval edoc:application\($(appname)\) -s erlang halt


dialyzer_plt :
	$(dialyzer) --plt dialyzer_plt --build_plt -r $(plt_paths)

.PHONY : dialyze
dialyze : dialyzer_plt
	$(dialyzer) --plt dialyzer_plt -r ebin test


.PHONY : clean
clean :
	-rm ebin/*.beam
	-rm doc/*
	-rm test/*.beam
	-rm -r test/log
