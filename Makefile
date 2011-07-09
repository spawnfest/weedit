ERL					?= erl
ERLC				= erlc
EBIN_DIRS		:= $(wildcard deps/*/ebin)
APPS				:= $(shell dir apps)
NODE				= semipro

.PHONY: rel deps

all: deps compile

compile: deps
	@rebar compile

deps:
	@rebar get-deps
	@rebar check-deps

clean:
	@rebar clean

realclean: clean
	@rebar delete-deps

run: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN} -s edit;\
	else\
		erl  -boot start_sasl ${RUN} -s edit;\
	fi

shell: all
	if [ -f `hostname`.config ]; then\
		erl  -config `hostname` -boot start_sasl ${RUN};\
	else\
		erl  -boot start_sasl ${RUN};\
	fi

test: all
	if [ -f `hostname`.config ]; then\
		erl -noshell -noinput -config `hostname` ${RUN} -run edit_tests main;\
	else\
		erl -noshell -noinput ${RUN} -run edit_tests main;\
	fi

rel: deps
	@rebar compile generate

doc:
	rebar skip_deps=true doc
	for app in $(APPS); do \
		cp -R apps/$${app}/doc doc/$${app}; \
	done;

analyze: checkplt
	@rebar skip_deps=true dialyze

buildplt:
	@rebar skip_deps=true build-plt

checkplt: buildplt
	@rebar skip_deps=true check-plt
