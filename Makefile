ERL					?= erl
ERLC				= erlc
EBIN_DIRS		:= $(wildcard deps/*/ebin)
NODE				= semipro
RUN 				:= +Bc +K true -smp enable -pa ebin deps/*/ebin -s crypto -s inets -s ssl -s elog -s ibrowse -s socketio -s erlang_js


.PHONY: rel deps

all: deps compile

compile: deps
	@rebar compile

quick:
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

xref: all
	@rebar skip_deps=true xref

rel: deps
	@rebar compile generate

analyze: checkplt
	@rebar skip_deps=true dialyze

buildplt:
	@rebar skip_deps=true build-plt

checkplt: buildplt
	@rebar skip_deps=true check-plt
