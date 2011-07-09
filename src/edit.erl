%%% @author C B DePue <chad@inakanetworks.com> 
%%% @since 
%%% @copyright 2011 C B DePue
%%% @doc 
%%% @end

-module(edit).
-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

start() ->
  application:start(public_key),
  application:start(ssl),
  application:start(edit).

stop() -> 
  application:stop(edit).

start(_StartType, _StartArgs) ->
  edit_sup:start_link().

stop(_State) ->
  ok.
