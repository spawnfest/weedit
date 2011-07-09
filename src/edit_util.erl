%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit utility functions
%% @end
%%-------------------------------------------------------------------
-module(edit_util).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-include("elog.hrl").

-export([get_env/1, set_env/2,random_url/0]).

%% @doc Returns application:get_env(edit, Field) or its default value
-spec get_env(atom()) -> term().
get_env(Field) ->
  case application:get_env(edit, Field) of
    {ok, Value} ->
      ?DEBUG("~p := ~p~n", [Field, Value]),
      Value;
    _ ->
      Value = get_env_default(Field),
      ?DEBUG("~p := ~p~n", [Field, Value]),
      Value
  end.

%% @private
-spec get_env_default(atom()) -> term().
get_env_default(socketio_port_range) -> {8001, 8001};
get_env_default(Field) ->
  throw({env_undefined, Field}).

%% @equiv application:set_env(edit, Field, Value)
-spec set_env(atom(), term()) -> ok.
set_env(Field, Value) ->
  application:set_env(edit, Field, Value).

random_url() -> 
  lists:flatten(lists:foldl(fun(_,AccIn) ->
      [random:uniform(25) + 96|AccIn] end,
      [], lists:seq(1,10))).

