%%%'   HEADER
%%% @author C B DePue <chad@inakanetworks.com>
%%% @since 
%%% @copyright 2011 C B DePue
%%% @doc 
%%% @end
-module(edit_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
%%%.
%%%'   PUBLIC API
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%.
%%%'   CALLBACKS
init([]) ->
  Listeners =
    [{erlang:list_to_atom("client-" ++ erlang:integer_to_list(Port)),
      {edit_listener, start_link, [Port]},
      permanent, 1000, worker, [edit_listener]} ||
     Port <- lists:seq(12000, 13000)],
  {ok, {{one_for_one, 5, 10}, Listeners}}.
