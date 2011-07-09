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
  {ok, { {one_for_one, 5, 10}, []} }.

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
