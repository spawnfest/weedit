%%-------------------------------------------------------------------
%% @author C B DePue <chad@inakanetworks.com>
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit main supervisor
%% @end
%%-------------------------------------------------------------------
-module(edit_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('C B DePue <chad@inakanetworks.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).

-include("elog.hrl").

%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  {MinPort, MaxPort} = edit_util:get_env(socketio_port_range),
  Listeners =
      [{erlang:list_to_atom("edit_listener-" ++ erlang:integer_to_list(Port)),
        {edit_listener, start_link, [Port]},
        permanent, 1000, worker, [edit_listener]} ||
       Port <- lists:seq(MinPort, MaxPort)],
  Db =
      {edit_db, {edit_db, start_link, []}, permanent, 1000, worker, [edit_db]},
  DocSup =
      {edit_doc_sup, {edit_doc_sup, start_link, []}, permanent, 5000, supervisor, [edit_doc_sup]},
  ?INFO("Edit supervisor starting...~n", []),
  {ok, {{one_for_one, 5, 10}, [Db, DocSup | Listeners]}}.
