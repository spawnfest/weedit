%%-------------------------------------------------------------------
%% @author C B DePue <chad@inakanetworks.com>
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit main supervisor
%% @end
%%-------------------------------------------------------------------
-module(edit_doc_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('C B DePue <chad@inakanetworks.com>').

-behaviour(supervisor).

-export([start_link/0, start_doc/1, init/1]).

-include("elog.hrl").
-include("edit_records.hrl").

%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc  Starts a document process
-spec start_doc(document_id()) -> {ok, pid()}.
start_doc(DocId) ->
  case supervisor:start_child(?MODULE, [DocId]) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} -> {ok, Pid};
    Error -> throw(Error)
  end.

%% @private
-spec init([]) -> {ok, {{simple_one_for_one, 5, 1}, [supervisor:child_spec()]}}.
init([]) ->
  DocSpec = {edit_document, {edit_document, start_link, []},
             transient, 5000, worker, [edit_document]}, 
  StartSpecs = {{simple_one_for_one, 5, 1}, [DocSpec]}, 
  {ok, StartSpecs}.