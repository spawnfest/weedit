%%-------------------------------------------------------------------
%% @author C B DePue <chad@inakanetworks.com>
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit document
%% @end
%%-------------------------------------------------------------------
-module(edit_document).

-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('C B DePue <chad@inakanetworks.com>').

-behaviour(gen_server).

-export([create/0, start_link/1]).
-export([event_dispatcher/1, process_name/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("elog.hrl").
-include("socketio.hrl").
-include("edit_records.hrl").

-record(state, {document :: #edit_document{}}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Creates a brand new document
-spec create() -> {ok, pid()}.
create() ->
  {ok, DocId} = edit_db:create_document(),
  edit_doc_sup:start_doc(DocId).

%% @doc Starts a listener
-spec start_link(document_id()) -> {ok, pid()}.
start_link(DocId) ->
  gen_server:start_link(process_name(DocId), ?MODULE, DocId, []).

-spec event_dispatcher(document_id()) -> {global, atom()}.
event_dispatcher(DocId) ->
  {global, event_dispatcher(DocId, local)}.
-spec event_dispatcher(document_id(), local) -> atom().
event_dispatcher(DocId, local) ->
  list_to_atom("edit-document-" ++ DocId).

-spec process_name(document_id()) -> {global, atom()}.
process_name(DocId) ->
  {global, process_name(DocId, local)}.
-spec process_name(document_id(), local) -> atom().
process_name(DocId, local) ->
  list_to_atom("edit-document-" ++ DocId).

-spec stop(document_id()) -> ok.
stop(DocId) ->
  ?INFO("Manually stopping ~s~n", [DocId]),
  try
    gen_server:call(process_name(DocId), stop)
  catch
    _:{noproc, _} -> ok;
    _:Reason ->
      ?WARN("Couldn't stop ~s: ~p~n", [DocId, Reason]),
      ok
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init(document_id()) -> {ok, state()}.
init(DocId) ->
  {ok, Doc} = edit_db:document(DocId),
  {ok, #state{document = Doc}}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

