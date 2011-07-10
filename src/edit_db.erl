%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Networks S.R.L.
%%% @doc WeEdit db
%%% @end
%%%-------------------------------------------------------------------
-module(edit_db).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('C B DePue <chad@inakanetworks.com>').

-behaviour(gen_server).

-include("elog.hrl").
-include("edit_records.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create_document/0, ensure_document/1, document/1]).
-export([update/4]).

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_document() ->
  {ok, random_document_id()}.

ensure_document(DocId) ->
  ok.

document(DocId) -> 
  {ok, #edit_document{id = DocId}}.

set_hash_tags(DocId, HashTags) ->
  ok.

%% TODO:SPEC
update(Document, User, Type, Patch) -> 
  ?INFO("db: ~p to ~s:~n\t~p ~n", [Type, Document#edit_document.id, Patch]),
  ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
  _ = random:seed(erlang:now()),
  {ok, Args}.

handle_call(create_document, _From, State) ->
  DocId = random_document_id(),
  {reply, ok, NewState} =
      handle_call({create_document, DocId}, _From, State),
  {reply, {ok, DocId}, NewState};

handle_call({create_document, DocId}, _From, State) ->
  {reply, {ok, DocId}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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
random_document_id() -> 
  DocId = edit_util:random_id(),
  case reserved(DocId) of 
    true  -> random_document_id();
    false -> reserve(DocId), DocId
  end.

reserved(DocId) -> false.
reserve(DocId) -> ok.