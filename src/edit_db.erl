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

-spec create_document() -> {ok, document_id()}.
create_document() ->
  gen_server:call(?MODULE, create_document).

-spec ensure_document(document_id()) -> ok.
ensure_document(DocId) ->
  gen_server:cast(?MODULE, {ensure_document, DocId}).

-spec document(document_id()) -> #edit_document{}.
document(DocId) ->
  case gen_server:call(?MODULE, {document, DocId}) of
    {ok, Document} -> Document;
    {error, Reason} -> throw(Reason)
  end.

-spec update(#edit_document{}, binary(), atom(), term()) -> ok.
update(Document, User, Type, Patch) -> 
  ?INFO("db: ~p to ~s:~n\t~p ~n", [Type, Document#edit_document.id, Patch]),
  gen_server:cast(?MODULE, {update, Document, User, Type, Patch}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init(Args) ->
  _ = random:seed(erlang:now()),
  {ok, Args}.

handle_call(create_document, _From, State) ->
  DocId = random_document_id(),
  {reply, {ok, DocId}, State};
handle_call({document, DocId}, _From, State) ->
  {reply, {ok, #edit_document{id = DocId}}, State}.

handle_cast({ensure_document, DocId}, State) ->
  {noreply, State};
handle_cast({update, Document, User, Type, Patch}, State) ->
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