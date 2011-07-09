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
-export([add_tweet/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([title/1, body/1]).
-export([set_hash_tags/3, edit_title/3, edit_body/3, login/3]).

-include("elog.hrl").
-include("socketio.hrl").
-include("edit_records.hrl").

-record(state, {document :: #edit_document{}}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Creates a brand new document
-spec create() -> {ok, document_id()}.
create() ->
  {ok, DocId} = edit_db:create_document(),
  {ok, _Pid} = edit_doc_sup:start_doc(DocId),
  {ok, DocId}.

%% @doc Starts a listener
-spec start_link(document_id()) -> {ok, pid()}.
start_link(DocId) ->
  gen_server:start_link(process_name(DocId), ?MODULE, DocId, []).

-spec event_dispatcher(document_id()) -> {global, atom()}.
event_dispatcher(DocId) ->
  {global, event_dispatcher(DocId, local)}.
-spec event_dispatcher(document_id(), local) -> atom().
event_dispatcher(DocId, local) ->
  list_to_atom("edit-document-dispatcher-" ++ DocId).

-spec process_name(document_id()) -> {global, atom()}.
process_name(DocId) ->
  {global, process_name(DocId, local)}.
-spec process_name(document_id(), local) -> atom().
process_name(DocId, local) ->
  list_to_atom("edit-document-" ++ DocId).

-spec add_tweet(document_id(), itweet:tweet()) -> ok.
add_tweet(DocId, Tweet) ->
  gen_server:cast(process_name(DocId), {add_tweet, Tweet}).

-spec set_hash_tags(document_id(), term(), [binary()]) -> ok.
set_hash_tags(DocId, Token, HashTags) ->
  gen_server:cast(process_name(DocId), {set_hash_tags, HashTags, Token}).

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

-spec title(document_id()) -> string().
title(DocId) ->
  gen_server:call(process_name(DocId), title).

-spec body(document_id()) -> string().
body(DocId) -> 
  gen_server:call(process_name(DocId), body).

-spec edit_title(document_id(), term(), diff()) -> ok.
edit_title(DocId, Token, Diff) ->
  gen_server:cast(process_name(DocId), {edit_tittle, Diff, Token}).

-spec edit_body(document_id(), term(), diff()) -> ok.
edit_body(DocId, Token, Diff) ->
  gen_server:cast(process_name(DocId), {edit_body, Diff, Token}).

-spec login(document_id(), term(), #edit_user{}) -> ok.
login(DocId, Token, User) ->
  gen_server:cast(process_name(DocId), {login, User, Token}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%% @hidden
-spec init(document_id()) -> {ok, state()}.
init(DocId) ->
  ?INFO("Starting ~s~n", [DocId]),
  {ok, Doc} = edit_db:document(DocId),
  DispPid =
    case gen_event:start_link(event_dispatcher(DocId)) of
      {ok, DPid} ->
        true = erlang:register(event_dispatcher(DocId, local), DPid),
        DPid;
      {error, {already_started, DPid}} -> DPid
    end,
  try edit_document_handler:subscribe(Doc)
  catch
    throw:couldnt_subscribe ->
      ?ERROR("Document ~s couldn't subscribe to the twitter stream.  No tweets for it.~n", [DocId])
  end,
  ?INFO("Event dispatcher for ~s running in ~p~n", [DocId, DispPid]),
  {ok, #state{document = Doc}}.

%% @hidden
-spec handle_call(term(), reference(), state()) -> {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(title, _From, State) ->
  {reply, State#state.document#edit_document.title, State};
handle_call(body, _From, State) ->
  {reply, State#state.document#edit_document.body, State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

%% @hidden
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({add_tweet, Tweet}, State) ->
  #edit_document{id = DocId} = State#state.document,
  ok = edit_db:add_tweet(DocId, Tweet),
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"tweet">>, edit_util:mochi_to_jsx(Tweet), undefined}),
  {noreply, State};
handle_cast({set_hash_tags, HashTags, Token}, State) ->
  #edit_document{id = DocId} = State#state.document,
  NewDocument = State#state.document#edit_document{hash_tags = HashTags},
  ok = edit_db:add_version(NewDocument),
  ok = edit_itweep:update_document(NewDocument),
  edit_document_handler:unsubscribe(DocId),
  try edit_document_handler:subscribe(NewDocument)
  catch
    throw:couldnt_subscribe ->
      ?ERROR("Document ~s couldn't subscribe to the twitter stream.  No tweets for it.~n", [DocId])
  end,
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"set_hash_tags">>, HashTags, Token}),
  {noreply, State#state{document = NewDocument}};
handle_cast({edit_title, Diff, Token}, State) ->
  #edit_document{id = DocId, title = Title} = State#state.document,
  NewDocument = State#state.document#edit_document{title = apply_diff(Diff, Title)},
  ok = edit_db:add_version(NewDocument),
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"edit_title">>, Diff, Token}),
  {noreply, State#state{document = NewDocument}};
handle_cast({edit_body, Diff, Token}, State) ->
  #edit_document{id = DocId, body = Body} = State#state.document,
  NewDocument = State#state.document#edit_document{title = apply_diff(Diff, Body)},
  ok = edit_db:add_version(NewDocument),
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"edit_body">>, Diff, Token}),
  {noreply, State#state{document = NewDocument}};
handle_cast({login, User, Token}, State) ->
  #edit_document{id = DocId, users = Users} = State#state.document,
  NewUsers = lists:keystore(User#edit_user.id, #edit_user.id, Users, User),
  NewDocument = State#state.document#edit_document{users = NewUsers},
  ok = edit_db:add_version(NewDocument),
  ok = edit_itweep:update_document(NewDocument),
  edit_document_handler:unsubscribe(DocId),
  try edit_document_handler:subscribe(NewDocument)
  catch
    throw:couldnt_subscribe ->
      ?ERROR("Document ~s couldn't subscribe to the twitter stream.  No tweets for it.~n", [DocId])
  end,
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"set_users">>, edit_util:to_jsx(Users), Token}),
  {noreply, State#state{document = NewDocument}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
-spec terminate(any(), state()) -> any().
terminate(Reason, State) ->
  #edit_document{id = DocId} = State#state.document,
  edit_document_handler:unsubscribe(DocId),
  case Reason of
    normal ->
      ?INFO("~s terminating~n", [DocId]);
    Reason ->
      ?WARN("~s terminating: ~p~n", [DocId, Reason])
  end,
  ok = gen_event:sync_notify(event_dispatcher(DocId, local), {document_EXIT, DocId, Reason}),
  ok = gen_event:stop(event_dispatcher(DocId, local)).

%% @hidden
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

apply_diff(Diff, Something) -> Something.