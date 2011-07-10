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

-export([ensure_started/1, start_link/1]).
-export([event_dispatcher/1, process_name/1, stop/1]).
-export([add_tweet/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([document/1]).
-export([set_hash_tags/3, edit_title/3, edit_body/3, login/3]).

-include("elog.hrl").
-include("socketio.hrl").
-include("edit_records.hrl").

-record(state, {document :: #edit_document{},
                js_context :: port()}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Creates a new document if it doesn't exists and starts its managing process if its not already started
-spec ensure_started(string()) -> {ok, pid()}.
ensure_started(DocId) ->
  ok = edit_db:ensure_document(DocId),
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

-spec document(document_id()) -> #edit_document{}.
document(DocId) ->
  gen_server:call(process_name(DocId), document).

-spec edit_title(document_id(), term(), diff()) -> ok.
edit_title(DocId, Token, Diff) ->
  gen_server:cast(process_name(DocId), {edit_title, Diff, Token}).

-spec edit_body(document_id(), term(), diff()) -> ok.
edit_body(DocId, Token, Diff) ->
  gen_server:cast(process_name(DocId), {edit_body, Diff, Token}).

-spec login(document_id(), term(), #edit_user{}) -> ok.
login(DocId, Token, User) ->
  gen_server:cast(process_name(DocId), {login, User, Token}).

%% ------------------------------------------------------------------
%% Behaviour Callbacks
%% ------------------------------------------------------------------
%% @private
-spec init(document_id()) -> {ok, state()}.
init(DocId) ->
  ?INFO("Starting ~s~n", [DocId]),
  Document = edit_db:document(DocId),
  DispPid =
    case gen_event:start_link(event_dispatcher(DocId)) of
      {ok, DPid} ->
        true = erlang:register(event_dispatcher(DocId, local), DPid),
        DPid;
      {error, {already_started, DPid}} -> DPid
    end,
  try edit_document_handler:subscribe(Document)
  catch
    throw:couldnt_subscribe ->
      ?ERROR("Document ~s couldn't subscribe to the twitter stream.  No tweets for it.~n", [DocId])
  end,

  %% init js
  JS = init_js(),

  ?INFO("Event dispatcher for ~s running in ~p~n", [DocId, DispPid]),
  {ok, #state{document = Document,
              js_context = JS}}.

%% @private
-spec handle_call(term(), reference(), state()) -> {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(document, _From, State) ->
  {reply, State#state.document, State};
handle_call(body, _From, State) ->
  {reply, State#state.document#edit_document.body, State};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({add_tweet, Tweet}, State) ->
  #edit_document{id = DocId} = State#state.document,
  ok = edit_db:update(State#state.document, tweet, Tweet),
  gen_event:notify(event_dispatcher(DocId, local),
    {outbound_message, <<"tweet">>, [{<<"tweet">>,edit_util:mochi_to_jsx(Tweet)}], undefined}),
  {noreply, State};

% save hash tags and broadcast to clients
handle_cast({set_hash_tags, HashTags, Token}, State) ->
  #edit_document{id = DocId} = State#state.document,
  NewDocument = State#state.document#edit_document{hash_tags = HashTags},
  ok = edit_db:update(NewDocument, hashtags, HashTags),
  ok = edit_itweep:update_document(NewDocument),
  edit_document_handler:unsubscribe(DocId),
  try edit_document_handler:subscribe(NewDocument)
  catch
    throw:couldnt_subscribe ->
      ?ERROR("Document ~s couldn't subscribe to the twitter stream.  No tweets for it.~n", [DocId])
  end,
  gen_event:notify(event_dispatcher(DocId, local),
          {outbound_message, <<"set_hash_tags">>, [{<<"tags">>,HashTags}], Token}),
  {noreply, State#state{document = NewDocument}};

%% save versions of title
handle_cast({edit_title, Diff, Token}, State) ->
  #edit_document{id = DocId, title = Title} = State#state.document,
  NewDocument = State#state.document#edit_document{title = apply_diff(Diff, Title, State#state.js_context)},
  ok = edit_db:update(NewDocument, edit_title, Diff),
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"edit_title">>, [{<<"diff">>, Diff}], Token}),
  {noreply, State#state{document = NewDocument}};

%% save versions of body
handle_cast({edit_body, Diff, Token}, State) ->
  #edit_document{id = DocId, body = Body} = State#state.document,
  NewDocument = State#state.document#edit_document{body = apply_diff(Diff, Body, State#state.js_context)},
  ok = edit_db:update(NewDocument, edit_body, Diff),
  gen_event:notify(event_dispatcher(DocId, local),
                   {outbound_message, <<"edit_body">>, [{<<"diff">>, Diff}], Token}),
  {noreply, State#state{document = NewDocument}};

%% save login of a user
handle_cast({login, User, Token}, State) ->
  #edit_document{id = DocId, users = Users} = State#state.document,
  NewUsers = lists:keystore(User#edit_user.username, #edit_user.username, Users, User),
  NewDocument = State#state.document#edit_document{users = NewUsers},
  ok = edit_db:update(NewDocument, login, edit_util:to_mochi(User)),
  ok = edit_itweep:update_document(NewDocument),
  edit_document_handler:unsubscribe(DocId),
  try edit_document_handler:subscribe(NewDocument)
  catch
    throw:couldnt_subscribe ->
      ?ERROR("Document ~s couldn't subscribe to the twitter stream.  No tweets for it.~n", [DocId])
  end,
  gen_event:notify(event_dispatcher(DocId, local),
      {outbound_message, <<"set_users">>, [{<<"users">>,
                                            lists:map(fun edit_util:to_jsx/1, NewUsers)}], Token}),
  {noreply, State#state{document = NewDocument}}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
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

%% @private
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Private functions
%% ------------------------------------------------------------------
init_js() ->
  {ok, JS} = js_driver:new(),
  js:define(JS, <<"window={};">>),
  {ok, F} = file:read_file("www/js/diff/diff_match_patch.js"),
  js:define(JS,F),
  js:define(JS, <<"window.dmp = new window.diff_match_patch();\n"
                  "window.dmp.Match_Threshold = 0.25;\n">>),
  js:define(JS, <<"function apply_diff(text1,diff_object) {\n"
                  "\tvar patches = window.dmp.patch_make(text1, diff_object);\n",
                  "\tvar results = window.dmp.patch_apply(patches, text1);\n",
                  "\treturn results[0]"
                  "};\n">>),
  JS.

apply_diff(Diff, Something, JS) ->
  case js:call(JS, <<"apply_diff">>, [Something, Diff]) of
    {ok, Result} ->
      Result;
    {error, Reason} ->
      ?THROW("Couldn't apply diff(~p, ~p, ~p): ~p~n", [Diff, Something, JS, Reason]),
      Something
  end.
