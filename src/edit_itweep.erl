%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Twitter Streaming API consumer
%%%-------------------------------------------------------------------
-module(edit_itweep).
-author('author Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(itweep).

-include("elog.hrl").
-include("edit_records.hrl").

-define(DISPATCHER, edit_itweep_dispatcher).

-record(state, {documents = [] :: [#edit_document{}]}).
-opaque state() :: #state{}.

-export([start_link/0, add_document/1, update_document/1, remove_document/1, event_dispatcher/0, stop/0]).
-export([handle_call/3, handle_event/3, handle_info/2, handle_status/2, init/1, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link() -> {ok, pid()}.
start_link() ->
  pg2:create(?MODULE),
  Pid =
    case itweep:start_link({local, ?MODULE}, ?MODULE, [],
                           [{user, edit_util:get_env(twitter_stream_user)},
                            {password, edit_util:get_env(twitter_stream_password)}]) of
      {ok, P} -> P;
      {error, {already_started, P}} -> P
    end,
  ok = pg2:join(?MODULE, Pid),
  {ok, Pid}.

-spec stop() -> ok.
stop() ->
  itweep:call(closest_member(), stop).

-spec add_document(#edit_document{}) -> ok.
add_document(Document) ->
  itweep:call(closest_member(), {add, Document}, infinity).

-spec update_document(#edit_document{}) -> ok.
update_document(Document) ->
  itweep:call(closest_member(), {update, Document}, infinity).

-spec remove_document(document_id()) -> ok.
remove_document(DocId) ->
  itweep:call(closest_member(), {remove, DocId}, infinity).

-spec event_dispatcher() -> pid() | undefined.
event_dispatcher() ->
  case closest_member() of
    ?MODULE ->
      erlang:whereis(?DISPATCHER);
    Pid ->
      rpc:call(node(Pid), erlang, whereis, [edit_itweep_dispatcher])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ITWEEP FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @hidden
-spec init(Args::term()) -> {ok, state()}.
init([]) ->
  ?INFO("Initializing...~n", []),
  Dispatcher =
    case gen_event:start_link({local, ?DISPATCHER}) of
      {ok, P} -> P;
      {error, {already_started, P}} -> P
    end,
  ?INFO("Initialized (dispatcher: ~p)~n", [Dispatcher]),
  {ok, #state{}}.

%% @hidden
-spec handle_status(Status::itweet:tweet(), State::term()) -> {ok, state()}.
handle_status(Tweet, State) ->
  ok = gen_event:notify(?DISPATCHER, Tweet),
  {ok, State}.

%% @hidden
-spec handle_event(Event::atom(), Data::itweet_mochijson2:json_object(), State::term()) -> {ok, state()}.
handle_event(delete, JsonObj, State) ->
  case itweet_mochijson2:get_value(<<"status">>, JsonObj, null) of
    null ->
      ok;
    JsonStatus ->
      case {itweet_mochijson2:get_value(<<"user_id_str">>, JsonStatus, null),
            itweet_mochijson2:get_value(<<"id_str">>, JsonStatus, null)} of
        {undefined, _} ->
          ok;
        {_, undefined} ->
          ok;
        {UserId, MsgId} ->
          ?INFO("delete: twitter-~s-~s~n", [UserId, MsgId])
          %%TODO: Honour deletes
      end
  end,
  {ok, State};
handle_event(Event, Data, State) ->
  ?INFO("~p: ~p~n", [Event, Data]),
  {ok, State}.

%% @hidden
-spec handle_call(Msg::term(), From::reference(), State::term()) -> {ok, itweep:method(), ok, state()} | {ok, ok, state()}.
handle_call(stop, _From, State = #state{documents = Documents}) ->
  ?INFO("Stopping, docs: ~p~n", [[DocId || #edit_document{id = DocId} <- Documents]]),
  {stop, normal, ok, State};
handle_call({add, Document}, _From, State = #state{documents = Documents}) ->
  ?INFO("Adding ~s~n", [Document#edit_document.id]),
  case lists:keymember(Document#edit_document.id, #edit_document.id, Documents) of
    true ->
      ?INFO("~s was already tracked~n", [Document#edit_document.id]),
      {ok, ok, State};
    false ->
      NewDocuments = [Document|Documents],
      ?INFO("~s added, tracking ~p~n", [Document#edit_document.id, [Id || #edit_document{id = Id} <- NewDocuments]]),
      {ok, method(NewDocuments), ok, State#state{documents = NewDocuments}}
  end;
handle_call({update, Document}, _From, State = #state{documents = Documents}) ->
  ?INFO("Updating ~s~n", [Document#edit_document.id]),
  NewDocuments = lists:keystore(Document#edit_document.id, #edit_document.id, Documents, Document),
  ?INFO("~s updated, tracking ~p~n", [Document#edit_document.id, [Id || #edit_document{id = Id} <- NewDocuments]]),
  {ok, method(NewDocuments), ok, State#state{documents = NewDocuments}};
handle_call({remove, DocId}, _From, State = #state{documents = Documents}) ->
  ?INFO("Removing ~s~n", [DocId]),
  case lists:keydelete(DocId, #edit_document.id, Documents) of
    Documents ->
      ?INFO("~s wasn't tracked~n", [DocId]),
      {ok, ok, State};
    NewDocuments ->
      ?INFO("~p removed, tracking ~p~n", [DocId, [Id || #edit_document{id = Id} <- NewDocuments]]),
      {ok, method(NewDocuments), ok, State#state{documents = NewDocuments}}
  end.

%% @hidden
-spec handle_info(Msg::term(), State::term()) -> {ok, state()}.
handle_info(Msg, State) ->
  ?INFO("~p~n", [Msg]),
  {ok, State}.

%% @hidden
-spec terminate(Reason :: normal | shutdown | term(), State::term()) -> _.
terminate(Reason, _State) ->
  ?WARN("Terminating: ~p~n", [Reason]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
closest_member() ->
  case pg2:get_closest_pid(?MODULE) of
    Pid when is_pid(Pid) -> Pid;
    Error ->
      ?WARN("Couldn't find ~p's closest member on pg2: ~p~n\tDefaulting to the local instance...~n", [?MODULE, Error]),
      ?MODULE
  end.

method([]) ->
  rest;
method(Documents) ->
  Words =
      sets:from_list([binary_to_list(HashTag) || #edit_document{hash_tags = HashTags} <- Documents,
                                                 HashTag <- HashTags]),
  Users =
      sets:from_list([UserId || #edit_document{users = DocUsers} <- Documents,
                                #edit_user{id = UserId} <- DocUsers]),
  case {sets:to_list(Words), sets:to_list(Users)} of
    {[], []} ->
      ?INFO("Nothing to follow or track. Docs: ~p~n", [[DocId || #edit_document{id = DocId} <- Documents]]),
      rest;
    {[], UserList} ->
      ?INFO("Following~n\t~p~n", [UserList]),
      {"filter", [{follow, UserList}]};
    {WordList, []} ->
      ?INFO("Tracking~n\t~p~n", [WordList]),
      {"filter", [{track, WordList}]};
    {WordList, UserList} ->
      ?INFO("Tracking~n\t~p~nFollowing~n\t~p~n", [WordList, UserList]),
      {"filter", [{track, WordList}, {follow, UserList}]}
  end.