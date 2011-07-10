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
-export([update/3]).

-record(state, {riak                :: pid(),
                riak_bucket_prefix  :: binary()
               }).
-opaque state() :: #state{}.

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

-spec update(#edit_document{}, atom(), term()) -> ok.
update(Document, Type, Patch) -> 
  ?INFO("db: ~p to ~s~n", [Type, Document#edit_document.id]),
  gen_server:cast(?MODULE, {update, Document, Type, Patch}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%% @private
-spec init(document_id()) -> {ok, state()}.
init([]) ->
  _ = random:seed(erlang:now()),
  case riakc_pb_socket:start_link(edit_util:get_env(riak_server),
                                  edit_util:get_env(riak_port),
                                  []) of
    {ok, Pid} ->
      {ok, #state{riak = Pid,
                  riak_bucket_prefix = edit_util:get_env(riak_bucket_prefix)}};
    {error, Reason} ->
      {stop, Reason}
  end.

%% @private
-spec handle_call(term(), reference(), state()) -> {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(create_document, _From, State) ->
  DocId = random_document_id(),
  {reply, {ok, DocId}, State};
handle_call({document, DocId}, _From, State) ->
  case riakc_pb_socket:get(State#state.riak,
                           <<(State#state.riak_bucket_prefix)/binary, "documents">>,
                           edit_util:safe_term_to_binary(DocId)) of
    {ok, RiakObj} ->
      case riakc_obj:get_content_type(RiakObj) of
        "application/json" ->
          try
            JsonObj = itweet_mochijson2:decode(riakc_obj:get_value(RiakObj)),
            Document = #edit_document{id = binary_to_list(itweet_mochijson2:get_value(<<"id">>, JsonObj)),
                                      body = itweet_mochijson2:get_value(<<"body">>, JsonObj),
                                      hash_tags = itweet_mochijson2:get_value(<<"hash-tags">>, JsonObj),
                                      title = itweet_mochijson2:get_value(<<"title">>, JsonObj),
                                      users = lists:map(
                                                fun(UserJsonObj) ->
                                                        #edit_user{id = itweet_mochijson2:get_value(<<"id">>, UserJsonObj),
                                                                   username = itweet_mochijson2:get_value(<<"username">>, JsonObj)}
                                                end, itweet_mochijson2:get_value(<<"users">>, JsonObj, []))
                                     },
            {reply, {ok, Document}, State}
          catch
            _:Reason ->
              throw({error, Reason})
          end;
        Ctype ->
          throw({error, {unknown_ctype, Ctype}})
      end;
    {error, Error} ->
      {reply, {error, Error}, State};
    Other ->
      {reply, {error, Other}, State}
  end.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({ensure_document, DocId}, State) ->
  case riakc_pb_socket:get(State#state.riak,
                           <<(State#state.riak_bucket_prefix)/binary, "documents">>,
                           edit_util:safe_term_to_binary(DocId)) of
    {ok, _} ->
      {noreply, State};
    _ ->
      handle_cast({update, #edit_document{id = DocId}, creation, null}, State)
  end;
handle_cast({update, Document, Type, Patch}, State) ->
  DRiakValue = iolist_to_binary(itweet_mochijson2:encode(edit_util:to_mochi(Document))),
  DRiakItemId = edit_util:safe_term_to_binary(Document#edit_document.id),
  DRiakObj =
    case riakc_pb_socket:get(State#state.riak,
                             <<(State#state.riak_bucket_prefix)/binary, "documents">>,
                             DRiakItemId) of
      {ok, DR} ->
        riakc_obj:update_value(DR, DRiakValue, "application/json");
      _ ->
        riakc_obj:new(<<(State#state.riak_bucket_prefix)/binary, "documents">>, DRiakItemId,
                      DRiakValue, "application/json")
    end,
  case riakc_pb_socket:put(State#state.riak, DRiakObj, []) of
    ok ->
      TS = edit_util:now(),
      JSONUpdate =
          {[{<<"edit-timestamp">>,  TS},
            {<<"edit-type">>,       edit_util:safe_term_to_binary(Type)},
            {<<"edit-patch">>,      Patch}]},
      RiakValue = iolist_to_binary(itweet_mochijson2:encode(JSONUpdate)),
      RiakObj =
          riakc_obj:new(<<(State#state.riak_bucket_prefix)/binary,
                          (edit_util:safe_term_to_binary(Document#edit_document.id))/binary,
                          "-updates">>,
                        edit_util:safe_term_to_binary(TS), RiakValue, "application/json"),
      case riakc_pb_socket:put(State#state.riak, RiakObj, [if_none_match]) of
        ok ->
          ?INFO("Stored: ~p~n", [TS]),
          {noreply, State};
        {error, <<"match_found">>} ->
          ?WARN("Duplicated: ~p~n", [TS]),
          {noreply, State};
        Error ->
          {stop, Error, State}
      end;
    Error ->
      {stop, Error, State}
  end.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info(_Info, State) -> {noreply, State}.

%% @private
-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
-spec terminate(any(), state()) -> any().
terminate(Reason, _State) ->
  case Reason of
    normal ->
      ?INFO("terminating~n", []);
    Reason ->
      ?WARN("terminating: ~p~n", [Reason])
  end,
  ok.


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