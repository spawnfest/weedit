%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Networks S.R.L.
%%% @doc Document event handler
%%% @end
%%%-------------------------------------------------------------------
-module(edit_document_handler).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_event).

-export([subscribe/1, unsubscribe/1, is_subscribed/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-include("elog.hrl").
-include("edit_records.hrl").

-record(state, {document              :: document_id(),
                accept_pattern = []   :: binary:cp(),
                processed_tweets = [] :: [binary()],
                stats = []            :: [{discarded | ignored | blocked | accepted | error | total, non_neg_integer()}]}).
-type state() :: #state{}.

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc Subscribes to a dispatcher. If it's already subscribed, the subscription is ignored.
-spec subscribe(#edit_document{}) -> ok.
subscribe(Document) ->
  ?DEBUG("Subscribing ~s~n", [Document#edit_document.id]),
  try
    Dispatcher = edit_itweep:event_dispatcher(),
    case lists:member({?MODULE, Document#edit_document.id}, gen_event:which_handlers(Dispatcher)) of
      true -> ok;
      false ->
        ok = edit_itweep:add_document(Document),
        gen_event:add_handler(Dispatcher, {?MODULE, Document#edit_document.id}, Document)
    end
  catch
    _:noproc ->
      ?ERROR("~s couldn't subscribe to edit_itweep. It is off~n", [Document#edit_document.id]),
      throw(couldnt_subscribe)
  end.

-spec is_subscribed(document_id()) -> boolean().
is_subscribed(DocId) ->
  try lists:member({?MODULE, DocId}, gen_event:which_handlers(edit_itweep:event_dispatcher()))
  catch
    _:noproc -> false
  end.

%% @doc Unsubscribes from the dispatcher. If it's not subscribed, the unsubscription is ignored.
-spec unsubscribe(document_id()) -> ok.
unsubscribe(DocId) ->
  ?DEBUG("Unsubscribing ~s~n", [DocId]),
  try
    Dispatcher = edit_itweep:event_dispatcher(),
    case gen_event:delete_handler(Dispatcher, {?MODULE, DocId}, normal) of
      ok ->
        case lists:member({?MODULE, DocId}, gen_event:which_handlers(Dispatcher)) of
          false ->
            edit_itweep:remove_document(DocId);
          true ->
            ok
        end;
      {error, module_not_found} ->
        ok;
      OtherError ->
        throw(OtherError)
    end
  catch
    _:noproc ->
      ok
  end.

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init(#edit_document{}) -> {ok, state()}.
init(Document) ->
  ?INFO("Init ~p for ~s~n", [?MODULE, Document#edit_document.id]),
  Pattern = binary:compile_pattern(Document#edit_document.hash_tags),
  {ok, #state{document       = Document,
              accept_pattern = Pattern}}.

%% @hidden
-spec handle_event(itweet:tweet(), state()) -> {ok, state()}.
handle_event(Tweet, State = #state{accept_pattern   = Pattern,
                                   document         = Document,
                                   processed_tweets = ProcessedTweets}) ->
  ?DEBUG("New twitter message for ~s~n", [Document#edit_document.id]),
  LowerCasedBody = edit_util:to_lower(itweet_mochijson2:get_value(<<"text">>, Tweet)),
  TweetId = itweet_mochijson2:get_value(<<"id_str">>, Tweet),
  TweepId =
      case itweet_mochijson2:get_value(<<"user">>, Tweet, null) of
        null -> -1;
        User -> itweet_mochijson2:get_value(<<"id">>, User, -1)
      end,
  case lists:member(TweetId, ProcessedTweets) of
    true ->
      {ok, State};
    false ->
      Result =
        case {binary:match(LowerCasedBody, Pattern),
              lists:keymember(TweepId, #edit_user.id, Document#edit_document.users)} of
          {nomatch, false} ->
            discarded;
          {_Match, true} ->
            edit_document:add_tweet(Document#edit_document.id, Tweet),
            accepted
        end,
      {ok, State#state{processed_tweets = [TweetId | ProcessedTweets],
                       stats = add_stat(Result, State#state.stats, Document#edit_document.id)}}
  end;
handle_event(Event, State) ->
  ?DEBUG("Ignored Event for ~s:~n\t~p~n", [State#state.document#edit_document.id, Event]),
  {ok, State}.

%% @hidden
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.
%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) -> {ok, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(Reason, #state{document = #edit_document{id = DocId}}) ->
  ?INFO("Event handler for ~s removed: ~p~n", [DocId, Reason]),
  ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Private functions
%% ====================================================================
add_stat(Result, Stats, DocId) ->
  Current = proplists:get_value(Result, Stats, 0),
  Total = proplists:get_value(total, Stats, 0),
  NewStats = lists:keystore(Result, 1,
                            lists:keystore(total, 1, Stats,
                                           {total, Total + 1}),
                            {Result, Current + 1}),
  case Total + 1 of
    NewTotal when NewTotal rem 50 =:= 0 ->
      ?INFO("~s twitter stats: ~w~n", [DocId, NewStats]);
    _ ->
      ok
  end,
  NewStats.