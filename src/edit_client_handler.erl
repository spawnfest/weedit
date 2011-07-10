%%-------------------------------------------------------------------
%% @author C B DePue <chad@inakanetworks.com>
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit client handler
%% @end
%%-------------------------------------------------------------------
-module(edit_client_handler).
-behaviour(gen_event).
-define(SERVER, ?MODULE).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start/1]).

-include("elog.hrl").
-include("socketio.hrl").
-include("misultin.hrl").

-record(state, {client_pid = none :: none | pid()}).
-opaque state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
-spec start(pid()) -> ok.
start(Pid) ->
  gen_event:add_handler(socketio_client:event_manager(Pid), ?MODULE, []).

%% ------------------------------------------------------------------
%% Behaviour Callbacks
%% ------------------------------------------------------------------
%% @private
-spec init([pid()]) -> {ok, state()}.
init([]) -> {ok, #state{}};
init([ClientPid]) -> {ok, #state{client_pid=ClientPid}}.

%% @private
-spec handle_event({message, pid(), #msg{}} | {outbound_message, binary(), [proplists:property()], term()}, state()) -> {ok, state()}.
handle_event({message, ClientPid, SMsg}, State) ->
  {Command, DocumentId, Data} =
      case SMsg of
        #msg{content = MsgProps, json = true} ->
          {proplists:get_value(<<"action">>, MsgProps, <<>>),
           binary_to_list(proplists:get_value(<<"doc_id">>, MsgProps, <<>>)),
           MsgProps};
        #msg{content = Text, json = false} ->
          {text, unknown, Text}
      end,
  ?INFO("~p~n", [Data]),
  noreply = edit_api:handle_command(ClientPid, Command, DocumentId, Data),
  {ok, State};
handle_event({outbound_message, Action, MessagePropList, FromClientPid}, State) ->
  ClientPid = State#state.client_pid,
  case ClientPid of
    none -> 
      ?INFO("oh no we got a mesage for a client but we don't know how to phone home: ~p: ~p ~n",[Action,MessagePropList]),
      noop;
    FromClientPid -> noop; %% the result of a message from ourselves, eat it...
    Pid -> 
      ?INFO("OUTBOUND MESSAGE FOR ~p: ~s~n", [ClientPid, Action]),
      ?DEBUG("OUTBOUND PROPLIST~n\t~p~n", [MessagePropList]),
      socketio_client:send(Pid,
                           #msg{json = true,
                                content = [
                                           {<<"error">>, false},
                                           {<<"action">>,Action} | MessagePropList
                                          ]})
  end,
  {ok, State};
handle_event({document_EXIT, DocId, Reason}, State) ->
  ?WARN("TODO DOC ~s EXIT: ~p", [DocId, Reason]),
  {ok,State}.

%% @private
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.

%% @private
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) -> {ok, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(stop, _State) -> ok;
terminate({stop, normal}, _State) -> ok;
terminate(Reason, _State) ->
  ?WARN("TERMINATE: ~p~n", [Reason]),
  ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
