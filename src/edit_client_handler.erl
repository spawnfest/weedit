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

start(Pid) ->
  gen_event:add_handler(socketio_client:event_manager(Pid), ?MODULE, []).

init([]) -> {ok, #state{}};
init([ClientPid]) -> {ok, #state{client_pid=ClientPid}}.

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
      ?INFO("OUTBOUND MESSAGE FOR ~p: ~s ~n", [ClientPid, Action]),
      socketio_client:send(Pid,
                           #msg{json = true,
                                content = [
                                           {<<"error">>, false},
                                           {<<"action">>,Action} | MessagePropList       
                                          ]})
  end,
  {ok, State};

handle_event({document_EXIT, DocId, Reason}, State) ->
  ?INFO("TODO DOC EXIT",[]),
  {ok,State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(Info, State) ->
  ?INFO("INFO: ~p~n", [Info]),
  {ok, State}.

terminate(Reason, _State) ->
  ?INFO("TERMINATE: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
