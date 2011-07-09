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

-record(state, {}).

start(Pid) ->
  gen_event:add_handler(socketio_client:event_manager(Pid), ?MODULE, []).

init([]) -> {ok, #state{}}.

handle_event({message, ClientPid, SMsg}, State) ->
  {Command, Data} =
      case SMsg of
        #msg{content = MsgProps, json = true} ->
          {edit_util:safe_term_to_binary(proplists:get_value(<<"action">>, MsgProps, <<>>)),MsgProps};
        #msg{content = Text, json = false} ->
          {text, Text}
      end,
      case edit_api:handle_command(Command, Data) of
          {ok, Response} -> 
            socketio_client:send(ClientPid,
             #msg{json = true,
              content = [{<<"error">>, false},
                     {<<"result">>, iolist_to_binary(io_lib:format("~p", [Response]))}]});
          {error, Why } -> 
             #msg{json = true,
                  content = [{<<"error">>, true},
                         {<<"result">>, iolist_to_binary(io_lib:format("~p", [Why]))}]}
      end,
  {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

