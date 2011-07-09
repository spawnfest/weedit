-module(client_handler).
-behaviour(gen_event).
-define(SERVER, ?MODULE).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-include("elog.hrl").
-include("socketio.hrl").
-include("misultin.hrl").

-record(state, {}).

start(Pid) ->
  gen_event:add_handler(socketio_client:event_manager(Pid), ?MODULE, []).

init([]) -> {ok, #state{}}.

handle_event({message, ClientPid, SocketIoMsg}, State) ->
ok.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

