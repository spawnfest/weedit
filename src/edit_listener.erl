-module(edit_listener).
-behaviour(gen_event).
-define(SERVER, ?MODULE).

%% API
-export([start_link/1, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([handle_request/3]).

-include("elog.hrl").
-include("socketio.hrl").

-record(state, {port}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
start_link(Port) ->
  {ok, Pid} = socketio_listener:start([{http_port, Port},
                                       {default_http_handler, ?MODULE}]),
  ok = gen_event:add_handler(socketio_listener:event_manager(Pid), ?MODULE, Port),
  {ok, Pid}.

%%--------------------------------------------------------------------
%% @spec add_handler() -> ok | {'EXIT',Reason} | term()
%% @doc Adds an event handler
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}
%% @doc Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end
%%--------------------------------------------------------------------
init(Port) -> {ok, #state{port = Port}}.

%%--------------------------------------------------------------------
%% @spec 
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% @doc an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%% @end
%%--------------------------------------------------------------------
handle_event({client, Pid}, State) ->
  ok = edit_client_handler:start(Pid),
  {ok, State};
handle_event({message, ClientPid, Smsg}, State) ->
  Cmd = case Smsg of
          #msg{content = MsgProps, json = true} -> todo;
          #msg{content = Text, json = false} -> todo
        end,
  {ok, State};

handle_event(Event, State) ->
  ?INFO("Ignored socketio event: ~p~n", [Event]),
  {ok, State}.
  
%%--------------------------------------------------------------------
%% @spec handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @spec
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_request('GET', [], Req) ->
  handle_request('GET', ["index.html"], Req);
handle_request('GET', Path, Req) ->
  ?INFO("~p~n", [Path]),
  Req:file(filename:join(["www"| Path])).
