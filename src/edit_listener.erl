%%-------------------------------------------------------------------
%% @author C B DePue <chad@inakanetworks.com>
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit socket.io listener
%% @end
%%-------------------------------------------------------------------
-module(edit_listener).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('C B DePue <chad@inakanetworks.com>').

-behaviour(gen_event).

%% API
-export([start_link/1]).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).
%% misultin callbacks
-export([handle_request/3]).

-include("elog.hrl").
-include("socketio.hrl").
-include("edit_records.hrl").

-record(state, {port :: pos_integer()}).
-type state() :: #state{}.

%%-------------------------------------------------------------------
%% PUBLIC API
%%-------------------------------------------------------------------
%% @doc Starts a listener
-spec start_link(pos_integer()) -> {ok, pid()}.
start_link(Port) ->
  {ok, Pid} = socketio_listener:start([{http_port, Port},
                                       {default_http_handler, ?MODULE}]),
  ok = gen_event:add_handler(socketio_listener:event_manager(Pid), ?MODULE, Port),
  {ok, Pid}.

%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec init(pos_integer()) -> {ok, state()}.
init(Port) -> {ok, #state{port = Port}}.

%% @private
-spec handle_event({client|disconnect, pid()} | term(), state()) -> {ok, state()}.
handle_event({client, Pid}, State) ->
  ok = edit_client_handler:start(Pid),
  {ok, State};
handle_event({disconnect, Pid}, State) ->
  ?DEBUG("~p disconnecting...~n", [Pid]),
  {ok, State};
handle_event(Event, State) ->
  ?INFO("Ignored socketio event: ~p~n", [Event]),
  {ok, State}.
  
%% @private
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @private
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info(_Info, State) ->
    {ok, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec handle_request(atom(), [string()], term()) -> term().
handle_request('GET', [], Req) ->
  Req:file(filename:join(["www","index.html"]));

handle_request('GET', ["new"], Req) ->
  {ok, DocId} = edit_db:create_document(),
  Req:raw_headers_respond(302, "Location: /doc/" ++ DocId ++ [13,10], "Type this!");

%% all documents - rendered with title, tags and body to the latest version
handle_request('GET', ["doc", DocId] , Req) ->
  ?INFO("RENDERING DOC ~p ~n", [DocId]),
  {ok, _Pid} = edit_document:ensure_started(DocId),
  Document = edit_document:document(DocId),
  Mustaches = dict:from_list([{title,     binary_to_list(Document#edit_document.title)},
                              {body,      binary_to_list(Document#edit_document.body)},
                              {hash_tags, Document#edit_document.hash_tags}]),
  {ok, FileData} = file:read_file(filename:join(["www","document.html"])),
  Req:ok(mustache:render(FileData,Mustaches));

%% these are only here because matt and manuel are self hosting and can't properly path the files..
%% remove when they self host

handle_request('GET', [ "doc", "images" | Path] , Req) ->
  File = filename:join(["www/images" | Path]),
  Req:file(File);

handle_request('GET', [ "doc", "js" | Path] , Req) ->
  File = filename:join(["www/js" | Path]),
  Req:file(File);

handle_request('GET', [ "doc", "stylesheets" |  Path] , Req) ->
  File = filename:join(["www/stylesheets" | Path]),
  Req:file(File);

%% Handle everything else

handle_request('GET', Path, Req) ->
  Req:file(filename:join(["www"| Path])).