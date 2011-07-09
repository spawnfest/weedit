-module(edb).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DOC_TABLE,documents).

%% API layer for weedit

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([random_document_name/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


random_document_name() -> 
  Name = lists:flatten(lists:foldl(fun(X,AccIn) ->
        [random:uniform(25) + 96|AccIn] end,
        [], lists:seq(1,10))).
  case ets:member(?DOC_TABLE, Name) of 
    true  -> random_document_name();
    false -> Name
  end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
init_schema() -> 
  ets:new(?DOC_TABLE,[protected,set,named_table]).

