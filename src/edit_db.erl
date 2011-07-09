-module(edit_db).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DOC_TABLE,edit_documents).
-define(USERS_TABLE,edit_users).
-define(VERSIONS_TABLE,edit_versions).

%% API layer for weedit

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-record(dv, {document,version,patch,user}).
-record(document, {url,title,owner,users,searches}).
-record(user, {id,username}).

-export([start_link/0]).
%%-export([create_document/2,add_version/3,remove_version/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_document() -> 
  Name = gen_server:call(?SERVER,new_document_url).

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
  ets:new(?DOC_TABLE,[protected,set,named_table]),
  ets:new(?USERS_TABLE,[protected,set,named_table]),
  ets:new(?VERSIONS_TABLE,[protected,set,named_table]).

random_document_url() -> 
  Name = lists:flatten(lists:foldl(fun(X,AccIn) ->
        [random:uniform(25) + 96|AccIn] end,
        [], lists:seq(1,10))),
  case ets:member(?DOC_TABLE, Name) of 
    true  -> random_document_url();
    false -> Name
  end.


