-module(edit_db).
-behaviour(gen_server).

-define(DOC_TABLE,edit_documents).
-define(URL_TABLE,edit_urls).
-define(USERS_TABLE,edit_users).
-define(VERSIONS_TABLE,edit_versions).

%% API layer for weedit

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).
-export([document/1,create_document/0,add_version/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_document() -> 
  {ok,Name} = gen_server:call(?MODULE,create_document).

document(Name) -> 
  {ok,todo}.

%% TODO:SPEC
add_version(Document,User,Patch) -> 
  gen_server:call(?MODULE,{add_version,Document,User,Patch}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  init_schema(),
  {ok, Args}.

handle_call(create_document, _From, State) ->
  Name = random_document_url(),
  {reply, {ok, Name}, State};

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
  ets:new(?URL_TABLE,[protected,set,named_table]),
  ets:new(?DOC_TABLE,[protected,set,named_table]),
  ets:new(?USERS_TABLE,[protected,set,named_table]),
  ets:new(?VERSIONS_TABLE,[protected,set,named_table]).

random_document_url() -> 
  Name = edit_util:random_url(),
  case ets:member(?URL_TABLE,{Name}) of 
    true  -> random_document_url();
    false -> 
      ets:insert(?URL_TABLE,{Name}),
      Name
  end.


