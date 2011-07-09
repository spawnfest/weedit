%%% @author C B DePue <chad@inakanetworks.com>
%%% @since 
%%% @copyright 2011 C B DePue
%%% @doc 
%%% @end
-module(edit_doc_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
%%%.
%%%'   PUBLIC API
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%.
%%%'   CALLBACKS
init([]) ->
  DocSpec = {edit_document, {edit_document, start_link, [] },
    transient, 5000, worker, [edit_document]}, 
  StartSpecs = {{simple_one_for_one, 5, 1}, [DocSpec]}, 
  {ok, StartSpecs}.

