%%%'   HEADER
%%% @author C B DePue <chad@inakanetworks.com> 
%%% @since 
%%% @copyright 2011 C B DePue
%%% @doc 
%%% @end

-module(edit_app).
-behaviour(application).

-export([start/2, stop/1]).
%%%.
%%%'   CALLBACKS
start(_StartType, _StartArgs) ->
  edit_sup:start_link().

stop(_State) ->
  ok.
%%%.
%%%'   PRIVATE FUNCTIONS

%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
