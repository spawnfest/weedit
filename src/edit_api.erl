-module(edit_api).
-export([handle_command/2]).

-include("elog.hrl").
-include("socketio.hrl").

handle_command(<<"title">>,Data) -> 
  ?INFO("~p~n",[{Action,Data}]),
  {ok,<<"thanks">>}.

