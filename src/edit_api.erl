-module(edit_api).
-export([handle_command/3]).

-include("elog.hrl").
-include("socketio.hrl").

handle_command(<<"login">>, DocId, Data) -> 
  noreply;
 
handle_command(<<"title">>, DocId, Data) -> 
  TitleDiff = edit_util:safe_term_to_binary(proplists:get_value(<<"diff">>, Data, <<>>)),
  edit_document:edit_title(DocId,TitleDiff),
  noreply;
 
handle_command(<<"doc">>, DocId, Data) -> 
  TitleDiff = edit_util:safe_term_to_binary(proplists:get_value(<<"diff">>, Data, <<>>)),
  edit_document:edit_doc(DocId,TitleDiff),
  noreply.

