-module(edit_api).
-export([handle_command/4]).

-include("elog.hrl").
-include("socketio.hrl").

%% required so that we can subscribe to events for this document
handle_command(ClientPid, <<"hello">>, DocId, Data) -> 
  gen_event:add_handler(edit_document:event_dispatcher(DocId), edit_client_handler, [ClientPid]),
  noreply;

%% we need the twitter user's name?
handle_command(_ClientPid, <<"login">>, DocId, Data) -> 
  noreply;

handle_command(_ClientPid, <<"set_twitter">>, DocId, Data) -> 
  HashTags = edit_util:safe_term_to_binary(proplists:get_value(<<"hashtag">>, Data, <<>>)),
  edit_document:set_hash_tags(DocId,HashTags),
  noreply;
 
%% diff title
handle_command(_ClientPid, <<"title">>, DocId, Data) -> 
  TitleDiff = edit_util:safe_term_to_binary(proplists:get_value(<<"diff">>, Data, <<>>)),
  ?INFO("got title diff: ~p ~n",[TitleDiff]),
  edit_document:edit_title(DocId,TitleDiff),
  noreply;
 
handle_command(_ClientPid, <<"doc">>, DocId, Data) -> 
  DocDiff = edit_util:safe_term_to_binary(proplists:get_value(<<"diff">>, Data, <<>>)),
  edit_document:edit_doc(DocId,DocDiff),
  noreply.

