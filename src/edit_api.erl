%%-------------------------------------------------------------------
%% @hidden
%% @author C B DePue <chad@inakanetworks.com>
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit API for clients
%% @end
%%-------------------------------------------------------------------
-module(edit_api).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-author('C B DePue <chad@inakanetworks.com>').

-include("elog.hrl").
-include("socketio.hrl").
-include("edit_records.hrl").

-export([handle_command/4]).

%% @private
-spec handle_command(pid(), binary(), string(), [proplists:property()]) -> noreply.
handle_command(ClientPid, <<"hello">>, DocId, _Data) -> %% required so that we can subscribe to events for this document
  ?INFO("got hello from ~p ~n",[DocId]),
  edit_document:ensure_started(DocId),
  gen_event:add_sup_handler(edit_document:event_dispatcher(DocId), edit_client_handler, [ClientPid]),
  #edit_document{users = Users, hash_tags = HashTags} = edit_document:document(DocId),
  ?INFO("sending users to the new guy: ~p ~n",[Users]),
  socketio_client:send(ClientPid,
                       #msg{json = true,
                            content = [{<<"error">>, false},
                                       {<<"action">>,<<"set_users">>},
                                       {<<"users">>, lists:map(fun edit_util:to_jsx/1, Users)}
                                      ]}),
  ?INFO("sending hash tags to the new guy: ~p ~n",[Users]),
    socketio_client:send(ClientPid,
                         #msg{json = true,
                              content = [ {<<"error">>, false},
                                         {<<"action">>,<<"set_hash_tags">>},
                                         {<<"tags">>,HashTags}
                                        ]}),
  noreply;
handle_command(_ClientPid, <<"login">>, DocId, Data) ->
  ?INFO("wow in login: ~p ~p ~n",[DocId,Data]),
  User = #edit_user{id = proplists:get_value(<<"id">>, Data, -1),
                    username = proplists:get_value(<<"username">>, Data, <<>>)}, %%NOTE: we need the twitter user's name?
  ?INFO("got user of ~p ~n",[User]),
  edit_document:login(DocId, i_want_my_message_back, User),
  noreply;
handle_command(_ClientPid, <<"set_hash_tags">>, DocId, Data) -> 
  ?INFO("got hashtags of ~p ~n",[Data]),
  HashTags =
      case proplists:get_value(<<"tags">>, Data, []) of
        L when is_list(L) -> L;
        <<>> -> [];
        B when is_binary(B) -> [B]
      end,
  edit_document:set_hash_tags(DocId, i_want_my_message_back, HashTags),
  noreply;
handle_command(ClientPid, <<"edit_title">>, DocId, Data) -> 
  TitleDiffText = proplists:get_value(<<"diff">>, Data, <<>>),
  TitleDiff = itweet_mochijson2:decode(TitleDiffText),
  ?INFO("got title diff: ~p ~n",[TitleDiff]),
  edit_document:edit_title(DocId,ClientPid,TitleDiff),
  noreply;
handle_command(ClientPid, <<"edit_body">>, DocId, Data) -> 
  DocDiffText = proplists:get_value(<<"diff">>, Data, <<>>),
  DocDiff = itweet_mochijson2:decode(DocDiffText),
  ?INFO("got diff of document: ~p ~n",[DocDiff]),
  edit_document:edit_body(DocId,ClientPid,DocDiff),
  noreply.
