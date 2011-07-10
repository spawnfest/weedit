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
  ?INFO("got hello for ~p to ~s~n",[ClientPid, DocId]),
  edit_document:ensure_started(DocId),
  gen_event:add_sup_handler(edit_document:event_dispatcher(DocId), edit_client_handler, [ClientPid]),
  #edit_document{users = Users, hash_tags = HashTags} = edit_document:document(DocId),
  socketio_client:send(ClientPid,
                       #msg{json = true,
                            content = [{<<"error">>, false},
                                       {<<"action">>,<<"set_users">>},
                                       {<<"users">>, lists:map(fun edit_util:to_jsx/1, Users)}
                                      ]}),
  socketio_client:send(ClientPid,
                       #msg{json = true,
                            content = [{<<"error">>, false},
                                       {<<"action">>,<<"set_hash_tags">>},
                                       {<<"tags">>,HashTags}
                                      ]}),
  proc_lib:spawn(fun() ->
                         lists:foreach(
                           fun({_TS, tweet, Tweet}) ->
                                   socketio_client:send(ClientPid,
                                                        #msg{json = true,
                                                             content = [{<<"error">>,  false},
                                                                        {<<"action">>, <<"tweet">>},
                                                                        {<<"tweet">>,  edit_util:mochi_to_jsx(Tweet)}
                                                                       ]});
                              (_) ->
                                   ok
                           end, edit_db:history(DocId))
                 end),
  noreply;
handle_command(_ClientPid, <<"login">>, DocId, Data) ->
  User =
      case {proplists:get_value(<<"id">>, Data, null),
            proplists:get_value(<<"username">>, Data, null)} of
        {null, null} ->
          ?WARN("BAD LOGIN: ~p~n", [Data]);
        {null, Username} ->
          #edit_user{id = -1, username = Username};
        {Id, null} ->
          #edit_user{id = Id, username = undefined};
        {Id, Username} ->
          #edit_user{id = Id, username = Username}
      end,
  ?INFO("~s logged in to ~s~n", [User, DocId]),
  edit_document:login(DocId, i_want_my_message_back, User),
  noreply;
handle_command(_ClientPid, <<"set_hash_tags">>, DocId, Data) -> 
  HashTags =
      case proplists:get_value(<<"tags">>, Data, []) of
        L when is_list(L) -> L;
        <<>> -> [];
        B when is_binary(B) -> [B]
      end,
  ?INFO("~p wants to set hashtags of ~s to ~p ~n", [_ClientPid, DocId, HashTags]),
  edit_document:set_hash_tags(DocId, i_want_my_message_back, HashTags),
  noreply;
handle_command(ClientPid, <<"edit_title">>, DocId, Data) -> 
  TitleDiffB64 = proplists:get_value(<<"diff">>, Data, <<>>),
  TitleDiffText = base64:decode(TitleDiffB64),
  TitleDiff = itweet_mochijson2:decode(TitleDiffText),
  ?INFO("~p sends title diff for ~s~n", [ClientPid, DocId]),
  edit_document:edit_title(DocId,ClientPid,TitleDiff),
  noreply;
handle_command(ClientPid, <<"edit_body">>, DocId, Data) -> 
  DocDiffB64 = proplists:get_value(<<"diff">>, Data, <<>>),
  DocDiffText = base64:decode(DocDiffB64),
  DocDiff = itweet_mochijson2:decode(DocDiffText),
  ?INFO("~p sends doc diff for ~s~n", [ClientPid, DocId]),
  edit_document:edit_body(DocId,ClientPid,DocDiff),
  noreply.