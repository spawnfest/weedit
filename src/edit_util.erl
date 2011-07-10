%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc WeEdit utility functions
%% @end
%%-------------------------------------------------------------------
-module(edit_util).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-include("elog.hrl").
-include("edit_records.hrl").

-export([get_env/1, set_env/2, random_id/0, safe_term_to_binary/1, to_lower/1, now/0]).
-export([mochi_to_jsx/1, to_jsx/1, to_mochi/1]).

%% @doc Returns application:get_env(edit, Field) or its default value
-spec get_env(atom()) -> term().
get_env(Field) ->
  case application:get_env(edit, Field) of
    {ok, Value} ->
      ?DEBUG("~p := ~p~n", [Field, Value]),
      Value;
    _ ->
      Value = get_env_default(Field),
      ?DEBUG("~p := ~p~n", [Field, Value]),
      Value
  end.

%% @private
-spec get_env_default(atom()) -> term().
get_env_default(socketio_port_range) -> {12001, 12001};
get_env_default(riak_server) -> "127.0.0.1";
get_env_default(riak_port) -> 8087;
get_env_default(riak_bucket_prefix) -> <<"edit-">>;
get_env_default(Field) ->
  throw({env_undefined, Field}).

%% @equiv application:set_env(edit, Field, Value)
-spec set_env(atom(), term()) -> ok.
set_env(Field, Value) ->
  application:set_env(edit, Field, Value).

-spec random_id() -> string().
random_id() -> 
  lists:flatten(
    lists:foldl(
      fun(_,AccIn) ->
              [random:uniform(25) + 96|AccIn] end,
      [], lists:seq(1,10))).

-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
  to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
  Acc;
to_lower(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
  to_lower(Rest, <<Acc/binary, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 128 =< C, C =< 150 -> %% A-0 with tildes plus enye
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 152 =< C, C =< 158 -> %% U and Y with tilde plus greeks
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
  to_lower(Rest, <<Acc/binary, C>>).

-spec safe_term_to_binary(term()) -> binary().
safe_term_to_binary(I) when is_integer(I) ->
  list_to_binary(integer_to_list(I));
safe_term_to_binary(L) when is_tuple(L) -> 
  <<>>;
safe_term_to_binary(L) when is_list(L) ->
  unicode:characters_to_binary(L);
safe_term_to_binary(undefined) -> 
  <<>>;
safe_term_to_binary(A) when is_atom(A) -> 
  list_to_binary(atom_to_list(A));
safe_term_to_binary(A) when is_binary(A) -> A.

-spec mochi_to_jsx(itweet_mochijson2:json_object()) -> jsx:eep0018().
mochi_to_jsx(Object) ->
  jsx:json_to_term(iolist_to_binary(itweet_mochijson2:encode(Object))).

-spec to_jsx(term()) -> jsx:eep0018().
to_jsx(#edit_user{id = Id, username = Name}) ->
  [{<<"id">>, Id}, {<<"username">>, Name}].

-spec to_mochi(term()) -> itweet_mochijson2:json_object().
to_mochi(#edit_user{id = Id, username = Name}) ->
  {[{<<"id">>, Id}, {<<"username">>, case Name of
                                       undefined -> null;
                                       Name -> Name
                                     end}]};
to_mochi(#edit_document{body = Body, hash_tags = HashTags, id = Id, users = Users, title = Title}) ->
  {[{<<"id">>,        edit_util:safe_term_to_binary(Id)},
    {<<"title">>,     Title},
    {<<"body">>,      Body},
    {<<"hash-tags">>, HashTags},
    {<<"users">>,     lists:map(fun to_mochi/1, Users)}]}.

-spec now() -> integer().
now() ->
  {_, _, MicroSecs} = erlang:now(),
  Millis = erlang:trunc(MicroSecs/1000),
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()) * 1000 + Millis.