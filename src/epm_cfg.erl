%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Configuration global storage
%%% @end
%%% Created : 08. Feb 2014 1:56 PM
%%%-------------------------------------------------------------------
-module(epm_cfg).

%% API
-export([init/1, set/2, get/1, get/2]).

-define(cfg_table, epm_cfg).

init(Config) ->
  ets:new(?cfg_table, [named_table]),
  lists:foreach(fun({K, V}) -> set(K, V) end, Config).

set(Key, Value) ->
  ets:insert(?cfg_table, {Key, Value}).

get(Key) ->
  case ets:lookup(?cfg_table, Key) of
    [] -> {error, not_found};
    [{Key, Value}] -> {ok, Value}
  end.

get(Key, Default) ->
  case ets:lookup(?cfg_table, Key) of
    [] -> Default;
    [{Key, Value}] -> Value
  end.
