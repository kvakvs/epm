%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Configuration global storage
%%% @end
%%% Created : 08. Feb 2014 1:56 PM
%%%-------------------------------------------------------------------
-module(epm_cfg).

%% API
-export([]).

init(Config) ->
  ets:new().

set(Key, Value) ->
