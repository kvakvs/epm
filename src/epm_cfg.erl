%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Configuration global storage
%%% @end
%%% Created : 08. Feb 2014 1:56 PM
%%%-------------------------------------------------------------------
-module(epm_cfg).

%% API
-export([init/1
        , set/2
        , get/1
        , get/2
        , get_all/0
        , print_config_values/0, write_config_file/0, delete/1]).

-include("epm.hrl").
-define(cfg_table, epm_cfg).

init(EpmHome) ->
  ets:new(?cfg_table, [named_table]),

  Home = epm_util:home_dir(),
  set(vsn, ?epm_version),

  %% consult global .epm config file in home directory
  %% TODO: Allow epm.conf to override dir-local -> user-global -> system-global
  GlobalConfig = case file:consult(filename:join([EpmHome, "epm.conf"])) of
      {ok, [C]} ->
        io:format("epm v~s, ~p~n~n", [?epm_version, ?epm_year]),
        C;
      {ok, []} -> [];
      {error, enoent} ->
        file:write_file(filename:join([Home, ".epm"]), <<>>),
        [];
      {error, Reason} ->
        ?EPM_FAIL("failed to read epm global config: ~p", [Reason])
    end,
  lists:foreach(fun({K, V}) -> set(K, V) end, GlobalConfig),
  case ?MODULE:get(install_dir) of
    {error, not_found} ->
      epm:p(dark_red, "Warning"),
      epm:p("You have not specified a value for install_dir in your .epm "
      "config file.~n'deps/' will be used.~nRun `epm config --set "
      "install_dir <path>`~n"),
      ?MODULE:set(install_dir, "deps/"),
      ok;
    {ok, _} -> ok
  end.

set(Key, Value) ->
  ets:insert(?cfg_table, {Key, Value}).

get(Key) ->
  case ets:lookup(?cfg_table, Key) of
    [] -> {error, not_found};
    [{Key, Value}] -> {ok, Value}
  end.

delete(Key) ->
  ets:delete(?cfg_table, Key).

get(Key, Default) ->
  case ets:lookup(?cfg_table, Key) of
    [] -> Default;
    [{Key, Value}] -> Value
  end.

get_all() ->
  ets:tab2list(?cfg_table).

%% -----------------------------------------------------------------------------
%% Global Config
%% -----------------------------------------------------------------------------
print_config_values() ->
  [io:format("~p\t\t~p~n", [K,V]) || {K,V} <- epm_cfg:get_all()].

write_config_file() ->
  {ok, FileLoc} = epm_cfg:get(global_config),
  case file:open(FileLoc, [write]) of
    {ok, IoDevice} ->
      io:format(IoDevice, "[~n", []),
      F = fun({Key, Val}, Count) ->
        if Count == 0 -> ok;
          true -> io:format(IoDevice, ",~n", [])
        end,
        case {Key, Val} of
          {repo_plugins, [C|_]} when is_integer(C) ->
            io:format(IoDevice, "  {~p, ~s}", [Key, Val]);
          _ ->
            io:format(IoDevice, "  {~p, ~p}", [Key, Val])
        end,
        Count + 1
      end,
      lists:foldl(F, 0, get_all()),
      io:format(IoDevice, "~n].~n", []),
      io:format("+ updated .epm config~n");
    {error, Reason} ->
      ?EPM_FAIL("failed to update .epm config (~s): ~p", [FileLoc, Reason])
  end.