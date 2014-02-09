%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents local installed package index and remote package index plus
%%% manipulations (search, dependencies etc).
%%% @end
%%% Created : 08. Feb 2014 2:47 PM
%%%-------------------------------------------------------------------
-module(epm_index).

%% API
-export([open/2
        , delete_local/1
        , insert_local/2
        , close/0
        , is_installed/1]).
-export([list_local_packages/0
        , list_global_packages/0
        , list_global_matching/1
        , list_local_matching/1
        ]).

-include("epm.hrl").

-define(local_index, epm_local_index).
-define(local_index_file, "epm_local_index").

-define(global_index, epm_index).
-define(global_index_file, "epm_packages").

%% -define(global_repo_index, epm_repo_index).
%% -define(global_repo_index_file, "epm_repos").

%%------------------------------------------------------------------------------
is_installed(Id=#pkgid{}) ->
  [] =/=  list_local_matching(Id).


close() -> ok.


open(_Home, EpmHome) ->
  G1 = open_dets_file(filename:join([EpmHome, ?global_index_file])),
  ets:new(?global_index, [named_table, {keypos, #pkg.id}]),
  lists:foreach(fun(X1) -> ets:insert(?global_index, X1) end, G1),

  %% Fixtures
  CowboyId = #pkgid{author="extend", pkg_name="cowboy", vsn="2.7", platform=x64},
  Cowboy2Id = #pkgid{author="derp", pkg_name="cowboy", vsn="2.6"},
  RanchId = #pkgid{pkg_name="ranch", vsn="1.1"},
  GunId = #pkgid{pkg_name="gun", vsn="0.1-dev", platform=x86},
  FwId = #pkgid{pkg_name="farwest", vsn="1a"},
  OtherId = #pkgid{pkg_name="other", erlang_vsn="r13b"},
  ets:insert(?global_index, #pkg{id=CowboyId, deps=[RanchId]}),
  ets:insert(?global_index, #pkg{id=Cowboy2Id, deps=[RanchId]}),
  ets:insert(?global_index, #pkg{id=RanchId, deps=[GunId]}),
  ets:insert(?global_index, #pkg{id=GunId}),
  ets:insert(?global_index, #pkg{id=FwId}),
  ets:insert(?global_index, #pkg{id=OtherId}),

  G2 = open_dets_file(?local_index_file),
  ets:new(?local_index, [named_table, {keypos, #pkg.id}]),
  lists:foreach(fun(X2) -> ets:insert(?local_index, X2) end, G2),
  ets:insert(?local_index, #pkg{id=GunId}),

  State = #epm_state{},
  State.

%% @doc List all installed packages in local index
list_local_packages() ->
  ets:match(?local_index, '$1').

%% @doc List all available packages in global index
list_global_packages() ->
  ets:match(?global_index, '$1').

%% @doc Search local index (installed)
list_local_matching(Id=#pkgid{}) ->
  Q = epm:pkgid_match_spec(Id),
  ets:select(?local_index, Q).

list_global_matching(Id=#pkgid{}) ->
  Q = epm:pkgid_match_spec(Id),
  ets:select(?global_index, Q).

delete_local(Key={_User, _Name, _Vsn}) ->
  ets:delete(?global_index, Key).

insert_local(Key={_User, _Name, _Vsn}, Package=#pkg{}) ->
  ets:insert(?global_index, {Key, Package}).

%% @private
open_dets_file(File) ->
  case dets:open_file(epm_temporary, [{type, set}, {file, File}]) of
    {ok, _} -> ok;
    {error, {file_error, _, eacces}} ->
      ?EPM_FAIL("insufficient access to epm index file: ~s", [File]);
    {error, Reason} ->
      ?EPM_FAIL("failed to open epm index file (~s): ~p", [File, Reason])
  end,
  %% Load everything and return, to be inserted to ETS or used later
  Objects = dets:match(epm_temporary, '$1'),
  dets:close(epm_temporary),
  Objects.