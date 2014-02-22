%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents local installed package index and remote package index plus
%%% manipulations (search, dependencies etc).
%%% @end
%%% Created : 08. Feb 2014 2:47 PM
%%%-------------------------------------------------------------------
-module(epm_index).

%% API
-export([open/1
        , delete_local/1
        , insert_local/2
        , close/0
        , is_installed/1
        , get_repo/1, get_pkg/1]).
-export([list_local_packages/0
        , list_global_packages/0
        , list_global_matching/1
        , list_local_matching/1
        ]).

-include("epm.hrl").

-define(local_pkgs, epm_local_index).
-define(local_pkgs_db, "epm_local_index").

-define(global_pkgs, epm_index).
-define(global_pkgs_db, "epm_packages").

-define(global_repos, epm_repos).
-define(global_repos_db, "epm_repos").

%%------------------------------------------------------------------------------
is_installed(Id=#pkgid{}) ->
  [] =/=  list_local_matching(Id).


close() -> ok.

open(EpmHome) ->
  create_and_load(EpmHome),

  %% Pkgid Fixtures
  CowboyId = #pkgid{author="extend", pkg_name="cowboy", vsn="2.7", platform=x64},
  Cowboy2Id = #pkgid{author="derp", pkg_name="cowboy", vsn="2.6"},
  RanchId = #pkgid{author="extend", pkg_name="ranch", vsn="1.1"},
  GunId = #pkgid{author="extend", pkg_name="gun", vsn="0.1-dev", platform=x86},
  FwId = #pkgid{author="extend", pkg_name="farwest", vsn="1a"},
  OtherId = #pkgid{pkg_name="other", erlang_vsn="r13b"},

  %% Repo Fixtures
  Github = #repoid{name="github"},
  ets:insert(?global_repos, #repo{ id=Github, api_module=epm_vcs_git
                                 , short_name="github" }),
  Git = #repoid{name="git"},
  ets:insert(?global_repos, #repo{ id=Git, api_module=epm_vcs_git
                                 , short_name="git" }),

  %% Pkg Fixtures
  ets:insert(?global_pkgs, #pkg{id=CowboyId, deps=[RanchId], repo=Github}),
  ets:insert(?global_pkgs, #pkg{id=Cowboy2Id, deps=[RanchId], repo=Github}),
  ets:insert(?global_pkgs, #pkg{id=RanchId, deps=[GunId], repo=Github}),
  ets:insert(?global_pkgs, #pkg{id=GunId, repo=Github}),
  ets:insert(?global_pkgs, #pkg{id=FwId, repo=Github}),
  ets:insert(?global_pkgs, #pkg{id=OtherId, repo=Git}),

  %ets:insert(?local_pkgs, #pkg{id=GunId}),

  State = #epm_state{},
  State.

get_repo(Id=#repoid{}) ->
  case ets:lookup(?global_repos, Id) of
    []     -> not_found;
    [Repo] -> Repo
  end.

get_pkg(Id=#pkgid{}) ->
  case ets:lookup(?global_pkgs, Id) of
    []    -> not_found;
    [Pkg] -> Pkg
  end.

%% @doc List all installed packages in local index
list_local_packages() ->
  ets:match(?local_pkgs, '$1').

%% @doc List all available packages in global index
list_global_packages() ->
  ets:match(?global_pkgs, '$1').

%% @doc Search local index (installed)
list_local_matching(Id=#pkgid{}) ->
  Q = epm:pkgid_match_spec(Id),
  ets:select(?local_pkgs, Q).

list_global_matching(Id=#pkgid{}) ->
  Q = epm:pkgid_match_spec(Id),
  ets:select(?global_pkgs, Q).

delete_local(Key={_User, _Name, _Vsn}) ->
  ets:delete(?global_pkgs, Key).

insert_local(Key={_User, _Name, _Vsn}, Package=#pkg{}) ->
  ets:insert(?global_pkgs, {Key, Package}).

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

create_and_load(EpmHome) ->
  PkgDb = open_dets_file(filename:join([EpmHome, ?global_pkgs_db])),
  ets:new(?global_pkgs, [named_table, {keypos, #pkg.id}]),
  lists:foreach(fun(X1) -> ets:insert(?global_pkgs, X1) end, PkgDb),

  RepoDb = open_dets_file(filename:join([EpmHome, ?global_repos_db])),
  ets:new(?global_repos, [named_table, {keypos, #repo.id}]),
  lists:foreach(fun(X3) -> ets:insert(?global_repos, X3) end, RepoDb),

  LocalPkgDb = open_dets_file(?local_pkgs_db),
  ets:new(?local_pkgs, [named_table, {keypos, #pkg.id}]),
  lists:foreach(fun(X2) -> ets:insert(?local_pkgs, X2) end, LocalPkgDb).