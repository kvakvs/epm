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
        , list_local_packages/0
        , list_local_by/3
        , delete_local/1
        , insert_local/2
        , close/0, find_package/1]).

-include("epm.hrl").
%-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% -define(local_index, epm_local_index).
%% -define(local_index_file, "epm_local_index").

-define(global_index, epm_index).
-define(global_index_file, "epm_packages").

%% -define(global_repo_index, epm_repo_index).
%% -define(global_repo_index_file, "epm_repos").

%%------------------------------------------------------------------------------
find_package(#pkgid{ author=A1, pkg_name=N1, platform=P1
                   , vsn=V1, erlang_vsn=E1}) ->
  Q = ets:fun2ms(fun(#pkg{id=#pkgid{ author=A2, pkg_name=N2, platform=P2
                                   , vsn=V2, erlang_vsn=E2}}=Pkg)
                      when (N1 =:= N2
                        andalso (A1 =:= A2 orelse A1 =:= ?any_author)) -> Pkg
                    end),
  ets:select(?global_index, Q).

close() -> ok.

open(_Home, EpmHome) ->
  G1 = open_dets_file(filename:join([EpmHome, ?global_index_file])),
  ets:new(?global_index, [named_table, {keypos, #pkg.id}]),
  lists:foreach(fun(X1) -> ets:insert(?global_index, X1) end, G1),

  %% Fixtures
  CowboyId = #pkgid{pkg_name="cowboy"},
  RanchId = #pkgid{pkg_name="ranch"},
  GunId = #pkgid{pkg_name="gun"},
  FwId = #pkgid{pkg_name="farwest"},
  OtherId = #pkgid{pkg_name="other"},
  ets:insert(?global_index, #pkg{id=CowboyId, deps=[RanchId]}),
  ets:insert(?global_index, #pkg{id=RanchId, deps=[GunId]}),
  ets:insert(?global_index, #pkg{id=GunId}),
  ets:insert(?global_index, #pkg{id=FwId}),
  ets:insert(?global_index, #pkg{id=OtherId}),

  State = #epm_state{},
  State.

list_local_packages() ->
  ets:match(?global_index, '$1').

%% @doc Provide '_' if field is not relevant
list_local_by(User, ProjectName, Version) ->
  ets:match(?global_index, {{User, ProjectName, Version}, '$1'}).

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