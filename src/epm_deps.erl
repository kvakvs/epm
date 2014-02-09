%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents package dependency graph and operations
%%% @end
%%% Created : 07. Feb 2014 7:25 PM
%%%-------------------------------------------------------------------
-module(epm_deps).

%% API
-export([resolve_dependencies/1]).

-include("epm.hrl").

%% -----------------------------------------------------------------------------
%% Compile list of dependencies
%% -----------------------------------------------------------------------------
-spec resolve_dependencies(Pkgids :: [pkgid()]) -> [pkgid()].
resolve_dependencies(Pkgids) ->
  %% TODO: Return tree of subdependencies for visual printout
  resolve_dependencies_internal(ordsets:from_list(Pkgids), ordsets:new()).

-spec resolve_dependencies_internal(ordsets:ordset(pkgid())
                                   , ordsets:ordset(pkgid()))
      -> ordsets:ordset(pkgid()).
resolve_dependencies_internal(Pkgids, Pkgids) -> Pkgids;
resolve_dependencies_internal(Pkgids, _Previous) ->
  F = fun(Pkgid=#pkgid{}, A) ->
      PkgList = epm_index:list_global_matching(Pkgid),
      lists:foldl(fun(Dep, A1) -> ordsets:add_element(Dep, A1) end
                 , A, lists:flatten([Pkg#pkg.deps || Pkg <- PkgList]))
    end,
  PkgidsPlusDeps = lists:foldl(F, ordsets:from_list(Pkgids), Pkgids),
  resolve_dependencies_internal(PkgidsPlusDeps, Pkgids).
