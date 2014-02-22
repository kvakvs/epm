%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents package dependency graph and operations
%%% @end
%%% Created : 07. Feb 2014 7:25 PM
%%%-------------------------------------------------------------------
-module(epm_deps).

%% API
-export([resolve_dependencies/1, preferred_package/2]).

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
      SourceFlag = epm:arg_bool(source, Pkgid),
      PkgList = epm_index:list_global_matching(Pkgid),
      lists:foldl(fun(Dep, A1) ->
                    % source flag spreads over all deps
                    Dep1 = epm:set_arg_bool(source, SourceFlag, Dep),
                    ordsets:add_element(Dep1, A1)
                  end, A, lists:flatten([Pkg#pkg.deps || Pkg <- PkgList]))
    end,
  %% For all the package ids get matching packages (TODO: Get 1 best candidate)
  %% and merge it with all the dependency ids

  PkgidsPlusDeps = lists:foldl(F, ordsets:from_list(Pkgids), Pkgids),
  resolve_dependencies_internal(PkgidsPlusDeps, Pkgids).

%% @doc Guesses preferred package from list of pkgids, and loads its definition
-spec preferred_package(PkgList :: [pkg()], Pkgid :: pkgid()) -> pkg().
preferred_package(PkgList, _Pkgid) ->
  %% TODO: Much heuristic. Such insight. Very compatible. Wow.
  hd(PkgList).
  %Id = hd(PkgList),
  %[Pkg] = epm_index:get_pkg(Id), % assume 1 result
  %Pkg.
