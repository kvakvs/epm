%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents package dependency graph and operations
%%% @end
%%% Created : 07. Feb 2014 7:25 PM
%%%-------------------------------------------------------------------
-module(epm_deps).

%% API
-export([package_dependencies/1]).

-include("epm.hrl").

%% -----------------------------------------------------------------------------
%% Compile list of dependencies
%% -----------------------------------------------------------------------------
package_dependencies(Packages) ->
  _RepoPlugins = epm_cfg:get(repo_plugins, ?DEFAULT_API_MODULES),
  G = digraph:new(),
  UpdatedPackages = dict:new(),
%%   UpdatedPackages = package_dependencies_internal(
%%                               Packages, RepoPlugins, G, undefined, dict:new()),
  Deps = digraph_utils:topsort(G),
  digraph:delete(G),
  [dict:fetch(Dep, UpdatedPackages) || Dep <- Deps].

%% @private
%% package_dependencies_internal([], _RepoPlugins, _G, _Parent, Dict) -> Dict;
%% package_dependencies_internal([Package|Tail], RepoPlugins, G
%%                              , ParentPkgid=#pkgid{}, Dict) ->
%%   Repo = epm_ops:retrieve_remote_repo(
%%     RepoPlugins, epm:author(Package), epm:name(Package)),
%%   WithoutDeps = lists:member(without_deps, Package#pkg.args),
%%   Pkgid = #pkgid{ author=Repo#repo.owner
%%               , pkg_name=Repo#repo.name
%%               , vsn=epm:vsn(Package)
%%               },
%%   digraph:add_vertex(G, Pkgid),
%%
%%   case ParentPkgid of
%%     undefined -> ok;
%%     #pkgid{pkg_name=ParentProjectName} ->
%%       digraph:add_edge(G, ParentPkgid, Pkgid),
%%       case digraph_utils:is_acyclic(G) of
%%         true  -> ok;
%%         false -> ?EXIT("circular dependency detected: ~s <--> ~s"
%%                       , [ParentProjectName, epm:name(Repo)])
%%       end
%%   end,
%%
%%   PkgVsn = case epm:vsn(Package) of
%%              undefined -> apply(Repo#repo.api_module, default_vsn, []);
%%              X -> X
%%            end,
%%
%%   {Deps, Dict1} =
%%     case WithoutDeps of
%%       true ->
%%         {[], Dict};
%%       false ->
%%         Deps0 = apply(Repo#repo.api_module, package_deps
%%                      , [epm:name(Repo), PkgVsn]),
%%         F = fun({Dep, Args}, TempDict) ->
%%           {DepName, DepUser} = epm_ops:split_package(Dep),
%%           DepVsn = epm_ops:read_vsn_from_args(
%%             Args, apply(Repo#repo.api_module, default_vsn, [])),
%%           PackageId = #pkgid{author=DepUser, author=DepName, vsn = DepVsn},
%%           Package0 = #pkg{id = PackageId
%%
%%                          , args = Args },
%%           TempDict1 = package_dependencies_internal([Package0], RepoPlugins, G, Pkgid
%%                                            , TempDict),
%%           {{DepUser, DepName, DepVsn}, TempDict1}
%%         end,
%%         lists:mapfoldl(F, Dict, Deps0)
%%     end,
%%
%%   NewKey = #pkgid{author=Repo#repo.owner, pkg_name=Repo#repo.name, vsn=PkgVsn},
%%   Package1 = Package#pkg{ id = NewKey
%%                         , deps = Deps
%%                         , repo = Repo },
%%   package_dependencies_internal(Tail, RepoPlugins, G, ParentPkgid
%%                        , dict:store(Pkgid, Package1, Dict1)).
