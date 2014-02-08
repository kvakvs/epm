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
  RepoPlugins = epm_cfg:get(repo_plugins, ?DEFAULT_API_MODULES),
  G = digraph:new(),
  UpdatedPackages = package_dependencies_internal(
                              Packages, RepoPlugins, G, undefined, dict:new()),
  Deps = digraph_utils:topsort(G),
  digraph:delete(G),
  [dict:fetch(Dep, UpdatedPackages) || Dep <- Deps].

%% @private
package_dependencies_internal([], _, _, _, Dict) -> Dict;
package_dependencies_internal([Package|Tail], RepoPlugins, G, Parent, Dict) ->
  Repo = epm_ops:retrieve_remote_repo(
    RepoPlugins, Package#epm_package.user, Package#epm_package.name),
  WithoutDeps = lists:member(without_deps, Package#epm_package.args),
  Key = {Repo#epm_repo.owner, Repo#epm_repo.name, Package#epm_package.vsn},

  digraph:add_vertex(G, Key),

  case Parent of
    undefined -> ok;
    {_, ParentProjectName, _} ->
      digraph:add_edge(G, Parent, Key),
      case digraph_utils:is_acyclic(G) of
        true  -> ok;
        false -> ?EXIT("circular dependency detected: ~s <--> ~s"
                      , [ParentProjectName, Repo#epm_repo.name])
      end
  end,

  PkgVsn =
    case Package#epm_package.vsn of
      undefined -> apply(Repo#epm_repo.api_module, default_vsn, []);
      _ -> Package#epm_package.vsn
    end,

  {Deps, Dict1} =
    case WithoutDeps of
      true ->
        {[], Dict};
      false ->
        Deps0 = apply(Repo#epm_repo.api_module, package_deps
                     , [Repo#epm_repo.owner, Repo#epm_repo.name, PkgVsn]),
        F = fun({Dep, Args}, TempDict) ->
            {DepName, DepUser} = epm_ops:split_package(Dep),
            DepVsn = epm_ops:read_vsn_from_args(
              Args, apply(Repo#epm_repo.api_module, default_vsn, [])),
            Package0 = #epm_package          {user = DepUser
                               , name = DepName
                               , vsn = DepVsn
                               , args = Args },
            TempDict1 = package_dependencies_internal([Package0], RepoPlugins, G, Key
                                             , TempDict),
            {{DepUser, DepName, DepVsn}, TempDict1}
          end,
        lists:mapfoldl(F, Dict, Deps0)
    end,

  Package1 = Package#epm_package{ user = Repo#epm_repo.owner
                            , name = Repo#epm_repo.name
                            , vsn = PkgVsn
                            , deps = Deps
                            , repo = Repo },
  package_dependencies_internal(Tail, RepoPlugins, G, Parent
                       , dict:store(Key, Package1, Dict1)).
