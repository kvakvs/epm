%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents package dependency graph and operations
%%% @end
%%% Created : 07. Feb 2014 7:25 PM
%%%-------------------------------------------------------------------
-module(epm_deps).

%% API
-export([package_dependencies/2]).

-include("epm.hrl").

%% -----------------------------------------------------------------------------
%% Compile list of dependencies
%% -----------------------------------------------------------------------------
package_dependencies(GlobalConfig, Packages) ->
  RepoPlugins = proplists:get_value(repo_plugins, GlobalConfig
                                   , ?DEFAULT_API_MODULES),
  G = digraph:new(),
  UpdatedPackages = package_dependencies1(Packages, RepoPlugins, G, undefined
                                         , dict:new()),
  Deps = digraph_utils:topsort(G),
  digraph:delete(G),
  [dict:fetch(Dep, UpdatedPackages) || Dep <- Deps].

%% @private
package_dependencies1([], _, _, _, Dict) -> Dict;
package_dependencies1([Package|Tail], RepoPlugins, G, Parent, Dict) ->
  Repo = epm_ops:retrieve_remote_repo(
    RepoPlugins, Package#package.user, Package#package.name),
  WithoutDeps = lists:member(without_deps, Package#package.args),
  Key = {Repo#repository.owner, Repo#repository.name, Package#package.vsn},

  digraph:add_vertex(G, Key),

  case Parent of
    undefined -> ok;
    {_, ParentProjectName, _} ->
      digraph:add_edge(G, Parent, Key),
      case digraph_utils:is_acyclic(G) of
        true  -> ok;
        false -> ?EXIT("circular dependency detected: ~s <--> ~s"
                      , [ParentProjectName, Repo#repository.name])
      end
  end,

  PkgVsn =
    case Package#package.vsn of
      undefined -> apply(Repo#repository.api_module, default_vsn, []);
      _ -> Package#package.vsn
    end,

  {Deps, Dict1} =
    case WithoutDeps of
      true ->
        {[], Dict};
      false ->
        Deps0 = apply(Repo#repository.api_module, package_deps
                     , [Repo#repository.owner, Repo#repository.name, PkgVsn]),
        F = fun({Dep, Args}, TempDict) ->
            {DepName, DepUser} = epm_ops:split_package(Dep),
            DepVsn = epm_ops:read_vsn_from_args(
              Args, apply(Repo#repository.api_module, default_vsn, [])),
            Package0 = #package{user = DepUser
                               , name = DepName
                               , vsn = DepVsn
                               , args = Args },
            TempDict1 = package_dependencies1([Package0], RepoPlugins, G, Key
                                             , TempDict),
            {{DepUser, DepName, DepVsn}, TempDict1}
          end,
        lists:mapfoldl(F, Dict, Deps0)
    end,

  Package1 = Package#package{ user = Repo#repository.owner
                            , name = Repo#repository.name
                            , vsn = PkgVsn
                            , deps = Deps
                            , repo = Repo },
  package_dependencies1(Tail, RepoPlugins, G, Parent
                       , dict:store(Key, Package1, Dict1)).
