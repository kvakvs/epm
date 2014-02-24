%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents operations on package storage and remote storage
%%% @end
%%% Created : 07. Feb 2014 7:16 PM
%%%-------------------------------------------------------------------
-module(epm_ops).

%% API
-export([ filter_installed_packages/1
        , read_vsn_from_args/2
        , print_installed_package_info/1
        , print_not_installed_package_info/1
        %, print_not_installed_package_info/2
        , update_package/1
        , remove_package/1
        , install_package/1
        , wrap_package/1
        , update_epm/0
        ]).
-export([ installed_packages/1 % move this to index
        , get_installed_packages/1 % move this to index
        , split_package/1 % move this to util
        ]).

-include("epm.hrl").

%%------------------------------------------------------------------------------
%% @doc Returns pair of installed and not installed packages
-spec filter_installed_packages([pkg:pkg()]
                               ) -> {[pkg:pkg()], [pkg:pkg()]}.
filter_installed_packages(Pkgids) ->
  {Installed, Not} = lists:partition(
                            fun(Id) when ?IS_PKGID(Id) ->
                              epm_index:is_installed(Id)
                            end, Pkgids),
  Installed1 = lists:map(fun(X) -> pkgid:set_arg_bool(source, false, X) end
                        , Installed),
  {Installed1, Not}.
%% filter_installed_packages(Packages) ->
%%   filter_installed_packages(Packages, [], []).
%%
%% %% @private
%% filter_installed_packages([], Installed, NotInstalled) ->
%%   {lists:reverse(Installed), NotInstalled};
%% filter_installed_packages([Package|Tail], Installed, NotInstalled) ->
%%   case local_package_info(Package) of
%%     [] -> filter_installed_packages(Tail, Installed, [Package|NotInstalled]);
%%     [P|_] -> filter_installed_packages(Tail, [P|Installed], NotInstalled)
%%   end.


%% TODO: move this to util or elsewhere
split_package(Raw) -> split_package(Raw, []).
split_package([], Package) -> {Package, ?any_author};
split_package([47 | Package], Author) -> {Package, Author};
split_package([A | Tail], Author) -> split_package(Tail, Author ++ [A]).

%% TODO: move this to index or elsewhere
installed_packages(_State=#epm_state{}) ->
  [Package || [{_,Package}] <- epm_index:list_local_packages()].

%% TODO: move this to index or elsewhere
get_installed_packages(_Packages) ->
  [].
%%   Installed = dict:to_list(installed_packages_internal(Packages, dict:new())),
%%   [V || {_K,V} <- Installed].

%% @private
%% installed_packages_internal([], Dict) ->
%%   Dict;
%% installed_packages_internal([Package|Tail], Dict) ->
%%   Dict1 =
%%     case local_package_info(Package) of
%%       [] ->
%%         Dict;
%%       List ->
%%         FoldF = fun(InstalledPackage, TempDict) ->
%%           NewValue = { epm:author(InstalledPackage)
%%                      , epm:name(InstalledPackage)
%%                      , epm:vsn(InstalledPackage) },
%%           TempDict1 = dict:store(NewValue, InstalledPackage, TempDict),
%%           DependantPackages = dependant_installed_packages(InstalledPackage),
%%           installed_packages_internal(DependantPackages, TempDict1)
%%         end,
%%         lists:foldl(FoldF, Dict, List)
%%     end,
%%   installed_packages_internal(Tail, Dict1).


%% dependant_installed_packages(Package) ->
%%   dependant_installed_packages(Package, [], epm_index:list_local_packages()).
%%
%% dependant_installed_packages(_Package, Acc, []) -> Acc;
%% dependant_installed_packages(#pkg{}=Package
%%                     , Acc
%%                     , [[{_, #pkg{deps = Deps} = InstalledPackage}]|Tail]) ->
%%   F = fun(Pkgid) when ?IS_PKGID(Pkgid) ->
%%       epm:matches(Pkgid, Package#pkg.id)
%%     end,
%%   Acc1 = case lists:filter(F, Deps) of
%%            [] -> Acc;
%%            [_] -> [InstalledPackage|Acc]
%%          end,
%%   dependant_installed_packages(Package, Acc1, Tail).


%% retrieve_remote_repo([], _, ProjectName) ->
%%   ?EXIT("failed to locate remote repo for ~s", [ProjectName]);
%%
%% retrieve_remote_repo([Module|Tail], none, ProjectName) ->
%%   case apply(Module, search, [ProjectName]) of
%%     [] ->
%%       retrieve_remote_repo(Tail, none, ProjectName);
%%     Repos when is_list(Repos) ->
%%       case lists:filter(fun(R1) -> R1#repo.name == ProjectName end
%%                        , Repos) of
%%         [R0|_] -> R0;
%%         [] -> retrieve_remote_repo(Tail, none, ProjectName)
%%       end;
%%     Err ->
%%       ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
%%   end;
%%
%% retrieve_remote_repo([Module|Tail], User, ProjectName) ->
%%   case apply(Module, info, [User, ProjectName]) of
%%     Repo when is_record(Repo, repo) ->
%%       Repo;
%%     undefined ->
%%       retrieve_remote_repo(Tail, User, ProjectName);
%%     Err ->
%%       ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
%%   end.
%%
%% retrieve_remote_repos(Modules, User, ProjectName, IsExact) ->
%%   retrieve_remote_repos(Modules, User, ProjectName, IsExact, []).
%%
%% retrieve_remote_repos([], _, _, _, Acc) -> Acc;
%%
%% retrieve_remote_repos([Module|Tail], none, ProjectName, IsExact, Acc) ->
%%   case apply(Module, search, [ProjectName]) of
%%     [] ->
%%       retrieve_remote_repos(Tail, none, ProjectName, IsExact, Acc);
%%     Repos when is_list(Repos), IsExact == true ->
%%       case lists:filter(fun(R1) -> R1#repo.name == ProjectName end
%%                        , Repos) of
%%         [] -> retrieve_remote_repos(Tail, none, ProjectName, IsExact, Acc);
%%         R0s ->
%%           retrieve_remote_repos(Tail, none, ProjectName, IsExact
%%                                , lists:append(Acc, R0s))
%%       end;
%%     Repos when is_list(Repos) ->
%%       retrieve_remote_repos(Tail, none, ProjectName, IsExact
%%                            , lists:append(Acc, Repos));
%%     Err ->
%%       ?EXIT("failed to locate remote repos for ~s: ~p", [ProjectName, Err])
%%   end;
%%
%% retrieve_remote_repos([Module|Tail], User, ProjectName, IsExact, Acc) ->
%%   case apply(Module, info, [User, ProjectName]) of
%%     Repo when is_record(Repo, repo) ->
%%       retrieve_remote_repos(Tail, User, ProjectName, IsExact, Acc ++ [Repo]);
%%     undefined ->
%%       retrieve_remote_repos(Tail, User, ProjectName, IsExact, Acc);
%%     Err ->
%%       ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
%%   end.

%% -----------------------------------------------------------------------------
%% package info
%% -----------------------------------------------------------------------------
%% @private
%% TODO: platform and erlang vsn support
%% -spec local_package_info(#pkg{}) -> list().
%% local_package_info(#pkg{id=#pkgid{ author = ?any_author
%%                                  , pkg_name = ProjectName
%%                                  , vsn = ?any_vsn}}) ->
%%   case epm_index:list_local_matching('_', ProjectName, '_') of
%%     [] -> [];
%%     List -> [Package || [Package] <- List]
%%   end;
%% local_package_info(#pkg{id=#pkgid{ author = ?any_author
%%                                  , pkg_name = ProjectName
%%                                  , vsn = Vsn}}) ->
%%   case epm_index:list_local_matching('_', ProjectName, Vsn) of
%%     [] -> [];
%%     List -> [Package || [Package] <- List]
%%   end;
%% local_package_info(#pkg{id=#pkgid{ author = User
%%                                  , pkg_name = ProjectName
%%                                  , vsn = ?any_vsn }}) ->
%%   case epm_index:list_local_matching(User, ProjectName, '_') of
%%     [] -> [];
%%     List -> [Package || [Package] <- List]
%%   end;
%% local_package_info(#pkg{id=#pkgid{ author = User
%%                                  , pkg_name = ProjectName
%%                                  , vsn = Vsn }}) ->
%%   case epm_index:list_local_matching(User, ProjectName, Vsn) of
%%     [] -> [];
%%     List -> [Package || [Package] <- List]
%%   end.


%% -----------------------------------------------------------------------------
%% Read vsn
%% -----------------------------------------------------------------------------
read_vsn_from_args([{tag, Tag}|_], _) -> Tag;
read_vsn_from_args([{branch, Branch}|_], _) -> Branch;
read_vsn_from_args([{sha, Sha}|_], _) -> Sha;
read_vsn_from_args([_|Tail], Default) -> read_vsn_from_args(Tail, Default);
read_vsn_from_args([], Default) -> Default.

%% -----------------------------------------------------------------------------
%% Print package info
%% -----------------------------------------------------------------------------
-spec print_installed_package_info(pkg:pkg()) -> any().
print_installed_package_info(Package) when ?IS_PKG(Package) ->
  Repo = pkg:repo(Package),
  io:format("~s~n", [epm:as_string(Repo)]),
  case pkg:deps(Package) of
    [] -> ok;
    Deps ->
      io:format("  dependencies: ~n  ~s~n",
        [string:join(lists:map(fun epm:as_string/1, Deps), "\n  ")])
  end.

print_not_installed_package_info(Packages) ->
  print_not_installed_package_info(Packages, false).

print_not_installed_package_info(Packages, IsExact) ->
  RepoPlugins = epm_cfg:get(repo_plugins, ?DEFAULT_API_MODULES),
  print_not_installed_internal(Packages, RepoPlugins, IsExact).

print_not_installed_internal(Packages, RepoPlugins, IsExact) ->
  case fetch_not_installed_package_info(Packages, RepoPlugins, [], IsExact) of
    [] ->
      io:format("- not found~n");
    Repos ->
      epm:p("===============================~n"
            "AVAILABLE~n"
            "===============================~n"),
      F = fun(Repo, Count) ->
          Tags = [], %apply(Repo#repo.api_module, tags, [Repo#repo.owner, Repo#repo.name]),
          Branches = [], %apply(Repo#repo.api_module, branches, [Repo#repo.owner, Repo#repo.name]),
          case Count of 0 -> ok; _ -> io:format("~n") end,
          epm:p("~s~n", [epm:as_string(Repo)]),
          if Tags =/= [] ->
            epm:p("  tags:~n"),
            [epm:p("    ~s~n", [Tag]) || Tag <- Tags];
            true -> ok
          end,
          if Branches =/= [] ->
              epm:p("  branches:~n"),
              [epm:p("    ~s~n", [Branch]) || Branch <- Branches];
            true -> ok
          end,
          Count + 1
        end,
      lists:foldl(F, 0, Repos)
  end.

fetch_not_installed_package_info([], _, Acc, _) -> Acc;
fetch_not_installed_package_info([Pkg | Tail]
                                , RepoPlugins, Acc, IsExact
                                ) when ?IS_PKG(Pkg) ->
  Pkgid = pkg:id(Pkg),
  _User = pkgid:author(Pkgid),
  _ProjectName = pkgid:pkg_name(Pkgid),
  Repos = [],
  %epm_ops:retrieve_remote_repos(RepoPlugins, User, ProjectName, IsExact),
  fetch_not_installed_package_info(
    Tail, RepoPlugins, lists:append(Acc, Repos), IsExact).


%% -----------------------------------------------------------------------------
%% REMOVE
%% -----------------------------------------------------------------------------
-spec remove_package(pkgid:pkgid()) -> ok.
remove_package(Pkgid) when ?IS_PKGID(Pkgid) ->
  epm:p("+ removing package ~s~n" , [epm:as_string(Pkgid)]),
  %RemoveCmd = "rm -rf " ++ InstallDir,
  %epm_util:print_cmd_output("~s~n", [RemoveCmd]),
  %epm_util:do_cmd(RemoveCmd, fail),
  %epm_index:delete_local({User, Name, Vsn}),
  ok.

%% -----------------------------------------------------------------------------
%% UPDATE
%% -----------------------------------------------------------------------------
-spec update_package(pkgid:pkgid()) -> ok.
update_package(Pkgid) when ?IS_PKGID(Pkgid) ->
  epm:p("+ updating package ~s~n" , [pkgid:as_string(Pkgid)]),
  ok.

%% -----------------------------------------------------------------------------
%% INSTALL
%% -----------------------------------------------------------------------------
-spec install_package(pkgid:pkgid()) -> ok.
install_package(Pkgid) when ?IS_PKGID(Pkgid) ->
  epm:p("+ searching to install ~s~n" , [epm:as_string(Pkgid)]),
  PkgList = epm_index:list_global_matching(Pkgid),
  epm:p("  available: ~s~n",
    [string:join(lists:map(fun epm:as_string/1, PkgList), "; ")]),

  %% Fetch the package
  Preferred = epm_deps:preferred_package(PkgList, Pkgid),
  epm_vcs:get_source(Preferred, "deps"),
  ok.

%% -----------------------------------------------------------------------------
%% WRAP UP THE PACKAGE
%% -----------------------------------------------------------------------------
-spec wrap_package(Path :: string()) -> ok.
wrap_package(_Path) ->
  ok.

%% -----------------------------------------------------------------------------
%% Replace epm script with most recent
%% -----------------------------------------------------------------------------
update_epm() ->
  File =
    case os:find_executable("epm") of
      false ->
        case filelib:is_regular("epm") of
          true  -> "./epm";
          false -> exit("failed to find epm executable to replace")
        end;
      F -> F
    end,
  Fork = proplists:get_value(epm_fork, application:get_all_env(epm), "JacobVorreuter"),
  Url = "http://github.com/" ++ Fork ++ "/epm/raw/master/epm",
  case epm_util:http_request(Url, [{"Host", "github.com"}], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Body}} ->
      case file:write_file(File, Body) of
        ok ->
          epm:p("+ updated epm (~s) to latest version~n", [File]);
        {error, Reason} ->
          exit(lists:flatten(io_lib:format("failed to overwrite epm executable ~s: ~p~n", [File, Reason])))
      end;
    _ ->
      exit("failed to download latest version of epm")
  end.