%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents operations on package storage and remote storage
%%% @end
%%% Created : 07. Feb 2014 7:16 PM
%%%-------------------------------------------------------------------
-module(epm_ops).

%% API
-export([filter_installed_packages/1
        , retrieve_remote_repo/3
        , retrieve_remote_repos/4
        , installed_packages/0
        , get_installed_packages/1
        , split_package/1
        , read_vsn_from_args/2
        , print_not_installed_package_info/2
        , print_installed_package_info/1
        , update_package/2
        , remove_package/2
        , install_package/2
        , install/3
        ]).

-include("epm.hrl").

%%------------------------------------------------------------------------------
filter_installed_packages(Packages) ->
  filter_installed_packages(Packages, [], []).

%% @private
filter_installed_packages([], Installed, NotInstalled) ->
  {lists:reverse(Installed), NotInstalled};
filter_installed_packages([Package|Tail], Installed, NotInstalled) ->
  case local_package_info(Package) of
    [] -> filter_installed_packages(Tail, Installed, [Package|NotInstalled]);
    [P|_] -> filter_installed_packages(Tail, [P|Installed], NotInstalled)
  end.


split_package(Raw) -> split_package(Raw, []).
split_package([], Package) -> {Package, none};
split_package([47 | Package], User) -> {Package, User};
split_package([A | Tail], User) -> split_package(Tail, User ++ [A]).


installed_packages() ->
  [Package || [{_,Package}] <- epm_index:list_local_packages()].

get_installed_packages(Packages) ->
  Installed = dict:to_list(installed_packages_internal(Packages, dict:new())),
  [V || {_K,V} <- Installed].

%% @private
installed_packages_internal([], Dict) ->
  Dict;
installed_packages_internal([Package|Tail], Dict) ->
  Dict1 =
    case local_package_info(Package) of
      [] ->
        Dict;
      List ->
        FoldF = fun(InstalledPackage, TempDict) ->
          NewValue = { InstalledPackage#package.user
                     , InstalledPackage#package.name
                     , InstalledPackage#package.vsn },
          TempDict1 = dict:store(NewValue, InstalledPackage, TempDict),
          DependantPackages = dependant_installed_packages(InstalledPackage),
          installed_packages_internal(DependantPackages, TempDict1)
        end,
        lists:foldl(FoldF, Dict, List)
    end,
  installed_packages_internal(Tail, Dict1).


dependant_installed_packages(Package) ->
  dependant_installed_packages(Package, [], epm_index:list_local_packages()).

dependant_installed_packages(_Package, Acc, []) -> Acc;
dependant_installed_packages(#package{user = User
                                     , name = Name
                                     , vsn = Vsn} = Package
                    , Acc
                    , [[{_, #package{deps = Deps} = InstalledPackage}]|Tail]) ->
  F = fun({U, N, V}) ->
    (U == User orelse U == none) andalso
      (N == Name) andalso
      (V == Vsn)
  end,
  Acc1 = case lists:filter(F, Deps) of
           [] -> Acc;
           [_] -> [InstalledPackage|Acc]
         end,
  dependant_installed_packages(Package, Acc1, Tail).


retrieve_remote_repo([], _, ProjectName) ->
  ?EXIT("failed to locate remote repo for ~s", [ProjectName]);

retrieve_remote_repo([Module|Tail], none, ProjectName) ->
  case apply(Module, search, [ProjectName]) of
    [] ->
      retrieve_remote_repo(Tail, none, ProjectName);
    Repos when is_list(Repos) ->
      case lists:filter(fun(R1) -> R1#repository.name == ProjectName end
                       , Repos) of
        [R0|_] -> R0;
        [] -> retrieve_remote_repo(Tail, none, ProjectName)
      end;
    Err ->
      ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
  end;

retrieve_remote_repo([Module|Tail], User, ProjectName) ->
  case apply(Module, info, [User, ProjectName]) of
    Repo when is_record(Repo, repository) ->
      Repo;
    undefined ->
      retrieve_remote_repo(Tail, User, ProjectName);
    Err ->
      ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
  end.

retrieve_remote_repos(Modules, User, ProjectName, IsExact) ->
  retrieve_remote_repos(Modules, User, ProjectName, IsExact, []).

retrieve_remote_repos([], _, _, _, Acc) -> Acc;

retrieve_remote_repos([Module|Tail], none, ProjectName, IsExact, Acc) ->
  case apply(Module, search, [ProjectName]) of
    [] ->
      retrieve_remote_repos(Tail, none, ProjectName, IsExact, Acc);
    Repos when is_list(Repos), IsExact == true ->
      case lists:filter(fun(R1) -> R1#repository.name == ProjectName end
                       , Repos) of
        [] -> retrieve_remote_repos(Tail, none, ProjectName, IsExact, Acc);
        R0s ->
          retrieve_remote_repos(Tail, none, ProjectName, IsExact
                               , lists:append(Acc, R0s))
      end;
    Repos when is_list(Repos) ->
      retrieve_remote_repos(Tail, none, ProjectName, IsExact
                           , lists:append(Acc, Repos));
    Err ->
      ?EXIT("failed to locate remote repos for ~s: ~p", [ProjectName, Err])
  end;

retrieve_remote_repos([Module|Tail], User, ProjectName, IsExact, Acc) ->
  case apply(Module, info, [User, ProjectName]) of
    Repo when is_record(Repo, repository) ->
      retrieve_remote_repos(Tail, User, ProjectName, IsExact, Acc ++ [Repo]);
    undefined ->
      retrieve_remote_repos(Tail, User, ProjectName, IsExact, Acc);
    Err ->
      ?EXIT("failed to locate remote repo for ~s: ~p", [ProjectName, Err])
  end.

%% -----------------------------------------------------------------------------
%% package info
%% -----------------------------------------------------------------------------
%% @private
local_package_info(#package{ user = none
                           , name = ProjectName
                           , vsn = undefined}) ->
  case epm_index:list_local_by('_', ProjectName, '_') of
    [] -> [];
    List -> [Package || [Package] <- List]
  end;
local_package_info(#package{ user = none
                           , name = ProjectName
                           , vsn = Vsn}) ->
  case epm_index:list_local_by('_', ProjectName, Vsn) of
    [] -> [];
    List -> [Package || [Package] <- List]
  end;
local_package_info(#package{ user = User
                           , name = ProjectName
                           , vsn = undefined}) ->
  case epm_index:list_local_by(User, ProjectName, '_') of
    [] -> [];
    List -> [Package || [Package] <- List]
  end;
local_package_info(#package{ user = User
                           , name = ProjectName
                           , vsn = Vsn}) ->
  case epm_index:list_local_by(User, ProjectName, Vsn) of
    [] -> [];
    List -> [Package || [Package] <- List]
  end.


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
print_installed_package_info(Package) ->
  Repo = Package#package.repo,
  [io:format("  ~s: ~s~n", [Field, if Value==undefined -> ""; true -> Value end]) || {Field, Value} <- [
    {"name", Repo#repository.name},
    {"owner", Repo#repository.owner},
    {"vsn", Package#package.vsn},
    {"pushed", Repo#repository.pushed},
    {"install dir", Package#package.install_dir},
    {"homepage", Repo#repository.homepage},
    {"description", Repo#repository.description}
                                                                                                       ]],
  case Package#package.deps of
    [] -> ok;
    Deps ->
      io:format("  dependencies: ~n    ~s~n", [string:join([
        case U of
          none -> lists:flatten(io_lib:format("~s/~s", [N,V]));
          _ -> lists:flatten(io_lib:format("~s/~s/~s", [U,N,V]))
        end || {U,N,V} <- Deps], "\n    ")])
  end.

print_not_installed_package_info(GlobalConfig, Packages) ->
  print_not_installed_package_info(GlobalConfig, Packages, false).

print_not_installed_package_info(GlobalConfig, Packages, IsExact) ->
  RepoPlugins = proplists:get_value(repo_plugins, GlobalConfig, ?DEFAULT_API_MODULES),
  write_not_installed_package_info1(Packages, RepoPlugins, IsExact).

write_not_installed_package_info1(Packages, RepoPlugins, IsExact) ->
  case fetch_not_installed_package_info(Packages, RepoPlugins, [], IsExact) of
    [] ->
      io:format("- not found~n");
    Repos ->
      io:format("===============================~n"),
      io:format("AVAILABLE~n"),
      io:format("===============================~n"),
      lists:foldl(
        fun(Repo, Count) ->
          Tags = apply(Repo#repository.api_module, tags, [Repo#repository.owner, Repo#repository.name]),
          Branches = apply(Repo#repository.api_module, branches, [Repo#repository.owner, Repo#repository.name]),
          case Count of
            0 -> ok;
            _ -> io:format("~n")
          end,
          [io:format("  ~s: ~s~n", [Field, if Value == undefined -> ""; true ->
            Value end              ]) || {Field, Value} <- [
            {"name", Repo#repository.name}                 ,
            {"owner", Repo#repository.owner}               ,
            {"followers", Repo#repository.followers}       ,
            {"pushed", Repo#repository.pushed}             ,
            {"homepage", Repo#repository.homepage}         ,
            {"description", Repo#repository.description}   ,
            {"repo plugin", atom_to_list(Repo#repository.api_module)}
                                                           ]],
          if
            Tags =/= [] ->
              io:format("  tags:~n"),
              [io:format("    ~s~n", [Tag]) || Tag <- Tags];
            true -> ok
          end,
          if
            Branches =/= [] ->
              io:format("  branches:~n"),
              [io:format("    ~s~n", [Branch]) || Branch <- Branches];
            true -> ok
          end,
          Count + 1
        end      , 0, Repos)
  end.

fetch_not_installed_package_info([], _, Acc, _) -> Acc;
fetch_not_installed_package_info([#package{user=User,name=ProjectName}|Tail]
                                , RepoPlugins, Acc, IsExact) ->
  Repos = epm_ops:retrieve_remote_repos(
    RepoPlugins, User, ProjectName, IsExact),
  fetch_not_installed_package_info(
    Tail, RepoPlugins, lists:append(Acc, Repos), IsExact).


%% -----------------------------------------------------------------------------
%% REMOVE
%% -----------------------------------------------------------------------------
remove_package(_GlobalConfig, #package{ user = User
                                      , name = Name
                                      , vsn = Vsn
                                      , install_dir = InstallDir}) ->
  io:format("+ removing package ~s-~s-~s from ~s~n"
           , [User, Name, Vsn, InstallDir]),
  RemoveCmd = "rm -rf " ++ InstallDir,
  epm_util:print_cmd_output("~s~n", [RemoveCmd]),
  epm_util:do_cmd(RemoveCmd, fail),
  epm_index:delete_local({User, Name, Vsn}).

%% -----------------------------------------------------------------------------
%% UPDATE
%% -----------------------------------------------------------------------------
update_package(GlobalConfig, Package) ->
  Repo = Package#package.repo,
  Vsn = Package#package.vsn,
  %% switch to build home dir
  epm_util:set_cwd_build_home(GlobalConfig),

  %% download correct version of package
  LocalProjectDir = apply(Repo#repository.api_module, download_package
                         , [Repo, Vsn]),

  %% switch to project dir
  epm_util:set_cwd_build_home(GlobalConfig),
  epm_util:set_cwd(LocalProjectDir),

  %% build/install project
  %_InstallDir = build_project(GlobalConfig, Package),

  %% switch to build home dir and delete cloned project
  epm_util:set_cwd_build_home(GlobalConfig),
  epm_util:del_dir(LocalProjectDir).


%% -----------------------------------------------------------------------------
%% INSTALL
%% -----------------------------------------------------------------------------
install_package(GlobalConfig, Package) ->
  Repo = Package#package.repo,
  User = Repo#repository.owner,
  Name = Repo#repository.name,
  Vsn = Package#package.vsn,
  %% switch to build home dir
  epm_util:set_cwd_build_home(GlobalConfig),

  %% download correct version of package
  LocalProjectDir = apply(Repo#repository.api_module, download_package, [Repo, Vsn]),

  %% switch to project dir
  epm_util:set_cwd_build_home(GlobalConfig),
  epm_util:set_cwd(LocalProjectDir),

  %% build/install project
  InstallDir = "not-building-anything", %build_project(GlobalConfig, Package),

  %% switch to build home dir and delete cloned project
  epm_util:set_cwd_build_home(GlobalConfig),
  epm_util:del_dir(LocalProjectDir),

  Package1 = Package#package{install_dir = InstallDir},
  epm_index:insert_local({User, Name, Vsn}, Package1),
  ok.

install(ProjectName, Config, undefined) ->
  install(ProjectName, Config, code:lib_dir());

install(ProjectName, _Config, LibDir) ->
  Vsn =
    case file:consult("ebin/" ++ ProjectName ++ ".app") of
      {ok, [{application, _, Props}]} ->
        proplists:get_value(vsn, Props);
      _ ->
        undefined
    end,
  Dir =
    case Vsn of
      undefined -> LibDir ++ "/" ++ ProjectName;
      _ -> LibDir ++ "/" ++ ProjectName ++ "-" ++ Vsn
    end,
  InstallCmd = "mkdir -p " ++ Dir ++ "; cp -R ./* " ++ Dir,
  io:format("+ running ~s install command~n", [ProjectName]),
  epm_util:print_cmd_output("~s~n", [InstallCmd]),
  epm_util:do_cmd(InstallCmd, fail),
  Ebin = Dir ++ "/ebin",
  case code:add_pathz(Ebin) of
    true -> ok;
    Err  ->
      M = io_lib:format("failed to add path for ~s (~s): ~p"
                       , [ProjectName, Ebin, Err]),
      exit(lists:flatten(M))
  end,
  Dir.