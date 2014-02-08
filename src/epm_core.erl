-module(epm_core).
-export([execute/2]).

-include("epm.hrl").

execute(GlobalConfig0, ["install" | Args]) ->
  {GlobalConfig, Packages, Flags} = collect_args(install, Args, GlobalConfig0),
  %% TODO: Remove put/get
  put(verbose, lists:member(verbose, Flags)),
  Deps = epm_deps:package_dependencies(GlobalConfig, Packages),
  {Installed, NotInstalled} = epm_cache:filter_installed_packages(Deps),
  case NotInstalled of
    [] ->
      io:format("+ nothing to do: packages and dependencies already installed~n");
    _ ->
      case Installed of
        [] -> ok;
        _ ->
          io:format("===============================~n"),
          io:format("Packages already installed:~n"),
          io:format("===============================~n"),
          [begin
             io:format("    + ~s-~s-~s (~s)~n", [U, N, V, AppVsn])
           end || #package{user = U, name = N, vsn = V, app_vsn = AppVsn} <- Installed]
      end,
      io:format("===============================~n"),
      io:format("Install the following packages?~n"),
      io:format("===============================~n"),
      [io:format("    + ~s-~s-~s~n", [U, N, V]) || #package{user = U, name = N, vsn = V} <- NotInstalled],
      io:format("~n([y]/n) "),
      case io:get_chars("", 1) of
        C when C == "y"; C == "\n" ->
          io:format("~n"),
          [epm_cache:install_package(GlobalConfig, Package)
            || Package <- NotInstalled];
        _ -> ok
      end
  end;

execute(GlobalConfig0, ["remove" | Args]) ->
  {GlobalConfig, Packages, Flags} = collect_args(remove, Args, GlobalConfig0),
  put(verbose, lists:member(verbose, Flags)),
  Installed = epm_cache:installed_packages(Packages),
  case Installed of
    [] ->
      io:format("+ nothing to remove: no matching packages installed~n");
    _ ->
      io:format("===============================~n"),
      io:format("Remove the following packages?~n"),
      io:format("===============================~n"),
      [io:format("    + ~s-~s-~s~n", [U, N, V])
        || #package{user = U, name = N, vsn = V} <- Installed],
      io:format("~n([y]/n) "),
      case io:get_chars("", 1) of
        C when C == "y"; C == "\n" ->
          io:format("~n"),
          [epm_cache:remove_package(GlobalConfig, Package)
            || Package <- Installed];
        _ -> ok
      end
  end;

execute(GlobalConfig0, ["update" | Args]) ->
  {GlobalConfig, Packages, Flags} = collect_args(update, Args, GlobalConfig0),
  put(verbose, lists:member(verbose, Flags)),
  Installed = epm_cache:installed_packages(Packages),
  case Installed of
    [] ->
      io:format("- nothing to update~n");
    _ ->
      io:format("===============================~n"),
      io:format("Update the following packages?~n"),
      io:format("===============================~n"),
      [io:format("    + ~s-~s-~s~n", [U, N, V])
        || #package{user = U, name = N, vsn = V} <- Installed],
      io:format("~n([y]/n) "),
      case io:get_chars("", 1) of
        C when C == "y"; C == "\n" ->
          io:format("~n"),
          [epm_cache:update_package(GlobalConfig, Package)
            || Package <- Installed];
        _ -> ok
      end
  end;

execute(GlobalConfig0, ["info" | Args]) ->
  {GlobalConfig, Packages, _Flags} = collect_args(info, Args, GlobalConfig0),
  {Installed, NotInstalled} = epm_cache:filter_installed_packages(Packages),
  case Installed of
    [] -> ok;
    _ ->
      io:format("===============================~n"),
      io:format("INSTALLED~n"),
      io:format("===============================~n"),

      lists:foldl(
        fun(Package, Count) ->
          case Count of
            0 -> ok;
            _ -> io:format("~n")
          end,
          epm_cache:print_installed_package_info(Package),
          Count + 1
        end      , 0, lists:reverse(Installed))
  end,

  case NotInstalled of
    [] -> ok;
    _ ->
      case Installed of
        [] -> ok;
        _ -> io:format("~n")
      end,
      epm_cache:print_not_installed_package_info(GlobalConfig
                                                , NotInstalled, true)
  end;

execute(GlobalConfig0, ["search" | Args]) ->
  {GlobalConfig, Packages, _Flags} = collect_args(search, Args, GlobalConfig0),
  epm_cache:print_not_installed_package_info(GlobalConfig
                                            , lists:reverse(Packages));

execute(_GlobalConfig, ["list" | _Args]) ->
  Installed = epm_cache:installed_packages(),
  case Installed of
    [] ->
      io:format("- no packages installed~n");
    _ ->
      io:format("===============================~n"),
      io:format("INSTALLED~n"),
      io:format("===============================~n"),

      lists:foldl(
        fun(Package, Count) ->
          case Count of
            0 -> ok;
            _ -> io:format("~n")
          end,
          epm_cache:print_installed_package_info(Package),
          Count + 1
        end      , 0, lists:reverse(Installed))
  end;

execute(_GlobalConfig, ["latest" | _Args]) ->
	update_epm();

execute(GlobalConfig0, ["config" | Args]) ->
  {GlobalConfig, _Packages, Flags} = collect_args(config, Args, GlobalConfig0),
  case Flags of
    [] ->
      print_config_values(GlobalConfig);
    [get] ->
      print_config_values(GlobalConfig);
    _ ->
      Config2 = lists:foldl(
        fun(Flag, Config) ->
          case Flag of
            {set, [K, V]} ->
              K1 = list_to_atom(K),
              [{K1, V}|proplists:delete(K1, Config)];
            {remove, K} ->
              K1 = list_to_atom(K),
              proplists:delete(K1, Config);
            _ ->
              Config
          end
        end                , GlobalConfig, Flags),
      write_config_file(Config2)
  end;

execute(_, _) ->
  io:format("Usage: epm commands~n~n"),
  io:format("    install [<user>/]<project> {project options}, ... {global options}~n"),
  io:format("        project options:~n"),
  io:format("             --tag <tag>~n"),
  io:format("             --branch <branch>~n"),
  io:format("             --sha <sha>~n"),
  io:format("             --with-deps (default)~n"),
  io:format("             --without-deps~n"),
  io:format("             --prebuild-command <cmd>~n"),
  io:format("             --build-command <cmd>~n"),
  io:format("             --test-command <cmd>~n"),
  io:format("        global options:~n"),
  io:format("             --verbose~n"),
  io:format("             --config-set <key> <value>~n~n"),
  io:format("    remove [<user>/]<project> {project options}, ... {global options}~n"),
  io:format("        project options:~n"),
  io:format("             --tag <tag>~n"),
  io:format("             --branch <branch>~n"),
  io:format("             --sha <sha>~n"),
  io:format("        global options:~n"),
  io:format("             --verbose~n~n"),
  io:format("             --config-set <key> <value>~n~n"),
  io:format("    update [<user>/]<project> {project options}, ... {global options}~n"),
  io:format("        project options:~n"),
  io:format("             --tag <tag>~n"),
  io:format("             --branch <branch>~n"),
  io:format("             --sha <sha>~n"),
  io:format("             --with-deps~n"),
  io:format("             --without-deps (default)~n"),
  io:format("        global options:~n"),
  io:format("             --verbose~n~n"),
  io:format("             --config-set <key> <value>~n~n"),
  io:format("    info [<user>/]<project>, ... {global options}~n"),
  io:format("        global options:~n"),
  io:format("             --config-set <key> <value>~n~n"),
  io:format("    search <project>, ... {global options}~n"),
  io:format("        global options:~n"),
  io:format("             --config-set <key> <value>~n~n"),
  io:format("    list~n~n"),
  io:format("    latest~n~n"),
  io:format("    config {options}~n"),
  io:format("        options:~n"),
  io:format("             --get (default)~n"),
  io:format("             --set <key> <value>~n"),
  io:format("             --remove <key>~n"),
  ok.

%% -----------------------------------------------------------------------------
%% parse input args
%% -----------------------------------------------------------------------------

%% collect_args(Target, Args, GlobalConfig) -> Results
%%   Target = atom()
%%	 Args = [string()]
%%`  GlobalConfig = list()
%%   Results = {[package(), Flags]}
%%	 Flags = [atom()]
collect_args(Target, Args, GlobalConfig) ->
  collect_args(Target, Args, GlobalConfig, [], []).
collect_args(_, [], GlobalConfig, Packages, Flags) ->
  {GlobalConfig, lists:reverse(Packages), lists:reverse(Flags)};
collect_args(Target, [Arg | Rest], GlobalConfig, Packages, Flags) ->
  case parse_tag(Target, Arg) of
    undefined -> %% if not a tag then must be a project name
      %% split into user and project
      {ProjectName, User} = epm_cache:split_package(Arg),
      collect_args(Target, Rest, GlobalConfig
                  , [#package{user = User, name = ProjectName}|Packages]
                  , Flags);
    {Type, Tag, 0} ->   %% tag with no trailing value
      case Type of
        project ->
          [#package{args = Args} = Package|OtherPackages] = Packages,
          collect_args(Target, Rest, GlobalConfig
                      , [Package#package{args = Args ++ [Tag]}|OtherPackages]
                      , Flags);
        global ->
          collect_args(Target, Rest, GlobalConfig, Packages, [Tag|Flags])
      end;
    {Type, Tag, NumVals} when is_integer(NumVals) -> % tag with trailing value(s)
      if
        length(Rest) < NumVals ->
          exit("poorly formatted command");
        true -> ok
      end,
      {Vals, Rest1} = lists:split(NumVals, Rest),
      Vals1 =
        case Vals of
          [V] -> V;
          _ -> Vals
        end,
      case Type of
        project ->
          %% this tag applies to the last project on the stack
          [#package{args = Args} = Package|OtherPackages] = Packages,
          Vsn = if
                  Tag == tag; Tag == branch; Tag == sha -> Vals1;
                  true -> Package#package.vsn
                end,
          collect_args( Target, Rest1, GlobalConfig
                      , [Package#package{ vsn = Vsn
                                        , args = Args ++ [{Tag, Vals1}]
                                        } | OtherPackages]
                      , Flags);
        global ->
          GlobalConfig1 =
            if
              Tag == config_set ->
                [K, V1] = Vals1,
                K1 = list_to_atom(K),
                [case K1 of
                   repo_plugins -> {K1, epm_util:eval(V1 ++ ".")};
                   _ -> {K1, V1}
                 end |proplists:delete(K1, GlobalConfig)];
              true ->
                GlobalConfig
            end,
          collect_args(Target, Rest1, GlobalConfig1, Packages, [{Tag, Vals1}|Flags])
      end
  end.

%% @spec parse_tag(Target, Arg) -> {Tag, HasValue} | undefined
%%		 Target = atom()
%%		 Arg = string()
%%		 Tag = atom()
%%		 HasValue = bool()
parse_tag(install, "--tag") -> {project, tag, 1};
parse_tag(install, "--branch") -> {project, branch, 1};
parse_tag(install, "--sha") -> {project, sha, 1};
parse_tag(install, "--prebuild-command") -> {project, prebuild_command, 1};
parse_tag(install, "--build-command") -> {project, build_command, 1};
parse_tag(install, "--test-command") -> {project, test_command, 1};

parse_tag(info, "--tag") -> {project, tag, 1};
parse_tag(info, "--branch") -> {project, branch, 1};
parse_tag(info, "--sha") -> {project, sha, 1};

parse_tag(_, "--with-deps") -> {project, with_deps, 0};
parse_tag(_, "--without-deps") -> {project, without_deps, 0};

parse_tag(config, "--get") -> {global, get, 0};
parse_tag(config, "--set") -> {global, set, 2};
parse_tag(config, "--remove") -> {global, remove, 1};
parse_tag(_, "--verbose") -> {global, verbose, 0};
parse_tag(_, "--config-set") -> {global, config_set, 2};

parse_tag(_, _) -> undefined.

%% -----------------------------------------------------------------------------
%% Replace epm script with most recent
%% -----------------------------------------------------------------------------
update_epm() ->
  File =
    case os:find_executable("epm") of
      false ->
        case filelib:is_regular("epm") of
          true -> "./epm";
          fasle -> exit("failed to find epm executable to replace")
        end;
      F -> F
    end,
  Fork = proplists:get_value(epm_fork, application:get_all_env(epm), "JacobVorreuter"),
  Url = "http://github.com/" ++ Fork ++ "/epm/raw/master/epm",
  case epm_util:http_request(Url, [{"Host", "github.com"}], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Body}} ->
      case file:write_file(File, Body) of
        ok ->
          io:format("+ updated epm (~s) to latest version~n", [File]);
        {error, Reason} ->
          exit(lists:flatten(io_lib:format("failed to overwrite epm executable ~s: ~p~n", [File, Reason])))
      end;
    _ ->
      exit("failed to download latest version of epm")
  end.

%% -----------------------------------------------------------------------------
%% Global Config
%% -----------------------------------------------------------------------------
print_config_values(GlobalConfig) ->
	[io:format("~p\t\t~p~n", [K,V]) || {K,V} <- GlobalConfig].

write_config_file(GlobalConfig) ->
  FileLoc = get(global_config),
  case file:open(FileLoc, [write]) of
    {ok, IoDevice} ->
      io:format(IoDevice, "[~n", []),
      lists:foldl(
        fun({Key, Val}, Count) ->
          if
            Count == 0 -> ok;
            true ->
              io:format(IoDevice, ",~n", [])
          end,
          case {Key, Val} of
            {repo_plugins, [C|_]} when is_integer(C) ->
              io:format(IoDevice, "  {~p, ~s}", [Key, Val]);
            _ ->
              io:format(IoDevice, "  {~p, ~p}", [Key, Val])
          end,
          Count + 1
        end      , 0, GlobalConfig),
      io:format(IoDevice, "~n].~n", []),
      io:format("+ updated .epm config~n");
    {error, Reason} ->
      ?EXIT("failed to update .epm config (~s): ~p", [FileLoc, Reason])
  end.

%% build_project(GlobalConfig, Package) ->
%%   ProjectName = (Package#package.repo)#repository.name,
%%   Props = Package#package.args,
%%   Config =
%%     case file:consult(ProjectName ++ ".epm") of
%%       {ok, [Config0]} -> Config0;
%%       _ -> []
%%     end,
%%   UserSuppliedPrebuildCommand = proplists:get_value(prebuild_command, Props),
%%   UserSuppliedBuildCommand = proplists:get_value(build_command, Props),
%%   UserSuppliedTestCommand = proplists:get_value(test_command, Props),
%%   prebuild(ProjectName, Config, UserSuppliedPrebuildCommand),
%%   build(ProjectName, Config, UserSuppliedBuildCommand),
%%   test(ProjectName, Config, UserSuppliedTestCommand),
%%   install(ProjectName, Config, proplists:get_value(install_dir, GlobalConfig)).

%% prebuild(ProjectName, Config, undefined) ->
%%     case proplists:get_value(prebuild_command, Config) of
%% 		undefined -> ok;
%% 		PrebuildCmd -> prebuild1(ProjectName, PrebuildCmd)
%% 	end;
%% prebuild(ProjectName, _Config, PrebuildCmd) ->
%% 	prebuild1(ProjectName, PrebuildCmd).

%% prebuild1(ProjectName, PrebuildCmd) ->
%% 	io:format("+ running ~s prebuild command~n", [ProjectName]),
%% 	epm_util:print_cmd_output("~s~n", [PrebuildCmd]),
%% 	epm_util:do_cmd(PrebuildCmd, fail).

%% build(ProjectName, Config, undefined) ->
%%   case proplists:get_value(build_command, Config) of
%%     undefined ->
%%       case filelib:is_regular("Makefile") of
%%         true ->
%%           build1(ProjectName, "make");
%%         false ->
%%           case os:find_executable("rebar") of
%%             false ->
%%               exit("failed to build package: No Makefile and rebar not installed");
%%             RebarExec ->
%%               io:format("+ compiling with rebar...~n"),
%%               build1(ProjectName, RebarExec ++ " compile")
%%           end
%%       end;
%%     Cmd ->
%%       build1(ProjectName, Cmd)
%%   end;
%% build(ProjectName, _Config, BuildCmd) ->
%% 	build1(ProjectName, BuildCmd).

%% build1(ProjectName, BuildCmd) ->
%% 	io:format("+ running ~s build command~n", [ProjectName]),
%% 	epm_util:print_cmd_output("~s~n", [BuildCmd]),
%% 	epm_util:do_cmd(BuildCmd, fail).

%% test(ProjectName, Config, undefined) ->
%%     case proplists:get_value(test_command, Config) of
%% 		undefined -> ok;
%% 		TestCmd -> test1(ProjectName, TestCmd)
%% 	end;
%% test(ProjectName, _Config, TestCmd) ->
%% 	test1(ProjectName, TestCmd).

%% test1(ProjectName, TestCmd) ->
%% 	io:format("+ running ~s test command~n", [ProjectName]),
%% 	epm_util:print_cmd_output("~s~n", [TestCmd]),
%% 	epm_util:do_cmd(TestCmd, fail).
