-module(epm_command_line).
-export([execute/1]).

-include("epm.hrl").

-type command() :: install | wrap | remove | update | config | info | search.

execute(["install" | Args]) ->
  {_Pkgids, Flags} = collect_args(wrap, Args),
  epm_cfg:set(verbose, lists:member(verbose, Flags)),
  ok;
execute(["install" | Args]) ->
  {Pkgids, Flags} = collect_args(install, Args),
  epm_cfg:set(verbose, lists:member(verbose, Flags)),

  Deps = epm_deps:resolve_dependencies(Pkgids),
  %Packages = lists:flatten(lists:map(fun epm_index:find_package/1, Pkgids)),
  epm:p("ids=~p~ndeps=~p~n", [Pkgids, Deps]),
  {Installed, NotInstalled} = epm_ops:filter_installed_packages(Deps),
  case NotInstalled of
    [] ->
      epm:p("+ nothing to do: packages and dependencies already installed~n");
    _ ->
      case Installed of
        [] -> ok;
        _ ->
          epm:p(green, "Packages already installed:~n"),
          [epm:p(dark_green, "    + ~s~n", [epm:as_string(P)]) || P <- Installed]
      end,
      epm:p(white, "Install the following packages? ([y]/n)~n"),
      [epm:p("    + ~s~n", [epm:as_string(P)]) || P <- NotInstalled],
      case io:get_chars("", 1) of
        C when C == "y"; C == "\n" ->
          lists:map(fun epm_ops:install_package/1, NotInstalled);
        _ -> ok
      end
  end;
execute(["remove" | Args]) ->
  {Packages, Flags} = collect_args(remove, Args),
  epm_cfg:set(verbose, lists:member(verbose, Flags)),
  Installed = epm_ops:get_installed_packages(Packages),
  case Installed of
    [] ->
      epm:p("+ nothing to remove: no matching packages installed~n");
    _ ->
      epm:p("===============================~n"
            "Remove the following packages?~n"
            "===============================~n"),
      [epm:p("    + ~s~n", [epm:as_string(P)]) || P <- Installed],
      epm:p(white, "~n([y]/n) "),
      case io:get_chars("", 1) of
        C when C == "y"; C == "\n" ->
          io:format("~n"),
          lists:map(fun epm_ops:remove_package/1, Installed);
        _ -> ok
      end
  end;
execute(["update" | Args]) ->
  {Packages, Flags} = collect_args(update, Args),
  epm_cfg:set(verbose, lists:member(verbose, Flags)),
  Installed = epm_ops:get_installed_packages(Packages),
  case Installed of
    [] ->
      epm:p("- nothing to update~n");
    _ ->
      epm:p("===============================~n"
            "Update the following packages?~n"
            "===============================~n"),
      [epm:p("    + ~s~n", [epm:as_string(P)]) || P <- Installed],
      epm:p(white, "~n([y]/n) "),
      case io:get_chars("", 1) of
        C when C == "y"; C == "\n" ->
          io:format("~n"),
          lists:map(fun epm_ops:update_package/1, Installed);
        _ -> ok
      end
  end;
execute(["info" | Args]) ->
  {Packages, _Flags} = collect_args(info, Args),
  {Installed, NotInstalled} = epm_ops:filter_installed_packages(Packages),
  case Installed of
    [] -> ok;
    _ ->
      epm:p("===============================~n"
            "INSTALLED~n"
            "===============================~n"),

      F = fun(Package, Count) when ?IS_PKG(Package) ->
            case Count of
              0 -> ok;
              _ -> epm:p("~n")
            end,
            epm_ops:print_installed_package_info(Package),
            Count + 1
          end,
      lists:foldl(F, 0, lists:reverse(Installed))
  end,

  case NotInstalled of
    [] -> ok;
    _ ->
      case Installed of
        [] -> ok;
        _ -> epm:p("~n")
      end,
      epm_ops:print_not_installed_package_info(NotInstalled, true)
  end;
execute(["search" | Args]) ->
  {Packages, _Flags} = collect_args(search, Args),
  epm_ops:print_not_installed_package_info(lists:reverse(Packages));
execute(["list" | _Args]) ->
  Installed = epm_ops:installed_packages(),
  case Installed of
    [] ->
      epm:p("- no packages installed~n");
    _ ->
      epm:p("===============================~n"
            "INSTALLED~n"
            "===============================~n"),

      F = fun(Package, Count) ->
          case Count of
            0 -> ok;
            _ -> epm:p("~n")
          end,
          epm_ops:print_installed_package_info(Package),
          Count + 1
        end,
      lists:foldl(F, 0, lists:reverse(Installed))
  end;
execute(["latest" | _Args]) -> epm_ops:update_epm();
execute(["config" | Args]) ->
  {_Packages, Flags} = collect_args(config, Args),
  case Flags of
    [] ->
      epm_cfg:print_config_values();
    [get] ->
      epm_cfg:print_config_values();
    _ ->
      F = fun({set, [K, V]}) -> epm_cfg:set(K, V);
             ({remove, K}) -> epm_cfg:delete(K);
             (_) -> ok
          end,
      _ = lists:foreach(F, Flags),
      epm_cfg:write_config_file()
  end;
execute(_) ->
  epm:p("Usage: epm commands~n"
  %------------------------------
  "    install [<author>/]<project> {project options}, ... {global options}~n"
  "      project options:~n"
  "        --source :: request source install~n"
  "        --platform x86|x64, --vsn <v>, --tag <t>, --branch <b>, --hash <h>~n"
  %------------------------------
  "    wrap [path] -- looks for epm.json in 'path' and wraps an package zip~n"
  %------------------------------
%%         "             --with-deps (default)~n"
%%         "             --without-deps~n"
%%         "             --prebuild-command <cmd>~n"
%%         "             --build-command <cmd>~n"
%%         "             --test-command <cmd>~n"
        "        global options:~n"
%%         "             --verbose~n"
        "             --config-set <key> <value>~n~n"
%%         "    remove [<user>/]<project> {project options}, ... {global options}~n"
%%         "        project options:~n"
%%         "             --tag <tag>~n"
%%         "             --branch <branch>~n"
%%         "             --sha <sha>~n"
%%         "        global options:~n"
%%         "             --verbose~n~n"
%%         "             --config-set <key> <value>~n~n"
%%         "    update [<user>/]<project> {project options}, ... {global options}~n"
%%         "        project options:~n"
%%         "             --tag <tag>~n"
%%         "             --branch <branch>~n"
%%         "             --sha <sha>~n"
%%         "             --with-deps~n"
%%         "             --without-deps (default)~n"
%%         "        global options:~n"
%%         "             --verbose~n~n"
%%         "             --config-set <key> <value>~n~n"
%%         "    info [<user>/]<project>, ... {global options}~n"
%%         "        global options:~n"
%%         "             --config-set <key> <value>~n~n"
%%         "    search <project>, ... {global options}~n"
%%         "        global options:~n"
%%         "             --config-set <key> <value>~n~n"
%%         "    list~n~n"
%%         "    latest~n~n"
        "    config {options}~n"
        "        options:~n"
        "             --get (default)~n"
        "             --set <key> <value>~n"
        "             --remove <key>~n"),
  ok.

%% -----------------------------------------------------------------------------
%% parse input args
%% -----------------------------------------------------------------------------
-spec collect_args( Target :: command()
                  , Args :: [string()]
                  ) -> {[pkgid:pkgid()], [atom()]}.
collect_args(Target, Args) ->
  collect_args_internal(Target, Args, [], []).

-spec collect_args_internal(Target :: atom()
                           , Args :: [string()]
                           , Pkgids :: [pkgid:pkgid()]
                           , Flags :: [atom()])
      -> {[pkgid:pkgid()], [atom()]}.
collect_args_internal(_, [], Packages, Flags) ->
  {lists:reverse(Packages), lists:reverse(Flags)};
collect_args_internal(Target, [Arg | Rest], Pkgids, Flags) ->
  case parse_tag(Target, Arg) of
    undefined -> %% if not a tag then must be a project name
      %% split into user and project
      {ProjectName, User} = epm_ops:split_package(Arg),
      NewP0 = pkgid:new(),
      NewP1 = pkgid:set(author, User, NewP0),
      NewP  = pkgid:set(pkg_name, ProjectName, NewP1),
      collect_args_internal(Target, Rest, [NewP|Pkgids], Flags);
    {Type, Tag, 0} ->   %% tag with no trailing value
      case Type of
        project ->
          %% Update head of Pkgids with additional arg
          [Pkgid|Tail] = Pkgids,
          Args = pkgid:args(Pkgid),
          Pkgid1 = pkgid:set(args, Args ++ [Tag], Pkgid),
          collect_args_internal(Target, Rest, [Pkgid1 | Tail], Flags);
        global ->
          collect_args_internal(Target, Rest, Pkgids, [Tag|Flags])
      end;
    {Type, Tag, NumVals} when is_integer(NumVals) -> % tag with trailing value(s)
      if length(Rest) < NumVals ->
          ?EPM_FAIL("poorly formatted command", []);
        true -> ok
      end,
      {Vals, Rest1} = lists:split(NumVals, Rest),
      Vals1 = case Vals of
                [V] -> V;
                _ -> Vals
              end,
      case Type of
        project ->
          %% Update head of Pkgids with additional arg
          %% this tag applies to the last project on the stack
          [Pkgid|Tail] = Pkgids,
          Args = pkgid:args(Pkgid),
          Vsn = if Tag == tag; Tag == branch; Tag == hash -> Vals1;
                  true -> pkgid:vsn(Pkgid)
                end,
          Pkgid2 = pkgid:set(vsn, Vsn, Pkgid),
          Pkgid3 = pkgid:set(args, Args ++ [{Tag, Vals1}], Pkgid2),
          collect_args_internal( Target, Rest1, [Pkgid3 | Tail], Flags);
        global ->
          if Tag == config_set ->
              [K, V1] = Vals1,
              K1 = list_to_atom(K),
              case K1 of
                 repo_plugins ->
                   epm_cfg:set(K1, epm_util:parse_erlang_term(V1 ++ "."));
                 _ -> ok
              end;
            true -> ok
          end,
          collect_args_internal(Target, Rest1, Pkgids, [{Tag, Vals1}|Flags])
      end
  end.

-spec parse_tag( Target :: command()
               , Arg :: string()
               ) -> {Tag :: atom(), HasValue :: bool} | undefined.
parse_tag(install, "--source") -> {project, source, 0};
parse_tag(install, "--tag") -> {project, tag, 1};
parse_tag(install, "--branch") -> {project, branch, 1};
parse_tag(install, "--hash") -> {project, hash, 1};
%% parse_tag(install, "--prebuild-command") -> {project, prebuild_command, 1};
%% parse_tag(install, "--build-command") -> {project, build_command, 1};
%% parse_tag(install, "--test-command") -> {project, test_command, 1};

%% parse_tag(info, "--tag") -> {project, tag, 1};
%% parse_tag(info, "--branch") -> {project, branch, 1};
%% parse_tag(info, "--sha") -> {project, sha, 1};

%% parse_tag(_, "--with-deps") -> {project, with_deps, 0};
%% parse_tag(_, "--without-deps") -> {project, without_deps, 0};

parse_tag(config, "--get") -> {global, get, 0};
parse_tag(config, "--set") -> {global, set, 2};
parse_tag(config, "--remove") -> {global, remove, 1};
parse_tag(_, "--verbose") -> {global, verbose, 0};
parse_tag(_, "--config-set") -> {global, config_set, 2};

parse_tag(_, _) -> undefined.


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
