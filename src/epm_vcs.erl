%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Generic repository handling with automatic VCS type guess
%%% @end
%%% Created : 21 Feb 2014 8:16 PM
%%%-------------------------------------------------------------------
-module(epm_vcs).
-behaviour(gen_vcs).

%% API
-export([get_source/2, install_dir_name/1]).

-include("epm.hrl").

-spec get_source(Pkg :: pkg:pkg()
                , DestDir :: string()
                ) -> ok | {error, any()}.
get_source(Pkg, DestDir) when ?IS_PKG(Pkg) ->
  RepoId = pkg:repo(Pkg),
  %% TODO: auto detection for git/svn protocol
  case epm_index:get_repo(RepoId) of
    not_found ->
      ?EPM_FAIL("[index error] repository was set, but not found for package ~s"
               , [epm:as_string(Pkg)]);
    Repo ->
      epm:p("repo ~p", [Repo]),
      ApiMod = Repo#repo.api_module,
      ApiMod:get_source(Pkg, DestDir)
  end.

%% @doc Naming scheme for local install
%% TODO: Configure naming format per project/per user
install_dir_name(Pkg) when ?IS_PKG(Pkg) ->
  Id     = pkg:id(Pkg),
  RepoId = pkg:repo(Pkg),
  A = pkgid:author(Id),
  N = pkgid:pkg_name(Id),
  #repo{short_name=RepoSN} = epm_index:get_repo(RepoId),
  epm:s("~s.~s.~s", [N, A, RepoSN]).