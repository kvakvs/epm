%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Generic repository handling with automatic VCS type guess
%%% @end
%%% Created : 21 Feb 2014 8:16 PM
%%%-------------------------------------------------------------------
-module(epm_vcs).
-behaviour(gen_vcs).

%% API
-export([get_source/2]).

-include("epm.hrl").

-spec get_source(Pkg :: pkg(), DestDir :: string()) -> ok | {error, any()}.
get_source(Pkg=#pkg{repo=RepoId}, DestDir) ->
  %% TODO: auto detection for git/svn protocol
  case epm_index:get_repo(RepoId) of
    undefined ->
      ?EPM_FAIL("repository not set for package ~s", [epm:s(Pkg)]);
    Repo ->
      epm:p("repo ~p", [Repo]),
      ApiMod = Repo#repo.api_module,
      ApiMod:get_source(Pkg, DestDir)
  end.
