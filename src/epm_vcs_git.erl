%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Handles Github repositories
%%% @end
%%% Created : 09. Feb 2014 8:16 PM
%%%-------------------------------------------------------------------
-module(epm_vcs_git).
-behaviour(gen_vcs).

%% API
-export([get_source/2]).

-include("epm.hrl").

-spec get_source(Pkg :: pkg(), DestDir :: string()) -> ok | {error, any()}.
get_source(Pkg, DestDir) ->
  Url = get_vcs_url(Pkg),
  Path = filename:join([DestDir, epm_vcs:install_dir_name(Pkg)]),
  %% TODO: versions, tags
  epm_util:git(["clone", Url, Path]),
  ok.

get_vcs_url(#pkg{id=#pkgid{author=?any_author}}) -> {error, author};
get_vcs_url(#pkg{id=#pkgid{pkg_name=?any_name}}) -> {error, pkg_name};
get_vcs_url(#pkg{id=#pkgid{author=A, pkg_name=N}}) ->
  epm:s("https://github.com/~s/~s.git", [A, N]).