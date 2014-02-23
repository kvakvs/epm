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

-spec get_source(Pkg :: pkg:pkg()
                , DestDir :: string()
                ) -> ok | {error, any()}.
get_source(Pkg, DestDir) ->
  Url = get_vcs_url(Pkg),
  Path = filename:join([DestDir, epm_vcs:install_dir_name(Pkg)]),
  %% TODO: versions, tags
  epm_util:git(["clone", Url, Path]),
  ok.

get_vcs_url(Pkg) when ?IS_PKG(Pkg) ->
  Pkgid = pkg:id(Pkg),
  Author = pkgid:author(Pkgid),
  Name = pkgid:pkg_name(Pkgid),
  if Author =:= ?any_author -> {error, author};
     Name   =:= ?any_name   -> {error, pkg_name};
     true -> epm:s("https://github.com/~s/~s.git", [Author, Name])
  end.
