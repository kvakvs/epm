%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Wrapper for data types
%%% @end
%%% Created : 23. Feb 2014 6:32 PM
%%%-------------------------------------------------------------------
-module(pkg).

%% API
-export([new/0, set/3
        , fixture_/3
        , field_position/1
        , id/1, deps/1, args/1, repo/1
        , as_string/1]).

-include("epm.hrl").

%% Package: Does not specify branches/tags as this info is queried from package
%% repository online.
-record(pkg, { id=erlang:error(id_not_set) :: pkgid:pkgid()
             , deps=[] :: [pkgid:pkgid()]
             , args=[] :: list()
             , repo=erlang:error(repo_not_set) :: repoid()
             }).
-type pkg() :: #pkg{}.
-export_type([pkg/0]).

%%%-----------------------------------------------------------------------------
new() -> #pkg{}.

set(id, X, R=#pkg{}) -> R#pkg{id=X};
set(deps, X, R=#pkg{}) -> R#pkg{deps=X};
set(args, X, R=#pkg{}) -> R#pkg{args=X};
set(repo, X, R=#pkg{}) -> R#pkg{repo=X}.

field_position(id) -> #pkg.id.

id(#pkg{id=X}) -> X.

deps(#pkg{deps=X}) -> X.

args(#pkg{args=X}) -> X.

repo(#pkg{repo=X}) -> X.

%%%-----------------------------------------------------------------------------
as_string(#pkg{id=Pkgid}) -> pkgid:as_string(Pkgid).

%%%-----------------------------------------------------------------------------
fixture_(Id, Deps, Repo) -> #pkg{id=Id, deps=Deps, repo=Repo}.