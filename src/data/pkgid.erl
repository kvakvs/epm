%%%-----------------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Wrapper for data types
%%% @end
%%% Created : 23. Feb 2014 6:32 PM
%%%-----------------------------------------------------------------------------
-module(pkgid).

%% API
-export([new/0, set/3
        , author/1, pkg_name/1, platform/1, vsn/1, erlang_vsn/1, args/1
        , arg_bool/2, set_arg_bool/3
        , as_string/1, matches/2, pkgid_match_spec/1
        , fixture_/3, fixture_/4]).

-include("epm.hrl").

%% Unified global identifier for package
-record(pkgid, { author     = ?any_author     :: string()    | ?any_author
               , pkg_name   = ?any_name       :: string()    | ?any_name
               , platform   = ?any_platform   :: platform()
               , vsn        = ?any_vsn        :: string()    | ?any_vsn
               , erlang_vsn = ?any_erlang_vsn :: erlangvsn() | ?any_erlang_vsn
  %% arguments passed from command line, like: source, {tag|branch|hash, x}
               , args=[] :: [pkg_arg()]
               }).
-type pkgid() :: #pkgid{}.
-export_type([pkgid/0]).

%%%-----------------------------------------------------------------------------
new() -> #pkgid{}.

set(author, X, R=#pkgid{}) -> R#pkgid{author=X};
set(pkg_name, X, R=#pkgid{}) -> R#pkgid{pkg_name=X};
set(platform, X, R=#pkgid{}) -> R#pkgid{platform=X};
set(vsn, X, R=#pkgid{}) -> R#pkgid{vsn=X};
set(erlang_vsn, X, R=#pkgid{}) -> R#pkgid{erlang_vsn=X};
set(args, X, R=#pkgid{}) -> R#pkgid{args=X}.

author(#pkgid{author=X}) -> X.

pkg_name(#pkgid{pkg_name=X}) -> X.

platform(#pkgid{platform=X}) -> X.

vsn(#pkgid{vsn=X}) -> X.

erlang_vsn(#pkgid{erlang_vsn=X}) -> X.

args(#pkgid{args=X}) -> X.

%%%-----------------------------------------------------------------------------
arg_bool(Arg, #pkgid{args=X}) -> lists:member(Arg, X).

set_arg_bool(Arg, false, #pkgid{args=Args}=Pkgid) ->
  Pkgid#pkgid{args = lists:delete(Arg, Args)};
set_arg_bool(Arg, true, #pkgid{args=Args}=Pkgid) ->
  Pkgid#pkgid{args = [Arg | lists:delete(Arg, Args)]}.

%% @doc Stringify
as_string(#pkgid{ author=A, pkg_name=N, vsn=V, platform=P, erlang_vsn=E
                          , args=Args}) ->
  "["
  ++ case A of ?any_author -> ""; _ -> epm:s("~s/", [A]) end
    ++ N
    ++ case V of ?any_vsn -> ""; _ -> epm:s(", ~s", [V]) end
    ++ case P of ?any_platform -> ""; _ -> epm:s(", ~s", [P]) end
    ++ case E of ?any_erlang_vsn -> ""; _ -> epm:s(", ~s", [E]) end
    ++ case Args of [] -> ""; _ -> epm:s(" ~p", [Args]) end
    ++ "]".


%% @doc Checks that P1 with some wildcard fields matches P2
matches(#pkgid{}=P1, #pkgid{}=P2) ->
  P1Author = author(P1),
  P1V = vsn(P1),
  P1Erlang = erlang_vsn(P1),
  P1Platf = platform(P1),
  ( pkg_name(P1) =:= pkg_name(P2)
    andalso (P1Author =:= author(P2) orelse P1Author =:= ?any_author)
    andalso (P1V =:= vsn(P2) orelse P1V =:= ?any_vsn)
    andalso (P1Erlang =:= erlang_vsn(P2) orelse P1Erlang =:= ?any_erlang_vsn)
    andalso (P1Platf =:= platform(P2) orelse P1Platf =:= ?any_platform)
  ).

pkgid_match_spec(#pkgid{ author=A1, pkg_name=N1, platform=P1
                                  , vsn=V1, erlang_vsn=E1}) ->
  ets:fun2ms(
    fun(Pkg) when ?IS_PKG(Pkg) ->
      Id = pkg:id(Pkg),
      A2 = pkgid:author(Id),
      N2 = pkgid:pkg_name(Id),
      P2 = pkgid:platform(Id),
      V2 = pkgid:vsn(Id),
      E2 = pkgid:erlang_vsn(Id),
      (N1 =:= N2
        andalso (A1 =:= A2 orelse A1 =:= ?any_author)
        andalso (V1 =:= V2 orelse V1 =:= ?any_vsn)
        andalso (E1 =:= E2 orelse E1 =:= ?any_erlang_vsn)
        andalso (P1 =:= P2 orelse P1 =:= ?any_platform)
      )
    end).

%%%-----------------------------------------------------------------------------
fixture_(A, N, V) -> #pkgid{author=A, pkg_name=N, vsn=V}.
fixture_(A, N, V, P) -> #pkgid{author=A, pkg_name=N, vsn=V, platform=P}.