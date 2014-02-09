-module(epm).
-export([main/1
        , author/1
        , name/1
        , vsn/1
        , erlang_vsn/1
        , as_string/1
        , matches/2
        , platform/1
        , p/3, p/2, p/1
        , s/2, pkgid_match_spec/1]).

-include("epm.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

main(Args) ->
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  lists:map(fun application:start/1
           , [sasl, crypto, public_key, ssl, ibrowse, epm]),

  case (catch main_internal(Args)) of
    {'EXIT', {ok, Msg}} when is_list(Msg)    -> epm:p(red, "- ~s~n", [Msg]);
    {'EXIT', {error, Msg}} when is_list(Msg) -> epm:p(green, "- ~s~n", [Msg]);
    {'EXIT', Other}                      -> epm:p(dark_cyan, "~p~n", [Other]);
    _ -> ok
  end,
  epm_index:close(),
	io:format("~n").

main_internal(Args) ->
  epm_cfg:init(),
  Home = epm_util:home_dir(),
  EpmHome = epm_util:epm_home_dir(Home),
  State = epm_index:open(Home, EpmHome),
  setup_proxy(),
  epm_core:execute(State, Args).

setup_proxy() ->
  epm_util:set_http_proxy( epm_cfg:get(proxy_host, none)
                         , epm_cfg:get(proxy_port, none)),
  epm_util:set_net_timeout(epm_cfg:get(net_timeout, 6000)).

%%------------------------------------------------------------------------------
author(#pkg{id=#pkgid{author=X}}) -> X;
author(#pkgid{author=X}) -> X.

name(#pkg{id=#pkgid{pkg_name=X}}) -> X;
name(#pkgid{pkg_name=X}) -> X;
name(#repo{id=#repoid{name=X}}) -> X.

vsn(#pkg{id=#pkgid{vsn=X}}) -> X;
vsn(#pkgid{vsn=X}) -> X.

platform(#pkg{id=#pkgid{platform=X}}) -> X;
platform(#pkgid{platform=X}) -> X.

erlang_vsn(#pkg{id=#pkgid{erlang_vsn=X}}) -> X;
erlang_vsn(#pkgid{erlang_vsn=X}) -> X.

%%------------------------------------------------------------------------------
as_string(#pkg{id=Id}) -> as_string(Id);
as_string(#pkgid{author=A, pkg_name=N, vsn=V, platform=P, erlang_vsn=E}) ->
  "["
    ++ case A of ?any_author -> ""; _ -> s("~s/", [A]) end
    ++ N
    ++ case V of ?any_vsn -> ""; _ -> s(", ~s", [V]) end
    ++ case P of ?any_platform -> ""; _ -> s(", ~s", [P]) end
    ++ case E of ?any_erlang_vsn -> ""; _ -> s(", ~s", [E]) end
    ++ "]";
as_string(#repo{id=I, description=_D, url=U}) ->
  s("[Repo ~s url=~s]", [I, U]);
as_string(#repoid{name=N}) ->
  s("[Repo id ~s]", [N]).

%% @doc Checks that P1 with some wildcard fields matches P2
matches(#pkgid{}=P1, #pkgid{}=P2) ->
  P1Author = author(P1),
  P1V = vsn(P1),
  P1Erlang = erlang_vsn(P1),
  P1Platf = platform(P1),
  ( name(P1) =:= name(P2)
    andalso (P1Author =:= author(P2) orelse P1Author =:= ?any_author)
    andalso (P1V =:= vsn(P2) orelse P1V =:= ?any_vsn)
    andalso (P1Erlang =:= erlang_vsn(P2) orelse P1Erlang =:= ?any_erlang_vsn)
    andalso (P1Platf =:= platform(P2) orelse P1Platf =:= ?any_platform)
    ).

pkgid_match_spec(#pkgid{ author=A1, pkg_name=N1, platform=P1
                       , vsn=V1, erlang_vsn=E1}) ->
  ets:fun2ms(fun(#pkg{id=#pkgid{ author=A2, pkg_name=N2, platform=P2
                               , vsn=V2, erlang_vsn=E2}}=Pkg)
      when (N1 =:= N2
          andalso (A1 =:= A2 orelse A1 =:= ?any_author)
          andalso (V1 =:= V2 orelse V1 =:= ?any_vsn)
          andalso (E1 =:= E2 orelse E1 =:= ?any_erlang_vsn)
          andalso (P1 =:= P2 orelse P1 =:= ?any_platform)
    ) -> Pkg end).

%%------------------------------------------------------------------------------
%% @doc Uncolored print
p(Text) -> io:format(Text).

%% @doc Colored print and formatted uncolored print
p(Color, Text) when is_atom(Color) ->
  p(ansi_color(Color) ++ Text ++ ansi_endfont());
p(Text, Args) -> io:format(Text, Args).

%% @doc Formatted colored print
p(Color, Format, Args) when is_atom(Color) ->
  p(ansi_color(Color) ++ Format ++ ansi_endfont() ++ "~n", Args).

%% @doc sprintf
s(Format, Args) ->
  lists:flatten(io_lib:format(Format, Args)).

%% @doc ANSI ESCape color codes: reset font color/weight
ansi_endfont() -> [27 | "[0m"].

%% @doc ANSI ESCape color codes: font color
ansi_color(black) -> [27 | "[1;30m"];
ansi_color(red) -> [27 | "[1;31m"];
ansi_color(green) -> [27 | "[1;32m"];
ansi_color(yellow) -> [27 | "[1;33m"];
ansi_color(blue) -> [27 | "[1;34m"];
ansi_color(magenta) -> [27 | "[1;35m"];
ansi_color(cyan) -> [27 | "[1;36m"];
ansi_color(white) -> [27 | "[1;37m"];

ansi_color(dark_blue) -> [27 | "[2;34m"];
ansi_color(dark_red) -> [27 | "[2;31m"];
ansi_color(dark_white) -> [27 | "[2;37m"];
ansi_color(dark_green) -> [27 | "[2;32m"];
ansi_color(dark_cyan) -> [27 | "[2;36m"].
