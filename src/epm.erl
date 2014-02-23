-module(epm).
-export([start/0, main/1
        , p/3, p/2, p/1, s/2
        , as_string/1]).

-include("epm.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

start() ->
  %% Debug
  main(["install", "cowboy"]).

main(Args) ->
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  lists:map(fun application:start/1
           , [sasl, crypto, public_key, ssl, ibrowse, epm]),

  case (catch main_internal(Args)) of
    {'EXIT', {ok, Msg}} when is_list(Msg)    -> epm:p(green, "OK ~s~n", [Msg]);
    {'EXIT', {error, Msg}} when is_list(Msg) -> epm:p(red, "ERROR ~s~n", [Msg]);
    {'EXIT', Other}                   -> epm:p(dark_cyan, "EXIT ~p~n", [Other]);
    _ -> ok
  end,
  epm_index:close(),
	io:format("~n").

main_internal(Args) ->
  Home = epm_util:home_dir(),
  EpmHome = epm_util:epm_home_dir(Home),
  epm_cfg:init(EpmHome),
  State = epm_index:open(EpmHome),
  setup_proxy(),
  epm_core:execute(State, Args).

setup_proxy() ->
  epm_util:set_http_proxy( epm_cfg:get(proxy_host, none)
                         , epm_cfg:get(proxy_port, none)),
  epm_util:set_net_timeout(epm_cfg:get(net_timeout, 6000)).

%%------------------------------------------------------------------------------
%% author(#pkg{id=Id}) -> pkgid:author(Id);
%% author(#pkgid{author=X}) -> X.
%%
%% name(#pkg{id=#pkgid{pkg_name=X}}) -> X;
%% name(#pkgid{pkg_name=X}) -> X;
%% name(#repo{id=#repoid{name=X}}) -> X.
%%
%% vsn(#pkg{id=#pkgid{vsn=X}}) -> X;
%% vsn(#pkgid{vsn=X}) -> X.
%%
%% platform(#pkg{id=#pkgid{platform=X}}) -> X;
%% platform(#pkgid{platform=X}) -> X.
%%
%% erlang_vsn(#pkg{id=#pkgid{erlang_vsn=X}}) -> X;
%% erlang_vsn(#pkgid{erlang_vsn=X}) -> X.
%%
%% args(#pkgid{args=X}) -> X.

%% arg_bool(Arg, #pkgid{args=X}) -> lists:member(Arg, X).

%% arg(Arg, #pkgid{args=X}) -> proplists:get_value(Arg, X).

%% set_arg_bool(Arg, false, #pkgid{args=Args}=Pkgid) ->
%%   Pkgid#pkgid{args = lists:delete(Arg, Args)};
%% set_arg_bool(Arg, true, #pkgid{args=Args}=Pkgid) ->
%%   Pkgid#pkgid{args = [Arg | lists:delete(Arg, Args)]}.

%%------------------------------------------------------------------------------
as_string(#repo{id=I, description=_D, url=U}) ->
  s("[Repo ~s url=~s]", [I, U]);
as_string(#repoid{name=N}) ->
  s("[Repo id ~s]", [N]).

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
