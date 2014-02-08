-module(epm).
-export([main/1
        , author/1, name/1, vsn/1, erlang_vsn/1, as_string/1, equals/2]).

-include("epm.hrl").

main(Args) ->
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  lists:map(fun application:start/1
           , [sasl, crypto, public_key, ssl, ibrowse, epm]),

  case (catch main_internal(Args)) of
    {'EXIT', Msg} when is_list(Msg) -> io:format("- ~s~n", [Msg]);
    {'EXIT', Other}                 -> io:format("~p~n", [Other]);
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
author(#pkg{id=#pkgid{author=X}}) -> X.

name(#pkg{id=#pkgid{pkg_name=X}}) -> X;
name(#repo{id=#repoid{name=X}}) -> X.

vsn(#pkg{id=#pkgid{vsn=X}}) -> X.

platform(#pkg{id=#pkgid{platform=X}}) -> X.

erlang_vsn(#pkg{id=#pkgid{erlang_vsn=X}}) -> X.

as_string(#pkgid{author=A, pkg_name=N, vsn=V, platform=P, erlang_vsn=E}) ->
  lists:flatten(io_lib:format("~s/~s/~s ~s/~s", [A,N,V, P,E]));
as_string(#repo{id=I, description=_D, url=U}) ->
  lists:flatten(io_lib:format("Repo ~s url=~s", [I, U])).

equals(#pkgid{}=P1, #pkgid{}=P2) ->
  (author(P1) == author(P2) orelse author(P2) =:= ?any_author)
    andalso (name(P1) == name(P2))
    andalso (vsn(P1) == vsn(P2))
    andalso (erlang_vsn(P1) == vsn(P2))
    andalso (platform(P1) == platform(P2)).
