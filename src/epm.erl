-module(epm).
-export([main/1]).
-include("epm.hrl").

main(Args) ->
  application:load(sasl),
  application:set_env(sasl, sasl_error_logger, false),
  lists:map(fun application:start/1, [sasl, crypto, public_key, ssl, ibrowse, epm]),

  case (catch main_internal(Args)) of
    {'EXIT', ErrorMsg} when is_list(ErrorMsg) ->
      io:format("- ~s~n", [ErrorMsg]);
    {'EXIT', Other} ->
      io:format("~p~n", [Other]);
    _ ->
      ok
  end,

  epm_index:close(),
	io:format("~n").

main_internal(Args) ->
  epm_cfg:init(),
  Home = epm_util:home_dir(),
  EpmHome = epm_util:epm_home_dir(Home),
  State = epm_index:open(Home, EpmHome),
  setup_connectivity(),
  epm_core:execute(State, Args).

setup_connectivity() ->
  epm_util:set_http_proxy( epm_cfg:get(proxy_host, none)
                         , epm_cfg:get(proxy_port, none)),
  epm_util:set_net_timeout(epm_cfg:get(net_timeout, 6000)).
