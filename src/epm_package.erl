-module(epm_package).
-export([download_tarball/2]).

-include("epm.hrl").

download_tarball(Repo, Url) ->
%%   LocalProjectDir = Repo#repo.owner ++ "-" ++ Repo#repo.name,
%%   io:format("+ downloading ~s~n", [Url]),
%%   case epm_util:http_request(Url, undefined, [{response_format, binary}]) of
%%     {ok, "200", _, Bin} ->
%%       case erl_tar:table({binary, Bin}, [compressed]) of
%%         {ok, Files} ->
%%           TarName = tarname(Files),
%%           epm_util:del_dir(TarName),
%%           epm_util:del_dir(LocalProjectDir),
%%           case erl_tar:extract({binary, Bin}, [compressed]) of
%%             ok ->
%%               epm_util:rn_dir(TarName, LocalProjectDir),
%%               LocalProjectDir;
%%             {error, Reason} ->
%%               ?EXIT("failed to extract ~s tarball: ~p", [Repo#repo.name, Reason])
%%           end;
%%         {error, Reason1} ->
%%           ?EXIT("failed to extract ~s tarball: ~p", [Repo#repo.name, Reason1])
%%       end;
%%     {ok, "404", _, _} ->
%%       ?EXIT("remote project does not exist: ~s", [Url]);
%%     Error ->
%%       io:format("~p~n", [Error]),
%%       ?EXIT("failed to download ~s tarball: ~s", [Repo#repo.name, Url])
%%   end.
  ok.

%% tarname([]) ->
%%   exit("package tarball does not contain proper folder structure");
%% tarname([File|Tail]) ->
%%   case re:run(File, "/") of
%%     nomatch -> tarname(Tail);
%%     _ -> hd(string:tokens(File, "/"))
%%   end.
