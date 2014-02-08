%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% @doc Represents local installed package index and remote package index plus
%%% manipulations (search, dependencies etc).
%%% @end
%%% Created : 08. Feb 2014 2:47 PM
%%%-------------------------------------------------------------------
-module(epm_index).

%% API
-export([ open/2
        , list_local_packages/0
        , list_local_by/3
        , delete_local/1
        , insert_local/2
        , close/0]).

-include("epm.hrl").

-define(local_index, epm_index).

%%------------------------------------------------------------------------------
close() ->
  dets:close(?local_index).

open(Home, EpmHome) ->
  File = filename:join([EpmHome, ?epm_index_filename]),

  %% TODO: delete this later
  Insert =
    case filelib:is_regular(filename:join([Home, ?epm_index_filename])) of
      true ->
        DetsPath = filename:join([Home, ?epm_index_filename]),
        case dets:open_file(?local_index, [{type, set}, {file, DetsPath}]) of
          {ok, _} ->
            Rows = dets:match(?local_index, '$1'),
            dets:close(?local_index),

            [{{User, Name, Vsn}
             , #epm_package{user = User
                      , name = Name
                      , vsn = Vsn
                      , install_dir = InstallDir
                      , deps = Deps
                      %, repo = github_api:info(User, Name)
                      }}
              || [{{User, Name, Vsn}, InstallDir, Deps}] <- Rows];
          _ -> []
        end;
      false -> []
    end,

  case dets:open_file(?local_index, [{type, set}, {file, File}]) of
    {ok, _} ->
      %% TODO: delete this later
      [dets:insert(?local_index, I) || I <- Insert],
      file:delete(filename:join([Home, ?epm_index_filename])),
      ok;
    {error, {file_error, _, eacces}} ->
      ?EXIT("insufficient access to epm index file: ~s", [File]);
    {error, Reason} ->
      ?EXIT("failed to open epm index file (~s): ~p", [File, Reason])
  end,
  State = #epm_state{},
  State.

list_local_packages() ->
  dets:match(?local_index, '$1').

%% @doc Provide '_' if field is not relevant
list_local_by(User, ProjectName, Version) ->
  dets:match(?local_index, {{User, ProjectName, Version}, '$1'}).

delete_local(Key={_User, _Name, _Vsn}) ->
  dets:delete(?local_index, Key).

insert_local(Key={_User, _Name, _Vsn}, Package=#epm_package{}) ->
  dets:insert(?local_index, {Key, Package}).
