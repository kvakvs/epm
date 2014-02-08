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

-define(local_index, epm_local_index).
-define(local_index_file, "epm_local_index").

-define(global_index, epm_index).
-define(global_index_file, "epm_index").

%%------------------------------------------------------------------------------
close() ->
  dets:close(?local_index),
  dets:close(?global_index).

open(_Home, EpmHome) ->
  LocalIndexFile = ?local_index_file,
  open_dets_file(LocalIndexFile, ?local_index),

  IndexFile = filename:join([EpmHome, ?global_index_file]),
  open_dets_file(IndexFile , ?global_index),

  State = #epm_state{},
  State.

list_local_packages() ->
  dets:match(?local_index, '$1').

%% @doc Provide '_' if field is not relevant
list_local_by(User, ProjectName, Version) ->
  dets:match(?local_index, {{User, ProjectName, Version}, '$1'}).

delete_local(Key={_User, _Name, _Vsn}) ->
  dets:delete(?local_index, Key).

insert_local(Key={_User, _Name, _Vsn}, Package=#pkg{}) ->
  dets:insert(?local_index, {Key, Package}).

%% @private
open_dets_file(File, Table) ->
  case dets:open_file(Table, [{type, set}, {file, File}]) of
    {ok, _} -> ok;
    {error, {file_error, _, eacces}} ->
      ?EXIT("insufficient access to epm index file: ~s", [File]);
    {error, Reason} ->
      ?EXIT("failed to open epm index file (~s): ~p", [File, Reason])
  end.