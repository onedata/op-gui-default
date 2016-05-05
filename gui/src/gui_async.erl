%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This module allows for easy creation and management of asynchronous
%%% processes that can aid in pushing information about
%%% model changes to client side.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_async).
-author("Lukasz Opiola").

-include_lib("ctool/include/logging.hrl").
%% API
-export([spawn/1, kill_async_processes/0]).
-export([push_created/2, push_created/3]).
-export([push_updated/2, push_updated/3]).
-export([push_deleted/2, push_deleted/3]).

% Keys in process dictionary used to store PIDs of processes.
-define(WEBSCOKET_PROCESS_KEY, ws_process).
-define(ASYNC_PROCESSES_KEY, async_processes).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an asynchronous process that can communicate with the calling
%% process. Functions push_created/2, push_updated/2 and push_deleted/2 can be
%% called from the async process to push information through WebSocket
%% channel to the client about model changes.
%% @end
%%--------------------------------------------------------------------
-spec spawn(Fun :: fun()) -> {ok, Pid :: pid()}.
spawn(Fun) ->
    % Prevent async proc from killing the calling proc on crash
    process_flag(trap_exit, true),
    WSPid = self(),
    Pid = spawn_link(fun() -> async_init(WSPid, Fun) end),
    append_async_process(Pid),
    {ok, Pid}.


%%--------------------------------------------------------------------
%% @doc
%% Kills all sync processes that have been spawned by the calling process.
%% @end
%%--------------------------------------------------------------------
-spec kill_async_processes() -> ok.
kill_async_processes() ->
    lists:foreach(
        fun(Pid) ->
            exit(Pid, kill)
        end, get_async_processes()),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about record creation to the client via WebSocket
%% channel. The Data is a proplist that will be translated to JSON, it must
%% include <<"id">> field.
%% This variant can be used only from a process spawned by gui_async:spawn in
%% backend init callback.
%% @end
%%--------------------------------------------------------------------
-spec push_created(ResType :: binary(), Data :: proplists:proplist()) -> ok.
push_created(ResourceType, Data) ->
    push_created(ResourceType, Data, get(?WEBSCOKET_PROCESS_KEY)).


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about record creation to the client via WebSocket
%% channel. The Data is a proplist that will be translated to JSON, it must
%% include <<"id">> field.
%% Pushes the change to given pid.
%% @end
%%--------------------------------------------------------------------
-spec push_created(ResType :: binary(), Data :: proplists:proplist(),
    Pid :: pid()) -> ok.
push_created(ResourceType, Data, Pid) ->
    Pid ! {push_created, ResourceType, Data},
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about model update to the client via WebSocket channel.
%% The Data is a proplist that will be translated to JSON, it must include
%% <<"id">> field. It might also be the updated data of many records.
%% This variant can be used only from a process spawned by gui_async:spawn in
%% backend init callback.
%% @end
%%--------------------------------------------------------------------
-spec push_updated(ResType :: binary(), Data :: proplists:proplist()) -> ok.
push_updated(ResourceType, Data) ->
    push_updated(ResourceType, Data, get(?WEBSCOKET_PROCESS_KEY)).


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about model update to the client via WebSocket channel.
%% The Data is a proplist that will be translated to JSON, it must include
%% <<"id">> field. It might also be the updated data of many records.
%% Pushes the change to given pid.
%% @end
%%--------------------------------------------------------------------
-spec push_updated(ResType :: binary(), Data :: proplists:proplist(),
    Pid :: pid()) -> ok.
push_updated(ResourceType, Data, Pid) ->
    Pid ! {push_updated, ResourceType, Data},
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about record deletion from model to the client
%% via WebSocket channel.
%% This variant can be used only from a process spawned by gui_async:spawn in
%% backend init callback.
%% @end
%%--------------------------------------------------------------------
-spec push_deleted(ResType :: binary(), IdOrIds :: binary() | [binary()]) -> ok.
push_deleted(ResourceType, IdOrIds) ->
    push_deleted(ResourceType, IdOrIds, get(?WEBSCOKET_PROCESS_KEY)).


%%--------------------------------------------------------------------
%% @doc
%% Pushes an information about record deletion from model to the client
%% via WebSocket channel.
%% Pushes the change to given pid.
%% @end
%%--------------------------------------------------------------------
-spec push_deleted(ResType :: binary(), IdOrIds :: binary() | [binary()],
    Pid :: pid()) -> ok.
push_deleted(ResourceType, IdOrIds, Pid) ->
    Ids = case IdOrIds of
        Bin when is_binary(Bin) ->
            [Bin];
        List when is_list(List) ->
            List
    end,
    Pid ! {push_deleted, ResourceType, Ids},
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Function called to initialize async_process and store the WebSocket
%% process key in its dictionary. Then, its main function is evaluated.
%% @end
%%--------------------------------------------------------------------
-spec async_init(WSPid :: pid(), Fun :: fun()) -> term().
async_init(WSPid, Fun) ->
    put(?WEBSCOKET_PROCESS_KEY, WSPid),
    Fun().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns all async processes spawned by the calling process.
%% @end
%%--------------------------------------------------------------------
-spec get_async_processes() -> [pid()].
get_async_processes() ->
    case get(?ASYNC_PROCESSES_KEY) of
        undefined ->
            [];
        List when is_list(List) ->
            List
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add the pid of an async process to the list of processes started by
%% the calling process.
%% @end
%%--------------------------------------------------------------------
-spec append_async_process(Pid :: pid()) -> ok.
append_async_process(Pid) ->
    put(?ASYNC_PROCESSES_KEY, [Pid | get_async_processes()]),
    ok.
