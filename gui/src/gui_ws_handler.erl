%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module is a cowboy websocket handler that handles the connection
%%% between Ember ws_adapter and server. This channel is used for models
%%% synchronization and performing RPC to the server.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_ws_handler).
-author("Lukasz Opiola").
-behaviour(cowboy_websocket_handler).

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-ifdef(TEST).
-compile(export_all).
-endif.


%% Interface between WebSocket Adapter client and server. Corresponding
%% interface is located in ws_adapter.js.

%% All in-coming JSONs have the following structure (opt = optional field)
%% {
%%   uuid
%%   msgType
%%   resourceType
%%   operation
%%   resourceIds (opt)
%%   data (opt)
%% }
%% All out-coming JSONs have the following structure (opt = optional field)
%% {
%%   uuid (opt, not used in push messages)
%%   msgType
%%   result
%%   data (opt)
%% }

%% Keys corresponding to above structure
-define(KEY_UUID, <<"uuid">>).
-define(KEY_MSG_TYPE, <<"msgType">>).
-define(KEY_RESOURCE_TYPE, <<"resourceType">>).
-define(KEY_OPERATION, <<"operation">>).
-define(KEY_RESOURCE_IDS, <<"resourceIds">>).
-define(KEY_DATA, <<"data">>).
-define(KEY_RESULT, <<"result">>).
%% Message types, identified by ?KEY_MSG_TYPE key
-define(TYPE_MODEL_REQ, <<"modelReq">>).
-define(TYPE_MODEL_RESP, <<"modelResp">>).
-define(TYPE_MODEL_CRT_PUSH, <<"modelPushCreated">>).
-define(TYPE_MODEL_UPT_PUSH, <<"modelPushUpdated">>).
-define(TYPE_MODEL_DLT_PUSH, <<"modelPushDeleted">>).
-define(TYPE_RPC_REQ, <<"RPCReq">>).
-define(TYPE_RPC_RESP, <<"RPCResp">>).
-define(TYPE_PUSH_MESSAGE, <<"pushMessage">>).
%% Operations on model, identified by ?KEY_OPERATION key
-define(OP_FIND, <<"find">>).
-define(OP_FIND_MANY, <<"findMany">>).
-define(OP_FIND_ALL, <<"findAll">>).
-define(OP_FIND_QUERY, <<"findQuery">>).
-define(OP_CREATE_RECORD, <<"createRecord">>).
-define(OP_UPDATE_RECORD, <<"updateRecord">>).
-define(OP_DELETE_RECORD, <<"deleteRecord">>).
%% Defined concerning session RPC
-define(RESOURCE_TYPE_PUBLIC_RPC, <<"public">>).
-define(RESOURCE_TYPE_PRIVATE_RPC, <<"private">>).
-define(RESOURCE_TYPE_SESSION, <<"session">>).
-define(KEY_SESSION_VALID, <<"sessionValid">>).
-define(KEY_SESSION_DETAILS, <<"sessionDetails">>).
%% Operation results
-define(RESULT_OK, <<"ok">>).
-define(RESULT_ERROR, <<"error">>).

-define(DATA_INTERNAL_SERVER_ERROR, <<"Internal Server Error">>).

%%%===================================================================
%%% cowboy_websocket_handler API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Upgrades the protocol to WebSocket.
%% @end
%%--------------------------------------------------------------------
-spec init({TransportName, ProtocolName}, Req, Opts) ->
    {upgrade, protocol, cowboy_websocket} when
    TransportName :: tcp | ssl | atom(),
    ProtocolName :: http | atom(),
    Req :: cowboy_req:req(),
    Opts :: any().
init({_, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


%%--------------------------------------------------------------------
%% @doc
%% Initializes the WebSocket state for current connection.
%% Accepts only connections from .html pages.
%% @end
%%--------------------------------------------------------------------
-spec websocket_init(TransportName, Req, Opts) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {ok, Req, State, Timeout} | {ok, Req, State, Timeout, hibernate} |
    {shutdown, Req} when
    TransportName :: tcp | ssl | atom(),
    Req :: cowboy_req:req(),
    Opts :: any(),
    State :: no_state,
    Timeout :: timeout().
websocket_init(_TransportName, Req, _Opts) ->
    % @todo geneneric error handling + reporting on client side
    {FullPath, _} = cowboy_req:path(Req),
    case gui_html_handler:is_html_req(FullPath) of
        true ->
            % Initialize context
            g_ctx:init(Req, true),
            % Check if the client is allowed to connect to WS
            WSRequirements = g_ctx:websocket_requirements(),
            UserLoggedIn = g_session:is_logged_in(),
            Result = case {WSRequirements, UserLoggedIn} of
                {?WEBSOCKET_DISABLED, _} -> error;
                {?SESSION_ANY, _} -> ok;
                {?SESSION_LOGGED_IN, true} -> ok;
                {?SESSION_NOT_LOGGED_IN, false} -> ok;
                {_, _} -> error
            end,
            case Result of
                ok ->
                    {ok, Req, no_state};
                error ->
                    % The client is not allowed to connect to WS,
                    % send 403 Forbidden.
                    g_ctx:reply(403, [], <<"">>),
                    NewReq = g_ctx:finish(),
                    {shutdown, NewReq}
            end;
        false ->
            % Not a HTML request, send 403 Forbidden.
            {ok, NewReq} = cowboy_req:reply(403, [], <<"">>, Req),
            {shutdown, NewReq}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handles the data received from the Websocket connection.
%% Performs unpacking of JSON, follows the data to handler and then encodes
%% its response to JSON and sends it back to the client.
%% @end
%%--------------------------------------------------------------------
-spec websocket_handle(InFrame, Req, State) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {reply, OutFrame | [OutFrame], Req, State} |
    {reply, OutFrame | [OutFrame], Req, State, hibernate} |
    {shutdown, Req, State} when
    InFrame :: {text | binary | ping | pong, binary()},
    Req :: cowboy_req:req(),
    State :: no_state,
    OutFrame :: cowboy_websocket:frame().
websocket_handle({text, MsgJSON}, Req, State) ->
    % Try to decode request
    DecodedMsg = try
        json_utils:decode(MsgJSON)
    catch
        _:_ -> undefined
    end,
    case DecodedMsg of
        % Accept only batch requests
        [{<<"batch">>, Requests}] ->
            % Batch was decoded, try to process all the requests.
            % Request processing is asynchronous.
            process_requests(Requests),
            {ok, Req, State};
        _ ->
            % Request could not be decoded, reply with an error
            {_, ErrorMsg} = gui_error:cannot_decode_message(),
            ResponseJSON = json_utils:encode([{<<"batch">>, ErrorMsg}]),
            {reply, {text, ResponseJSON}, Req, State}
    end;


websocket_handle(Data, Req, State) ->
    ?debug("Received unexpected data in GUI WS: ~p", [Data]),
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% Handles any Erlang messages received.
%% Async processes can message the websocket process
%% (push_deleted, push_updated) to push data to the client.
%% @end
%%--------------------------------------------------------------------
-spec websocket_info(Info, Req, State) ->
    {ok, Req, State} | {ok, Req, State, hibernate} |
    {reply, OutFrame | [OutFrame], Req, State} |
    {reply, OutFrame | [OutFrame], Req, State, hibernate} |
    {shutdown, Req, State} when
    Info :: any(),
    Req :: cowboy_req:req(),
    State :: no_state,
    OutFrame :: cowboy_websocket:frame().
% Sends any data to the client
websocket_info({send, Data}, Req, State) ->
    Msg = [
        {<<"batch">>, [Data]}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

% Sends a push message (server side event) to the client
websocket_info({push_message, Data}, Req, State) ->
    Msg = [
        {<<"batch">>, [
            [
                {?KEY_MSG_TYPE, ?TYPE_PUSH_MESSAGE},
                {?KEY_DATA, Data}
            ]
        ]}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

% Sends a push message informing about newly created item to the client
% Concerns only model level items
websocket_info({push_created, ResourceType, Data}, Req, State) ->
    Msg = [
        {<<"batch">>, [
            [
                {?KEY_MSG_TYPE, ?TYPE_MODEL_CRT_PUSH},
                {?KEY_RESOURCE_TYPE, ResourceType},
                {?KEY_DATA, Data}
            ]
        ]}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

% Sends a push message informing about updated item to the client
% Concerns only model level items
websocket_info({push_updated, ResourceType, Data}, Req, State) ->
    Msg = [
        {<<"batch">>, [
            [
                {?KEY_MSG_TYPE, ?TYPE_MODEL_UPT_PUSH},
                {?KEY_RESOURCE_TYPE, ResourceType},
                {?KEY_DATA, Data}
            ]
        ]}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

% Sends a push message informing about deleted item to the client
% Concerns only model level items
websocket_info({push_deleted, ResourceType, Ids}, Req, State) ->
    Msg = [
        {<<"batch">>, [
            [
                {?KEY_MSG_TYPE, ?TYPE_MODEL_DLT_PUSH},
                {?KEY_RESOURCE_TYPE, ResourceType},
                {?KEY_DATA, Ids}
            ]
        ]}
    ],
    {reply, {text, json_utils:encode(Msg)}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.


%%--------------------------------------------------------------------
%% @doc
%% Performs any necessary cleanup of the state.
%% @end
%%--------------------------------------------------------------------
-spec websocket_terminate(Reason, Req, State) -> ok when
    Reason :: {normal, shutdown | timeout} | {remote, closed} |
    {remote, cowboy_websocket:close_code(), binary()} |
    {error, badencoding | badframe | closed | atom()},
    Req :: cowboy_req:req(),
    State :: no_state.
websocket_terminate(_Reason, _Req, _State) ->
    gui_async:kill_async_processes(),
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @private
%% Handled a decoded websocket message. Based on the type of message and type
%% of requested resource, decides which handler module should be called.
%% @end
%%--------------------------------------------------------------------
handle_decoded_message(Props) ->
    MsgType = proplists:get_value(?KEY_MSG_TYPE, Props),
    MsgUUID = proplists:get_value(?KEY_UUID, Props, null),
    % Choose handling module depending on message type
    {Result, ReplyType} = case MsgType of
        ?TYPE_MODEL_REQ ->
            RsrcType = proplists:get_value(?KEY_RESOURCE_TYPE, Props),
            Handler = get_data_backend(RsrcType),
            Res = handle_model_req(Props, Handler),
            {Res, ?TYPE_MODEL_RESP};
        ?TYPE_RPC_REQ ->
            Res = handle_RPC_req(Props),
            {Res, ?TYPE_RPC_RESP}
    end,
    % Resolve returned values
    {RespResult, RespData} = case Result of
        ok ->
            {?RESULT_OK, null};
        {ok, Data} ->
            {?RESULT_OK, Data};
        {error_result, Data} ->
            {?RESULT_ERROR, Data}
    end,
    [
        {?KEY_MSG_TYPE, ReplyType},
        {?KEY_UUID, MsgUUID},
        {?KEY_RESULT, RespResult},
        {?KEY_DATA, RespData}
    ].


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Resolves data backend for given model synchronization request.
%% Data backends must be initialized on first call, so it uses a map to keep
%% track which backends are already initialized.
%% @end
%%--------------------------------------------------------------------
-spec get_data_backend(ResourceType :: binary()) -> Handler :: atom().
get_data_backend(ResourceType) ->
    % Initialized data backends are cached in process dictionary.
    case get({data_backend, ResourceType}) of
        undefined ->
            HasSession = g_session:is_logged_in(),
            Handler = ?GUI_ROUTE_PLUGIN:data_backend(HasSession, ResourceType),
            ok = Handler:init(),
            put({data_backend, ResourceType}, Handler),
            Handler;
        Handler ->
            Handler
    end.


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Handles message of type PULL REQUEST, which is a message requesting data
%% about certain model. Returns a proplist that is later encoded to JSON.
%% @end
%%--------------------------------------------------------------------
-spec handle_model_req(Props :: proplists:proplist(), Handler :: atom()) ->
    ok | {ok, Res :: proplists:proplist()} | gui_error:error_result().
handle_model_req(Props, Handler) ->
    RsrcType = proplists:get_value(?KEY_RESOURCE_TYPE, Props),
    Data = proplists:get_value(?KEY_DATA, Props),
    EntityIdOrIds = proplists:get_value(?KEY_RESOURCE_IDS, Props),
    % Catch crashes here so we can respond with an error corresponding
    % to given request rather that with generic error.
    try
        case proplists:get_value(?KEY_OPERATION, Props) of
            ?OP_FIND ->
                erlang:apply(Handler, find, [RsrcType, EntityIdOrIds]);
            ?OP_FIND_MANY ->
                % Will return list of found entities only if all finds succeed.
                Res = lists:foldl(
                    fun(EntityId, Acc) ->
                        case Acc of
                            List when is_list(List) ->
                                case erlang:apply(Handler, find,
                                    [RsrcType, EntityId]) of
                                    {ok, Data} ->
                                        [Data | Acc];
                                    Error ->
                                        Error
                                end;
                            Error ->
                                Error
                        end
                    end, [], EntityIdOrIds),
                {ok, Res};
            ?OP_FIND_ALL ->
                erlang:apply(Handler, find_all, [RsrcType]);
            ?OP_FIND_QUERY ->
                erlang:apply(Handler, find_query, [RsrcType, Data]);
            ?OP_CREATE_RECORD ->
                erlang:apply(Handler, create_record, [RsrcType, Data]);
            ?OP_UPDATE_RECORD ->
                erlang:apply(Handler, update_record,
                    [RsrcType, EntityIdOrIds, Data]);
            ?OP_DELETE_RECORD ->
                erlang:apply(Handler, delete_record,
                    [RsrcType, EntityIdOrIds])
        end
    catch
        T:M ->
            % There was an error processing the request, reply with
            % an error.
            ?error_stacktrace("Error while handling GUI model request - ~p:~p",
                [T, M]),
            gui_error:internal_server_error()
    end.


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Handles message of type RPC_REQUEST, which is a message requesting that
%% the server performs some operation.
%% Returns a proplist that is later encoded to JSON.
%% @end
%%--------------------------------------------------------------------
-spec handle_RPC_req(Props :: proplists:proplist()) ->
    Res :: ok | {ok, proplists:proplist()} | gui_error:error_result().
handle_RPC_req(Props) ->
    ResourceType = proplists:get_value(?KEY_RESOURCE_TYPE, Props),
    Operation = proplists:get_value(?KEY_OPERATION, Props),
    Data = proplists:get_value(?KEY_DATA, Props),
    % Catch crashes here so we can respond with an error corresponding
    % to given request rather that with generic error.
    try
        case ResourceType of
            ?RESOURCE_TYPE_SESSION ->
                handle_session_RPC();
            ?RESOURCE_TYPE_PUBLIC_RPC ->
                handle_public_RPC(Operation, Data);
            ?RESOURCE_TYPE_PRIVATE_RPC ->
                case g_session:is_logged_in() of
                    true ->
                        handle_private_RPC(Operation, Data);
                    false ->
                        gui_error:no_session()
                end
        end
    catch
        T:M ->
            % There was an error processing the request, reply with
            % an error.
            ?error_stacktrace("Error while handling GUI RPC request - ~p:~p",
                [T, M]),
            gui_error:internal_server_error()
    end.


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Handles an RPC call for clients with active session.
%% @end
%%--------------------------------------------------------------------
-spec handle_private_RPC(Operation :: binary(), Data :: proplists:proplist()) ->
    Res :: ok | {ok, proplists:proplist()} | gui_error:error_result().
handle_private_RPC(Operation, Data) ->
    Handler = ?GUI_ROUTE_PLUGIN:private_rpc_backend(),
    Handler:handle(Operation, Data).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Handles an RPC call for clients with any session (both clients with and
%% without active session are allowed to use these RPC calls).
%% @end
%%--------------------------------------------------------------------
-spec handle_public_RPC(Operation :: binary(), Data :: proplists:proplist()) ->
    Res :: ok | {ok, proplists:proplist()} | gui_error:error_result().
handle_public_RPC(Operation, Data) ->
    Handler = ?GUI_ROUTE_PLUGIN:public_rpc_backend(),
    Handler:handle(Operation, Data).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Handles an RPC call session details.
%% @end
%%--------------------------------------------------------------------
-spec handle_session_RPC() -> {ok, proplists:proplist()}.
handle_session_RPC() ->
    Data = case g_session:is_logged_in() of
        true ->
            {ok, Props} = ?GUI_ROUTE_PLUGIN:session_details(),
            [
                {?KEY_SESSION_VALID, true},
                {?KEY_SESSION_DETAILS, Props}
            ];
        false ->
            [
                {?KEY_SESSION_VALID, false}
            ]
    end,
    {ok, Data}.


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Processes a batch of requests. A pool of processes is spawned and
%% the requests are split between them.
%% The requests are processed asynchronously and responses are sent gradually
%% to the client.
%% @end
%%--------------------------------------------------------------------
-spec process_requests(Requests :: [proplists:proplist()]) -> ok.
process_requests(Requests) ->
    {ok, ProcessLimit} =
        application:get_env(gui, gui_max_async_processes_per_batch),
    Parts = split_into_sublists(Requests, ProcessLimit),
    lists:foreach(
        fun(Part) ->
            gui_async:spawn(true, fun() -> process_requests_async(Part) end)
        end, Parts).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Processes a batch of request. Responses are sent gradually to
%% the websocket process, which sends them to the client.
%% This should be done in an async process for scalability.
%% @end
%%--------------------------------------------------------------------
-spec process_requests_async(Requests :: [proplists:proplist()]) -> ok.
process_requests_async(Requests) ->
    lists:foreach(
        fun(Request) ->
            Result = try
                handle_decoded_message(Request)
            catch
                T:M ->
                    % There was an error processing the request, reply with
                    % an error.
                    ?error_stacktrace("Error while handling websocket message "
                    "- ~p:~p", [T, M]),
                    {_, ErrorMsg} = gui_error:internal_server_error(),
                    ErrorMsg
            end,
            gui_async:send(Result)
        end, Requests).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Splits given list into a list of sublists with even length (+/- 1 element).
%% If the list length is smaller than the number of parts, it splits it into a
%% list of one element lists and the result list might be smaller than NumParts.
%% @end
%%--------------------------------------------------------------------
-spec split_into_sublists(List :: list(), NumberOfParts :: non_neg_integer()) ->
    [list()].
split_into_sublists(List, NumberOfParts) when length(List) =< NumberOfParts ->
    lists:map(fun(Element) -> [Element] end, List);
split_into_sublists(List, NumberOfParts) ->
    split_into_sublists(List, NumberOfParts, []).


%%--------------------------------------------------------------------
%% @doc
%% @private
%% Splits given list into a list of sublists with even length (+/- 1 element).
%% @end
%%--------------------------------------------------------------------
-spec split_into_sublists(List :: list(), NumberOfParts :: non_neg_integer(),
    ResultList :: [list()]) -> [list()].
split_into_sublists(List, 1, ResultList) ->
    [List | ResultList];

split_into_sublists(List, NumberOfParts, ResultList) ->
    {Part, Tail} = lists:split(length(List) div NumberOfParts, List),
    split_into_sublists(Tail, NumberOfParts - 1, [Part | ResultList]).
