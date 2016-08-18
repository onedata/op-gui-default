%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc This is the main moule for gui library application.
%%% gui:init/0 and gui:cleanup/0 should be called from including
%%% application to set up eveything that is needed, such as session plugin.
%%% @end
%%%-------------------------------------------------------------------
-module(gui).
-author("Lukasz Opiola").

-include("gui.hrl").

%% API
-export([init/0, cleanup/0, response_headers/1]).


%%--------------------------------------------------------------------
%% @doc
%% Should be called from the application including gui to set up gui modules.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    ?GUI_SESSION_PLUGIN:init().


%%--------------------------------------------------------------------
%% @doc
%% Should be called from the application including gui to clean up gui modules.
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> ok.
cleanup() ->
    ?GUI_SESSION_PLUGIN:cleanup().


%%--------------------------------------------------------------------
%% @doc Callback hook for cowboy to modify response headers for HTTPS GUI.
%% Those headers improve security of https connection.
%% Headers defined in gui_route_plugin:response_headers/0 will be added.
%% @end
%%--------------------------------------------------------------------
-spec response_headers(Req :: cowboy_req:req()) -> cowboy_req:req().
response_headers(Req) ->
    lists:foldl(
        fun({Key, Value}, AccReq) ->
            cowboy_req:set_resp_header(Key, Value, AccReq)
        end, Req, ?GUI_ROUTE_PLUGIN:response_headers()).


