%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C) 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour specifies an API for gui routing logic - a module that
%%% decides on:
%%%   - mapping of URLs to pages (routes)
%%%   - logic and requirements on different routes
%%%   - what pages are used for login, logout, displaying errors
%%%   - what modules handles server logic of WebSocket connection with
%%%         the client (data and callback backends)
%%%
%%% The implementing module must be called ?GUI_ROUTE_PLUGIN.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_route_plugin_behaviour).
-author("Lukasz Opiola").

-include("gui.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Should return a gui_route record per every page that a user can visit.
%% If the Path is not valid, error_404_html_file/0 function will be used
%% to retrieve .html file to serve that will display the error.
%% @end
%%--------------------------------------------------------------------
-callback route(Path :: binary()) -> #gui_route{}.


%%--------------------------------------------------------------------
%% @doc
%% Should return a module that implements data_backend_behaviour and
%% will be called for models synchronization over websocket.
%% The returned module should depend on the fact if client has an
%% active session.
%% @end
%%--------------------------------------------------------------------
-callback data_backend(HasSession :: boolean(), Identifier :: binary()) ->
    HandlerModule :: module().


%%--------------------------------------------------------------------
%% @doc
%% Should return a module that implements rpc_backend_behaviour and
%% will be called to handle RPC from the GUI that do not regard models.
%% Access to this API is restricted for logged in clients.
%% @end
%%--------------------------------------------------------------------
-callback private_rpc_backend() -> HandlerModule :: module().


%%--------------------------------------------------------------------
%% @doc
%% Should return a module that implements rpc_backend_behaviour and
%% will be called to handle RPC from the GUI that do not regard models.
%% Access to this API is allowed for all clients.
%% @end
%%--------------------------------------------------------------------
-callback public_rpc_backend() -> HandlerModule :: module().


%%--------------------------------------------------------------------
%% @doc
%% Should return a proplist (that will be later converted to JSON) containing
%% session information that will be sent to client when it has logged in,
%% for example user name, user email etc.
%% This will be called only with valid gui context and for logged in users.
%% @end
%%--------------------------------------------------------------------
-callback session_details() ->
    {ok, proplists:proplist()} | gui_error:error_result().


%%--------------------------------------------------------------------
%% @doc
%% Should return login page where the user will be redirected if he requests
%% a page that can only be visited when logged in.
%% @end
%%--------------------------------------------------------------------
-callback login_page_path() -> Path :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should return a default page where the user will be redirected if
%% he requests a page that he cannot currently visit (for example login page
%% when the user is already logged in).
%% @end
%%--------------------------------------------------------------------
-callback default_page_path() -> Path :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should return a file name of the HTML file that displays error 404 page.
%% @end
%%--------------------------------------------------------------------
-callback error_404_html_file() -> FileName :: binary().


%%--------------------------------------------------------------------
%% @doc
%% Should return a file name of the HTML file that displays error 500 page.
%% @end
%%--------------------------------------------------------------------
-callback error_500_html_file() -> FileName :: binary().

