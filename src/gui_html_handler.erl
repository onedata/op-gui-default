%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This module handles requests form .html files, injecting some GUI logic.
%%% @end
%%%-------------------------------------------------------------------
-module(gui_html_handler).
-author("Lukasz Opiola").

-include("gui.hrl").
-include_lib("ctool/include/logging.hrl").

-export([is_html_req/1, maybe_handle_html_req/1]).

-type page_init_result() ::
%% Will serve the HTML file defined in ?GUI_ROUTE_PLUGIN
serve_html |
%% Same as above, adding given headers to default ones
{serve_html, Headers :: http_client:headers()} |
%% Will serve explicit body
{serve_body, Body :: binary()} |
%% Same as above, with given headers
{serve_body, Body :: binary(), Headers :: http_client:headers()} |
%% Will display the 404 page specified in ?GUI_ROUTE_PLUGIN
display_404_page |
%% Will display the 500 page specified in ?GUI_ROUTE_PLUGIN
display_500_page |
%% Will reply with given code
{reply, Code :: integer()} |
%% Will reply with given code and headers
{reply, Code :: integer(), Headers :: http_client:headers()} |
%% Will reply with given code, body and headers
{reply, Code :: integer(), Headers :: http_client:headers(), Body :: binary()} |
%% Will send a 307 redirect back to the client,
%% given URL must be relative to current domain, e.g. /images/image.png
{redirect_relative, URL :: binary()} |
%% Will send a 307 redirect back to the client,
%% given URL must be full, e.g. https://google.com/images/image.png
{redirect_absolute, URL :: binary()} |
%% Will display "Internal Server Error" page.
error.

-export_type([page_init_result/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Indicates if the requests is a .html requests based on requested path.
%% @end
%%--------------------------------------------------------------------
-spec is_html_req(binary()) -> boolean().
is_html_req(<<"/">>) ->
    true;

is_html_req(<<?WEBSOCKET_PREFIX_PATH>>) ->
    true;

is_html_req(<<?WEBSOCKET_PREFIX_PATH, Path/binary>>) ->
    is_html_req(Path);

is_html_req(<<"/", Path/binary>>) ->
    is_html_req(Path);

is_html_req(Path) ->
    case binary:split(Path, <<"/">>) =:= [Path] of
        false ->
            false;
        true ->
            case binary:split(Path, <<".">>) of
                [_, <<"html">>] ->
                    true;
                % Accept also pages with no extension
                [Path] ->
                    true;
                _ ->
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Indicates if the requests is a .html requets based on requested path.
%% Returns continue or finish atom indicating if the html file should be
%% served or not.
%% @end
%%--------------------------------------------------------------------
-spec maybe_handle_html_req(Req :: cowboy_req:req()) ->
    {continue | finish, NewReq :: cowboy_req:req()}.
maybe_handle_html_req(Req) ->
    {FullPath, _} = cowboy_req:path(Req),
    case is_html_req(FullPath) of
        true ->
            % Initialize context, run page's init code, reply, redirect or just
            % let cowboy static handler serve the html.
            % Catch all errors here.
            try
                handle_html_req(Req)
            catch T:M ->
                % If an error occured, display error 505 page (will be served
                % by cowboy_static).
                ?error_stacktrace("Error while handling a HTTP request - ~p:~p",
                    [T, M]),
                Page505File = ?GUI_ROUTE_PLUGIN:error_500_html_file(),
                Req2 = cowboy_req:set([{path, <<"/", Page505File/binary>>}], Req),
                Req3 = cowboy_req:set([{path_info, [Page505File]}], Req2),
                {continue, Req3}
            end;
        false ->
            % Just let the cowboy static handler serve a static file
            {continue, Req}
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handles a .html file request. Decides what reply should be given to the
%% client or lets cowboy serve the html file.
%% @end
%%--------------------------------------------------------------------
-spec handle_html_req(Req :: cowboy_req:req()) ->
    {continue | finish, NewReq :: cowboy_req:req()}.
handle_html_req(Req) ->
    % Initialize request context
    gui_ctx:init(Req, true),
    % Check if the user is permitted to see the page
    % If so, call page_init/0 callback
    % If not, redirect to another page
    ReqrsSess = gui_ctx:session_requirements(),
    LoggedIn = gui_session:is_logged_in(),
    PageInitResult =
        case {ReqrsSess, LoggedIn} of
            {?SESSION_ANY, _} ->
                page_init_callback();
            {?SESSION_LOGGED_IN, true} ->
                page_init_callback();
            {?SESSION_LOGGED_IN, false} ->
                {redirect_relative, ?GUI_ROUTE_PLUGIN:login_page_path()};
            {?SESSION_NOT_LOGGED_IN, false} ->
                page_init_callback();
            {?SESSION_NOT_LOGGED_IN, true} ->
                {redirect_relative, ?GUI_ROUTE_PLUGIN:default_page_path()}
        end,
    % Coalesce possible results from page_init
    CoalescedResult =
        case PageInitResult of
            serve_html ->
                {serve_html, []};
            {serve_body, Bd} ->
                {reply, 200, [{<<"content-type">>, <<"text/plain">>}], Bd};
            {serve_body, Bd, Hdrs} ->
                {reply, 200, Hdrs, Bd};
            display_404_page ->
                gui_ctx:set_html_file(?GUI_ROUTE_PLUGIN:error_404_html_file()),
                {serve_html, []};
            display_500_page ->
                gui_ctx:set_html_file(?GUI_ROUTE_PLUGIN:error_500_html_file()),
                {serve_html, []};
            {redirect_relative, URL} ->
                % @todo https should be detected automatically, not hardcoded
                FullURL = <<"https://", (gui_ctx:get_requested_hostname())/binary,
                URL/binary>>,
                {reply, 307, [{<<"location">>, FullURL}], <<"">>};
            {redirect_absolute, AbsURL} ->
                {reply, 307, [{<<"location">>, AbsURL}], <<"">>};
            {reply, Code_} ->
                {reply, Code_, [], <<"">>};
            {reply, Code_, Hdrs} ->
                {reply, Code_, Hdrs, <<"">>};
            Other ->
                Other
        end,
    % Process the page_init results.
    Result =
        case CoalescedResult of
            {serve_html, Headers} ->
                case gui_ctx:get_html_file() of
                    undefined ->
                        ?error("HTML file for page ~p is not defined.",
                            [gui_ctx:get_path()]),
                        Page500Path = ?GUI_ROUTE_PLUGIN:error_500_html_file(),
                        HtmlFileToServe = <<"/", (Page500Path)/binary>>,
                        gui_ctx:set_path(HtmlFileToServe),
                        continue;
                    Path ->
                        HtmlFileToServe = <<"/", (Path)/binary>>,
                        gui_ctx:set_path(HtmlFileToServe),
                        gui_ctx:set_resp_headers(Headers),
                        continue
                end;
            {reply, Code, Headers, Body} ->
                gui_ctx:reply(Code, Headers, Body),
                finish
        end,
    NewReq = gui_ctx:finish(),
    {Result, NewReq}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to call page_backend for page init callback. If the page_backend is not
%% defined, just serves the html.
%% @end
%%--------------------------------------------------------------------
-spec page_init_callback() -> page_init_result().
page_init_callback() ->
    case gui_ctx:get_page_backend() of
        undefined ->
            % No backend specified - just serve the HTML.
            {serve_html, []};
        Module ->
            % Backend is specified - run the page_init fun. Crash if it
            % is undefined.
            Module:page_init()
    end.