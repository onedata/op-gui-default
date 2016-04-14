%%%-------------------------------------------------------------------
%%% @author Lukasz Opiola
%%% @copyright (C): 2015 ACK CYFRONET AGH
%%% This software is released under the MIT license
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% This behaviour defines a page_backend module which will be called before
%%% a html file is served, if corresponding gui_route specifies so.
%%% @end
%%%-------------------------------------------------------------------
-module(page_backend_behaviour).
-author("Lukasz Opiola").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Called before the page (HTML) is served. Used to decide how to
%% respond to page query (see returned values).
%% @end
%%--------------------------------------------------------------------
-callback page_init() -> gui_html_handler:page_init_result().

