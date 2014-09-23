%% ===================================================================
%% @author uyounri
%% @doc This is the main application module for the ez application. 
%%
%% "ez" supports getting and setting weather data for a city. It is
%% meant to be a simple means of exploring several different Erlang
%% technologies/capabilities.
%%
%% "ez" is an application that demonstrates the usage of:
%%    1. Rebar - see the rebar.config file in the project root directory
%%               for details.
%%    2. Starting multiple, dependent, applications. For example, this
%%       project depends on lager and zookeeper.  See the start.sh file for
%%       more details, particularly the application:ensure_all_started()
%%       directive.  This starts all the applications listed in the 
%%       application resource file's, src/ez.app.src, {applications, [...]}
%%       tuple for the complete set of applications.
%%    3. Lager as a logging replacement for the standard Erlang logger.
%%       See the erl-zookeeper.config file for Lager configuration settings.
%%    4. Cowboy to enable support for a ReSTful interface to the application.
%%       In this application the interface is used to get and set weather
%%       data.  
%%    5. Zookeeper is used as the data store for weather data. 
%%
%% ===================================================================


-module(ez_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1, start_phase/3]).

-define(APP, ez).

%% ===================================================================
%% Admin API
%% ===================================================================
start() ->
    %% Starts all dependent applications listed in {applications, [...]} 
    %% tuple in ez.app.src.
    application:ensure_all_started(?APP).

stop() -> application:stop(?APP).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    setup_cowboy(),
    ez_sup:start_link().

stop(_State) ->
    ok.

start_phase(init, _StartType, _StartArgs) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup_cowboy() ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    C_ACCEPTORS = 5,
    case cowboy:start_http(http, C_ACCEPTORS, TransOpts, ProtoOpts) of
        {ok, _} ->
            lager:info("Starting web service");
        Error ->
            lager:error("Unexpected result starting web service, ~p", [Error])
    end.

%%
%% @doc routes() defines the set of ReST resources (aka routes) exposed by the application.  There
%% are currently 2 exposed resources (or resource types):
%% 1. /status - reports current status info. As indicated below, see ez_status_handler.erl for details.
%% 2. /weather/:resource/[:city] - sets city specific weather information; gets city specific weather
%% information, or returns URLs for all cities with recorded weather information. :resource is a 
%% Cowboy binding that can be used to dereference the actual value stored at this place in the URL
%% (e.g., cities or city). [:city] is a Cowboy binding for the actual city (e.g., Denver) for setting
%% or retrieval.  The brackets indicate this binding is optional (i.e., it won't exist if :resource)
%% is "cities").
%%
routes() ->
    %% The following defines the "handler" modules to be used by Cowboy for each of the routes
    %% expected by this app.  The '_' pattern matches any host. Routes can be specified by the
    %% host the request is directed to. The following 2 lists specify the route segment following
    %% the hostname. "/status" is the first route (e.g., http://somehost/status) and has an 
    %% associated handler module of ez_status_handler.  The "/weather" route is defined in a similar
    %% manner.  It has some extra elements which are explained below.
    [
     {'_', [
            {"/status", ez_status_handler, []},
            %% ":resource" is the binding used to retrieve this path segment needed to distinguish 
            %% between the "cities" and "city" resources
            %% ":city" is an optional path element (designated by the surrounding brackets). It is
            %% used to retrieve the name of the city when ":resource" is "city".
            {"/weather/:resource/[:city]", ez_weather_handler, []} 
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            lager:debug("application:get_env(http_port): ~p", [Port]),
            Port;
        Other ->
            list_to_integer(Other)
    end.


