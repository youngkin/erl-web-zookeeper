-module(ez_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1, start_phase/3]).

-define(APP, ez).

%% =========================================
%% Admin API
%% =========================================
%% @doc Starts the application
start() ->
  application:ensure_all_started(?APP).

%% @doc Stops the application
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


