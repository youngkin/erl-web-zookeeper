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
    C_ACCEPTORS = 100,
    case cowboy:start_http(http, C_ACCEPTORS, TransOpts, ProtoOpts) of
        {ok, _} ->
            lager:info("Starting web service");
        Error ->
            lager:error("Unexpected result starting web service, ~p", [Error])
    end.

routes() ->
    [
     {'_', [
            {"/status", ez_status_handler, []},
            {"/weather", ez_weather_handler, []}
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


