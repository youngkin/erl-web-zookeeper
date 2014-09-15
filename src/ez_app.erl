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
    start_deps().

%%%===================================================================
%%% Internal functions
%%%===================================================================

dep_start(App, Type) ->
    start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
    ok = dep_start(Dep, Type),
    dep_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

start_deps() ->
    application:load(?APP),
    case application:get_key(?APP, deps) of
        {ok, Deps} ->
            [dep_start(App, permanent) || App <- Deps];
        undefined -> ok
    end,
    ok.

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
            {"/status", ez_status, []}
           ]}
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.


