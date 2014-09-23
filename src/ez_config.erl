%% ===================================================================
%% @author youngkin
%% @doc This module simply creates the initial set of Zookeeper znodes
%% needed by the application, namely "/ez/weather/cities". The actual
%% weather data will be stored under this znode structure as
%% .../cityname/weathertype such as .../denver/clear.
%%
%% Once started and initialized, this gen_server does nothing. 
%%
%% ===================================================================

-module(ez_config).

-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    start_link/0,
    terminate/2
]).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

start_link() ->
    lager:debug("enter", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    lager:debug("Creating required znodes in zookeeper"),
    ez_data:create("/ez", <<"ez application directory">>),
    ez_data:create("/ez/weather", <<"ez app weather directory">>),
    ez_data:create("/ez/weather/cities", <<"ez app cities directory">>),
    {ok, []}.

handle_call(Request, _From, State) ->
   lager:warning("Unknown call: ~p", [Request]),
   {reply, ok, State}.

handle_cast(Request, State) ->
    lager:warning("Unknown cast: ~p", [Request]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unknown info message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

