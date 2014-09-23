%% ===================================================================
%% @author uyounri
%% @doc This module is the cowboy callback module used to get application
%% status information. Currently it merely responds with the port used
%% by Cowboy to accept requests. This indicates that it is healthy and
%% the simple Cowboy port config information. It could be extended to
%% expose additional status information if desired.  This might involve
%% defining additional Cowboy routes in ez_app.erl and the various
%% content_types_provided, perhaps with some bound route data as 
%% demonstrated in ez_app.erl for the ez_weather_handler.erl callback
%% module. 
%%
%% ===================================================================

-module(ez_status_handler).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/3, 
        content_types_provided/2, 
        status_to_html/2, 
        status_to_json/2, 
        status_to_text/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, status_to_html},
        {<<"application/json">>, status_to_json},
        {<<"text/plain">>, status_to_text}
    ], Req, State}.

status_to_html(Req, State) ->
    Beginning = <<"<html>
<head>
   <meta charset=\"utf-8\">
   <title>EZ Status</title>
</head>
<body>
    <p><em><center>EZ Status</center></em></p>
    <p>Cowboy Port: ">>,

    MidList = tuple_to_list(get_status()),
    [_ | PortList] = MidList,
    [PortInt | _] = PortList,
    PortBin = integer_to_binary(PortInt),
    
    End = <<"</p>
</body>
</html>">>,

    Body = <<Beginning/binary, PortBin/binary, End/binary>>,
    {Body, Req, State}.

status_to_json(Req, State) ->
    Beginning = <<"{\"Cowboy Port\": ">>,
    MidList = tuple_to_list(get_status()),
    [_ | PortList] = MidList,
    [PortInt | _] = PortList,
    PortBin = integer_to_binary(PortInt),
    End = <<"\}">>,
    Body = <<Beginning/binary, PortBin/binary, End/binary>>,
    {Body, Req, State}.

status_to_text(Req, State) ->
    Beginning = <<"Cowboy Port: ">>,
    MidList = tuple_to_list(get_status()),
    [_ | PortList] = MidList,
    [PortInt | _] = PortList,
    PortBin = integer_to_binary(PortInt),
    Body = <<Beginning/binary, PortBin/binary>>,
    {Body, Req, State}.

%% ====================================================================
%% Internal Functions
%% ====================================================================
get_status() ->
    Port = application:get_env(ez,http_port),
    lager:debug("Cowboy listener port: ~p", [Port]),
    Port.