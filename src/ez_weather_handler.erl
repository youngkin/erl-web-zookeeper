%% @author uyounri
%% @doc @todo Add description to ez_weather_handler.


-module(ez_weather_handler).

%% ====================================================================
%% API functions
%% ====================================================================

%% Cowboy callbacks
-export(
  [ init/3
  , allowed_methods/2
  , content_types_provided/2
  , content_types_accepted/2
  , terminate/3
  ]).

%% Callbacks for GET/POST processing, will be used by Cowboy
-export(
  [weather_to_html/2, 
   weather_to_json/2, 
   weather_to_text/2,
   post_weather_update/2]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, weather_to_html},
        {<<"application/json">>, weather_to_json},
        {<<"text/plain">>, weather_to_text}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, post_weather_update}], Req, State}.


weather_to_html(Req, State) ->
        Body = <<"<html>
<head>
    <meta charset=\"utf-8\">
    <title>EZ Weather Service</title>
</head>
<body>
    <p>EZ Weather Service - Under Construction</p>
</body>
</html>">>,
    {Body, Req, State}.


weather_to_json(Req, State) ->
    Body = <<"{\"weather\": \"Under Construction\"}">>,
    {Body, Req, State}.

weather_to_text(Req, State) ->
    Body = <<"EZ Weather Service is under construction">>,
    {Body, Req, State}.

post_weather_update(Req, State) ->
    lager:debug("Enter"),
    {ok, Body, _Req1} = cowboy_req:body(Req),
    lager:debug("Body contents: ~p", [Body]),
    %% TODO: Just echoing the input for now, need to do something more sophisticated...
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {true, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


