-module(root_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Params) ->
  {ok, Req, Params}.

handle(Req, ["ws_req", WsAddr] = State) ->
  
  Json = mochijson2:encode(
           {struct, [{"websocket", WsAddr}]}),
  
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json, Req),
  {ok, Req2, State};

handle(Req, State) ->
  Html = get_html(),
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"text/html">>}],
                 Html, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

get_html() ->
  {ok, Cwd} = file:get_cwd(),
  Filename = filename:join([Cwd, "priv", "root.html"]),
  {ok, Binary} = file:read_file(Filename),
  Binary.
