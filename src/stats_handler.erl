-module(stats_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Params) ->
  {ok, Req, Params}.

handle(Req, State) ->
  {Method, Req2} = cowboy_req:method(Req),
  {Path, Req3} = cowboy_req:path_info(Req2),
  handle(Method, Path, Req3, State).

terminate(_Reason, _Req, _State) ->
  ok.

handle(<<"GET">>, [<<"today">>], Req, State) ->
  S = d2api_stats:date_stats(date()),
  JSon = mochijson2:encode(S),
  return_json(JSon, Req, State);

handle(<<"GET">>, [From, To], Req, State) ->
  S = d2api_stats:date_stats(binary_to_date(From),
                             binary_to_date(To)),
  JSon = mochijson2:encode(S),
  return_json(JSon, Req, State).

return_json(Json, Req, State) ->
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json, Req),
  {ok, Req2, State}.

binary_to_date(<<Y,"-",M,"-",D>>) ->
  {Y,M,D}.
