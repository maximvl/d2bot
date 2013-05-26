-module(bets_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Params) ->
  {ok, Req, Params}.

handle(Req, State) ->
  %% {Path, Req2} = cowboy:path_info(Req),
  %% {ok, Req3} = cowboy_req:reply(
  %%                200,
  %%                [{<<"content-type">>, <<"text/html">>}],
  %%                Html, Req2),
  {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.
