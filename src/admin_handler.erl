-module(admin_handler).

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

handle(<<"GET">>, [], Req, State) ->
  Page = get_html(),
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"text/html">>}],
                 Page, Req),
  {ok, Req2, State};

handle(<<"GET">>, [<<"team">>, Id], Req, State) ->
  Team = d2api:teams(binary_to_list(Id), "1"),
  return_as_json(Team, Req, State);

handle(<<"GET">>, [<<"match">>, Id], Req, State) ->
  Match = d2api:match(binary_to_list(Id)),
  return_as_json(Match, Req, State);

handle(<<"GET">>, [<<"leagues">>], Req, State) ->
  Leagues = d2api:leagues(),
  return_as_json(Leagues, Req, State);

handle(<<"GET">>, [<<"live">>], Req, State) ->
  Live = d2api:live_games(),
  return_as_json(Live, Req, State);

handle(<<"GET">>, [<<"scheduled">>, From, To], Req, State) ->
  Scheduled = d2api:scheduled_games(binary_to_list(From),
                                    binary_to_list(To)),
  return_as_json(Scheduled, Req, State);

handle(<<"GET">>, _, Req, State) ->
  {ok, Req2} = cowboy_req:reply(
                 404,
                 [{<<"content-type">>, <<"text/html">>}],
                 <<"<h1>Not Found</h1>">>, Req),
  {ok, Req2, State};

handle(<<"POST">>, _, Req, State) ->
  {ok, Req2} = cowboy_req:reply(
                 404,
                 [{<<"content-type">>, <<"text/html">>}],
                 <<"<h1>Not Found</h1>">>, Req),
  {ok, Req2, State}.

get_html() ->
  {ok, Cwd} = file:get_cwd(),
  Filename = filename:join([Cwd, "priv", "admin.html"]),
  {ok, Binary} = file:read_file(Filename),
  Binary.

binary_to_integer(B) ->
  list_to_integer(binary_to_list(B)).

return_as_json({ok, Ret}, Req, State) ->
  return_json(Ret, Req, State);

return_as_json({error, Ret}, Req, State) ->
  return_as_json({<<"error">>, Ret}, Req, State);

return_as_json(Ret, Req, State) when is_list(Ret) ->
  return_json(mochijson2:encode(Ret), Req, State);

return_as_json(Ret, Req, State) when is_tuple(Ret) ->
  return_json(mochijson2:encode({struct, [Ret]}), Req, State);

return_as_json(Ret, Req, State) ->
  return_json(mochijson2:encode(Ret), Req, State).

return_json(Json, Req, State) ->
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json, Req),
  {ok, Req2, State}.
