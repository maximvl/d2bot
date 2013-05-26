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
  Team = matches_db:get_team(Id),
  Props = matches_db:to_proplist(Team),
  Json = mochijson2:encode({struct, Props}),
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json, Req),
  {ok, Req2, State};

handle(<<"GET">>, [<<"teams">>], Req, State) ->
  {From, Req2} = cowboy_req:qs_val(<<"start">>, Req, <<"0">>),
  {Count, Req3} = cowboy_req:qs_val(<<"count">>, Req2, <<"10">>),
  Teams = case binary_to_integer(From) of
            0 ->
              matches_db:get_teams(binary_to_integer(Count));
            X ->
              matches_db:get_teams(X, binary_to_integer(Count))
          end,
  PropsList = [matches_db:to_proplist(Team) || Team <- Teams],
  Json = mochijson2:encode({struct, [{<<"teams">>, PropsList}]}),
  {ok, Req4} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json, Req3),
  {ok, Req4, State};

handle(<<"POST">>, [<<"team">>, <<"add">>], Req, State) ->
  {ok, Body, Req2} = cowboy_req:body_qs(Req),
  Name = proplists:get_value(<<"name">>, Body),
  Resp = matches_db:add_team(Name),
  Json = mochijson2:encode({struct, [{<<"result">>, Resp}]}),
  {ok, Req3} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json, Req2),
  {ok, Req3, State}.

get_html() ->
  {ok, Cwd} = file:get_cwd(),
  Filename = filename:join([Cwd, "priv", "admin.html"]),
  {ok, Binary} = file:read_file(Filename),
  Binary.

binary_to_integer(B) ->
  list_to_integer(binary_to_list(B)).
