-module(d2bot).

-export([start/0]).

start() ->
  reloader:start(),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(inets),
  ok = application:start(d2api),
  ok = application:start(d2bot).
