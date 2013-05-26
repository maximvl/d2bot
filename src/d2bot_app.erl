-module(d2bot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  d2bot_sup:start_link(),

  {ok, MnesiaDir} = application:get_env(mnesia_dir),
  application:set_env(mnesia, dir, MnesiaDir),
  matches_db:setup([node()]),

  {ok, Host} = application:get_env(listen_ip),
  {ok, Port} = application:get_env(listen_port),

  WsAddr = case os:getenv("OPENSHIFT_APP_DNS") of
             false ->
               list_to_binary("ws://" ++ Host ++ ":" ++ Port ++ "/websock");
             DNS ->
               list_to_binary("ws://" ++ DNS ++ ":8000" ++ "/websock")
           end,

  io:format("websocket addr: ~s~n", [WsAddr]),

  Dispatch = cowboy_router:compile(
               [
                %% {URIHost, list({URIPath, Handler, Opts})}
                {'_', [
                       {"/",            root_handler, []},
                       {"/ws_req",      root_handler, ["ws_req", WsAddr]},
                       {"/bets/[...]",  bets_handler, []},
                       {"/admin/[...]", admin_handler, []},
                       {"/ws",          ws_handler,   []},
                       {"/static/[...]",cowboy_static, 
                        [{directory, {priv_dir, d2bot, [<<"static">>]}},
                         {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                        ]}
                      ]}
               ]),
  
  IntPort = erlang:list_to_integer(Port),
  {ok, IP} = inet_parse:address(Host),
  
  io:format("listening on: ~w:~w~n", [IP, IntPort]),
  
  cowboy:start_http(
    http,
    100,
    [{port, IntPort}, {ip, IP}],
    [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
    ok.
