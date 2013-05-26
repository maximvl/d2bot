-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([dispatch_json/2]).

-record(state, {user}).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, #state{user=false}}.

websocket_handle({text, Json}, Req, State) ->
  spawn(?MODULE, dispatch_json, [Json, State]),
  %% {reply, {text, << Msg/binary >>}, Req2, State};
  {ok, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({msg, From, Msg}, Req, State) ->
  Json = mochijson2:encode([{"action", "reply"},
                            {"from", From},
                            {"message", Msg}]),
  {reply, {text, Json}, Req, State};

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
  users_sup:remove_user(State),
  ok.

dispatch_json(Json, State) ->
  {struct, Props} = mochijson2:decode(Json),
  Action = proplists:get_value(<<"action">>, Props),
  dispatch(Action, Props, State).

dispatch(<<"make_bet">>, Props, State) ->
  MatchID = proplists:get_value(<<"match-id">>, Props),
  Team = proplists:get_value(<<"team">>, Props);
  %% TODO team belongs to match-id validation
  
dispatch(<<"send_msg">>, Props, State) ->
  Msg = proplists:get_value(<<"message">>, Props),
  gen_server:cast(State#state.user, {send_msg, Msg});

dispatch(<<"set_nick">>, Props, State) ->
  Nick = proplists:get_value(<<"nick">>, Props),
  gen_server:cast(State#state.user, {set_nick, Nick});

dispatch(_, _Props, _State) ->
  ok.
