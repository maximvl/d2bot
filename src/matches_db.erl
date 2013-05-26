-module(matches_db).
-export([setup/1, to_proplist/1]).

%% Team api
-export([add_team/1, set_team_name/2, remove_team/1,
         get_team/1, get_teams/1, get_teams/2,
         teams_count/0]).

-record(team, {id :: integer(),
               name :: binary()
              }).

-record(match, {id :: integer(),
                teamA :: #team{},
                teamB :: #team{},
                time :: calendar:datetime(),
                bestof :: integer()
               }).

setup(Nodes) ->
  ok = mnesia:create_schema(Nodes),
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(team,
                      [{attributes, record_info(fields, team)},
                       {disc_copies, Nodes}]),
  mnesia:create_table(mafiapp_match,
                      [{attributes, record_info(fields, match)},
                       {disc_copies, Nodes}]).
  

-spec add_team(binary()) -> ok.
add_team(Name) ->
  F = fun() ->
          mnesia:write(#team{
                          id = gen_id(Name),
                          name = Name})
      end,
  mnesia:activity(transaction, F).

-spec set_team_name(integer(), binary()) -> ok | {error, binary()}.
set_team_name(Id, NewName) ->
  F = fun() ->
          case mnesia:read(team, Id) of
            [] ->
              {error, <<"no such item">>};
            Team ->
              mnesia:write(Team#team{name = NewName})
          end
      end,
  mnesia:activity(transaction, F).

-spec remove_team(integer()) -> ok.
remove_team(Id) ->
  F = fun() ->
          mnesia:delete({team, Id})
      end,
  mnesia:activity(transaction, F).

-spec get_team(integer() | binary()) -> [#team{}] | {aborted, term()}.
get_team(Id) when is_integer(Id) ->
  F = fun() ->
          mnesia:read(team, Id)
      end,
  mnesia:activity(transaction, F);

get_team(Name) ->
  F = fun() ->
          mnesia:index_read(team, Name, #team.name)
      end,
  mnesia:activity(transaction, F).

-spec get_teams(integer()) -> [#team{}].
get_teams(N) ->
  F = fun() ->
          Keys = get_keys(mnesia:first(team), N, team),
          [mnesia:read(team, Key) || Key <- Keys]
      end,
  mnesia:activity(transaction, F).

-spec get_teams(integer(), integer()) -> [#team{}].
get_teams(StartID, N) ->
  F = fun() ->
          Keys = get_keys(StartID, N, team),
          [mnesia:read(team, Key) || Key <- Keys]
      end,
  mnesia:activity(transaction, F).

-spec teams_count() -> integer().
teams_count() ->
  mnesia:table_info(team, size).


%% Utils


-spec gen_id() -> integer().
gen_id() ->
  erlang:phash2({node(), now()}).

-spec gen_id(term()) -> integer().
gen_id(Mix) ->
  erlang:phash2({node(), now(), Mix}).

get_keys(Start, N, Tab) when N >=0->
  Keys = get_keys1({Start, N}, [], fun(Key, N1) -> {mnesia:next(Tab, Key), N1+1} end),
  lists:reverse(Keys);

get_keys(Start, N, Tab) ->
  get_keys1({Start, N}, [], fun(Key, N1) -> {mnesia:prev(Tab, Key), N1-1} end).

get_keys1({'$end_of_table', _}, Acc, _) ->
  lists:reverse(Acc);

get_keys1({_, 0}, Acc, _) ->
  lists:reverse(Acc);

get_keys1({Key, N}, Acc, Walker) ->
  get_keys1(Walker(Key, N), [Key|Acc], Walker).

to_proplist(Rec) when is_record(Rec, team) ->
  [_|Fields] = tuple_to_list(Rec),
  lists:zipwith(fun(Key, Val) ->
                    {atom_to_binary(Key, unicode), Val}
                end, record_info(fields, team), Fields);

to_proplist(Rec) when is_record(Rec, match) ->
  [_|Fields] = tuple_to_list(Rec),
  lists:zipwith(fun(Key, Val) ->
                    {atom_to_binary(Key, unicode), Val}
                end, record_info(fields, match), Fields);

to_proplist(_) ->
  [{<<"error">>, <<"bad record">>}].
