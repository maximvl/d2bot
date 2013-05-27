-module(matches_db).
-export([setup/1, to_proplist/1]).

%% Team api
-export([store_team/1, store_team/2, delete_team/1,
         fetch_team/1, fetch_teams/1, fetch_teams/2,
         teams_count/0]).

-record(team, {id :: integer(),
               name :: binary()
              }).

-record(match, {id :: integer(),
                teamA :: integer(),
                teamB :: integer(),
                time :: calendar:datetime(),
                bestof :: integer()
               }).

-record(tournament, {id :: integer(),
                    matches :: [integer()],
                    name :: binary(),
                    season :: binary()
                   }).

setup(Nodes) ->
  mnesia:create_schema(Nodes),                  % schema and tables can already exist
  rpc:multicall(Nodes, application, start, [mnesia]),
  mnesia:create_table(team,
                      [{attributes, record_info(fields, team)},
                       {disc_copies, Nodes},
                       {index, [name]}]),
  mnesia:create_table(match,
                      [{attributes, record_info(fields, match)},
                       {disc_copies, Nodes}]),
  mnesia:create_table(tournament,
                     [{attirbutes, record_info(fields, tournament)},
                      {disc_copies, Nodes},
                      {index, [name]}]).
  

-spec store_team(binary()) -> ok | iodata().
store_team(Name) ->
  F = fun() ->
          case mnesia:index_read(team, Name, name) of
            [] ->
              mnesia:write(#team{
                              id = gen_id(Name),
                              name = Name});
            _ ->
              [<<"error">>, <<"already exist">>]
          end
      end,
  mnesia:activity(transaction, F).

-spec store_team(integer(), binary()) -> ok | iodata().
store_team(Id, NewName) ->
  F = fun() ->
          case mnesia:read(team, Id) of
            [] ->
              [<<"error">>, <<"no such item">>];
            Team ->
              mnesia:write(Team#team{name = NewName})
          end
      end,
  mnesia:activity(transaction, F).

-spec delete_team(integer()) -> ok.
delete_team(Id) when is_integer(Id) ->
  F = fun() ->
          mnesia:delete({team, Id})
      end,
  mnesia:activity(transaction, F);

delete_team(Name) ->
  F = fun() ->
          case mnesia:index_read(team, Name, name) of
            [T] ->
              mnesia:delete({team, T#team.id});
            [] ->
              ok
          end
      end,
  mnesia:activity(transaction, F).

-spec fetch_team(integer() | binary()) -> [#team{}] | {aborted, term()}.
fetch_team(Id) when is_integer(Id) ->
  F = fun() ->
          mnesia:read(team, Id)
      end,
  mnesia:activity(transaction, F);

fetch_team(Name) ->
  F = fun() ->
          mnesia:index_read(team, Name, #team.name)
      end,
  mnesia:activity(transaction, F).

-spec fetch_teams(integer()) -> [#team{}].
fetch_teams(N) ->
  F = fun() ->
          Keys = get_keys(mnesia:first(team), N, team),
          lists:flatten([mnesia:read(team, Key) || Key <- Keys])
      end,
  mnesia:activity(transaction, F).

-spec fetch_teams(integer(), integer()) -> [#team{}].
fetch_teams(StartID, N) ->
  F = fun() ->
          Keys = get_keys(StartID, N, team),
          lists:flatten([mnesia:read(team, Key) || Key <- Keys])
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
  to_proplist1(Rec, record_info(fields, team));

to_proplist(Rec) when is_record(Rec, match) ->
  to_proplist1(Rec, record_info(fields, match));

to_proplist(Rec) when is_record(Rec, tournament) ->
  to_proplist1(Rec, record_info(fields, tournament));

to_proplist(_) ->
  [{<<"error">>, <<"unknown record">>}].

to_proplist1(Rec, Fields) ->
  [_|Vals] = tuple_to_list(Rec),
  lists:zipwith(fun(Key, Val) ->
                    {atom_to_binary(Key, unicode), Val}
                end, Fields, Vals).
