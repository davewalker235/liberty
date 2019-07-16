-module(e_db).
-include("e_db.hrl").
-export([
  start/0,
  clear_tables/0,
  record_to_map/1,
  read/2
]).

-define(TABLES, [
  {counter      , set          , [table, count]},
  {season       , ordered_set  , record_info(fields, season)},
  {team         , ordered_set  , record_info(fields, team)},
  {season__team , bag          , record_info(fields, season__team)},
  {score        , bag          , record_info(fields, score)},
  {boat         , set          , record_info(fields, boat)}
]).

start() ->
  pg2:create(score),
  pg2:create(team),
  mnesia:create_schema([node()]),
  mnesia:start(),
  create_tables(),
  mnesia:wait_for_tables([element(1, Table) || Table <- ?TABLES], 3000).

create_tables() ->
  [
    mnesia:create_table(Table, [{disc_copies, [node()]}, {type, Type}, {attributes, Fields}])
    || {Table, Type, Fields} <- ?TABLES
  ],
  mnesia:add_table_index(score, date),
  mnesia:add_table_index(season, slug).

clear_tables() ->
  [
    [mnesia:dirty_delete(Table, K) || K <- mnesia:dirty_all_keys(Table)]
    || {Table, _, _} <- ?TABLES
  ].

boats() -> [
  {173, <<"DE">>, <<"Delaware">>}
  , {174, <<"PA">>, <<"Pennsylvania">>}
  , {175, <<"NJ">>, <<"New Jersey">>}
  , {176, <<"GA">>, <<"Georgia">>}
  , {177, <<"IN">>, <<"Independance">>}
  , {178, <<"CT">>, <<"Conneticut">>}
  , {179, <<"MA">>, <<"Massachusetts">>}
].

read(Type, ID) ->
  case mnesia:dirty_read(Type, ID) of
    [Item | _Tail] -> Item;
    _Else -> nil
  end.

record_to_map(#season{} = I)       -> record_to_map(record_info(fields, season), I);
record_to_map(#team{} = I)         -> record_to_map(record_info(fields, team), I);
record_to_map(#season__team{} = I) -> record_to_map(record_info(fields, season__team), I);
record_to_map(#score{} = I)        -> record_to_map(record_info(fields, score), I);
record_to_map(#boat{} = I)         -> record_to_map(record_info(fields, boat), I).
record_to_map(Keys, Values) ->
  Values1 = tuple_to_list(Values),
  Values2 = tl(Values1),
  Props = lists:zip(Keys, Values2),
  maps:from_list(Props).
