-module(e_db_score).
-include("e_db.hrl").
-export([
  to_map/1,
  list/1,
  update/1,
  from_post/1
]).

list(SeasonID) ->
  Scores = mnesia:dirty_read(score, SeasonID),
  lists:sort(fun (A, B) -> {A#score.date, A#score.team} < {B#score.date, B#score.team} end, Scores).

%% @doc Should take a whole score for a date and team ond replace anything in
%% the database with the new information.
update(#score{date = Date, team = Team} = Score) ->
  F = fun () ->
    Records = mnesia:match_object(#score{date = Date, team = Team, _ = '_'}),
    lists:map(fun mnesia:delete_object/1, Records),
    mnesia:write(Score)
  end,
  mnesia:transaction(F).


to_map(Score) when is_record(Score, score) ->
  Map1 = e_db:record_to_map(Score),
  Map2 = maps:update_with(date, fun e_formatter:date_to_input/1, Map1),
  Map2.

from_post(Post) ->
  Fields1 = record_info(fields, score),
  Fields2 = [atom_to_binary(I, utf8) || I <- Fields1],
  Fields3 = lists:zip(Fields2, lists:seq(2, length(Fields2) + 1)),
  Fields4 = maps:from_list(Fields3),
  erlang:display(Post),
  Record1 = lists:foldl(fun({K, V}, Rec) ->
    case maps:get(K, Fields4, nil) of
      nil -> Rec;
      I -> case {K, V} of
        {<<"scores">>, <<"committee">>} -> setelement(I, Rec, committee);
        {<<"scores">>, V} -> setelement(I, Rec, lists:filter(fun is_integer/1, V));
        _Else -> setelement(I, Rec, V)
      end
    end
  end, #score{}, Post),
  Record2 = Record1#score{date = e_formatter:input_to_date(Record1#score.date)},
  Record2.
