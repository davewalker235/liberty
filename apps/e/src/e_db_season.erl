-module(e_db_season).
-include("e_db.hrl").
-export([
  list/0,
  read/1,
  stats/1,
  ordered_stats/1
]).

read(ID) -> e_db:read(season, ID).

list() ->
  [read(I) || I <- mnesia:dirty_all_keys(season)].

stats(SeasonID) ->
  Season = read(SeasonID),
  Scores = mnesia:dirty_read(score, SeasonID),
  stats(Scores, Season#season.dates).
stats(Scores, SeriesDates) -> stats(Scores, SeriesDates, #{}, #{}).
stats([], _SeriesDates, Avgs, Series) ->
  stats_wrapup(Avgs, Series);
stats([H | T] = _Scores, SeriesDates, Avgs, Series) ->
  #score{date = Date, team = Team, scores = Scores} = H,
  Scores1 = case Scores of
    committee -> Date;
    _Else -> Scores
  end,
  SeriesKey = get_series(Date, SeriesDates),
  Avgs1 = stats_update_avgs(Avgs, Scores, Date),
  Series1 = maps:update_with(Team, fun(SeriesMap) ->
    maps:update_with(SeriesKey, fun(OldScores) ->
      [Scores1 | OldScores]
    end, [Scores1], SeriesMap)
  end, #{SeriesKey => [Scores1]}, Series),
  stats(T, SeriesDates, Avgs1, Series1).

stats_update_avgs(Avgs, committee, _Date) ->
  Avgs;
stats_update_avgs(Avgs, Scores, Date) ->
  Sum = lists:sum(Scores),
  maps:update_with(Date, fun({Sum1, Count}) -> {Sum + Sum1, Count + 1} end, {Sum, 1}, Avgs).

stats_wrapup(Avgs, Series) ->
  Avgs1 = maps:map(fun(_K, {Sum, Count}) -> Sum / Count end, Avgs),
  Series1 = maps:map(fun(TeamID, TeamSeries) ->
    maps:map(fun(SeriesID, Scores) ->
      OrderedScores = lists:reverse(Scores),
      Worst = worst(Scores),
      Total = lists:foldl(fun
        (I, Acc) when is_integer(I) -> I + Acc;
        (I, Acc) when is_tuple(I) -> maps:get(I, Avgs1) + Acc
      end, 0, lists:flatten(Scores)),
      #{
        scores => OrderedScores,
        worst => Worst,
        total => Total,
        adjusted_total => Total - lists:sum(Worst)
      }
    end, TeamSeries)
  end, Series),
  {Avgs1, Series1}.

worst(Scores) ->
  Scores1 = lists:flatten(Scores),
  Scores2 = lists:filter(fun is_integer/1, Scores1),
  Scores3 = lists:sort(Scores2),
  Scores4 = lists:reverse(Scores3),
  Scores5 = lists:sublist(Scores4, 2).

ordered_stats(SeasonID) when is_integer(SeasonID) ->
  ordered_stats(stats(SeasonID));
ordered_stats({_Avgs, Stats}) ->
  Results1 = maps:map(fun(K1, V1) ->
    Series1 = maps:map(fun(K2, V2) -> maps:get(adjusted_total, V2) end, V1),
    Series2 = maps:to_list(Series1),
    Series3 = lists:sort(Series2),
    Series4 = lists:map(fun({_, V}) -> V end, Series3),
    #{
      team => e_db:record_to_map(e_db_team:read(K1)),
      series => Series4,
      total => lists:sum(Series4)
    }
  end, Stats),
  Results2 = maps:values(Results1),
  Results3 = lists:sort(fun(#{total := A}, #{total := B}) -> A < B end, Results2).
get_series(Date, Series) ->
  get_series(Date, Series, 0).
get_series(_Date, [], Index) ->
  Index;
get_series(Date, [H | T], Index) ->
  case Date < H of
    false -> get_series(Date, T, Index + 1);
    true -> Index
  end.
