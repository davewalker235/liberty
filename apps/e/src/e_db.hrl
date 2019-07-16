-type date() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-record(season, {
  id = 0 :: non_neg_integer(),
  slug = <<>> :: binary(),
  name = <<>> :: binary(),
  dates = [] :: [date()]
}).

-record(season__team, {
  season_id = 0 :: non_neg_integer(),
  team_id = 0 :: non_neg_integer()
}).

-record(team, {
  id = 0 :: non_neg_integer(),
  name = <<>> :: binary(),
  skippers = [] :: [binary()],
  crew = [] :: [binary()]
}).

-record(score, {
  season = 0 :: non_neg_integer(),
  date = undefined :: date(),
  team = 0 :: non_neg_integer(),
  boat = 0 :: non_neg_integer(),
  scores = [] :: [integer() | atom()]
}).

-record(boat, {
  id = 0 : non_neg_integer(),
  abbrev = <<>> : binary(),
  name = <<>> : binary()
}).
