-module(e_db_team).
-export([
  read/1
]).

read(ID) -> e_db:read(team, ID).
