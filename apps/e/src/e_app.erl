-module(e_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  e_db:start(),
  e_sup:start_link().

stop(_State) ->
  ok.
