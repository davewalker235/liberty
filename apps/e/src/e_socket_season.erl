-module(e_socket_season).
-include("e_db.hrl").

-export([
  init/2,
  websocket_init/1,
  websocket_handle/2,
  websocket_info/2
]).

init(Req, _Opts) ->
  {cowboy_websocket, Req, nil, #{idle_timeout => 60000}}.

websocket_init(State) ->
  {ok, self()}.

websocket_handle({text, Msg} = Payload, State) ->
  ws_handle(Payload, State).

ws_handle({text, <<"/season/index">>}, State) ->
  Seasons1 = e_db_season:list(),
  Seasons2 = lists:map(fun e_db:record_to_map/1, Seasons1),
  Seasons3 = lists:map(fun(I) -> maps:remove(dates, I) end, Seasons2),
  Seasons4 = jiffy:encode(Seasons3),
  {reply, {text, <<"/season/index|", Seasons4/binary>>}, State};
ws_handle({text, <<"subscribe:/season/", ID1/binary>> = Path}, State) ->
  <<"subscribe:", Channel/binary>> = Path,
  pg2:create(Channel),
  pg2:join(Channel, State),
  ID = binary_to_integer(ID1),
  Payload = get_json_stats(ID),
  {reply, {text, <<"/season/", ID1/binary, "|", Payload/binary>>}, State};
ws_handle({text, <<"unsubscribe:/season/", ID1/binary>> = Path}, State) ->
  io:format("~p", [Path]),
  <<"unsubscribe:", Channel/binary>> = Path,
  pg2:leave(Channel, State),
  {ok, State};

ws_handle({text, <<"score:", Data/binary>>}, State) ->
  {Data1} = jiffy:decode(Data),
  Rec1 = e_db_score:from_post(Data1),
  e_db_score:update(Rec1),
  erlang:display(Rec1),
  Payload = get_json_stats(Rec1#score.season),
  io:format("~p", ["test"]),
  {reply, {text, [<<"/season/">>, integer_to_binary(Rec1#score.season), <<"|">>, Payload]}, State};

ws_handle(Data, State) ->
  erlang:display(Data),
	{ok, State}.

get_json_stats(ID) ->
  Season1 = e_db_season:read(ID),
  Season2 = e_db:record_to_map(Season1),
  Season3 = maps:remove(dates, Season2),
  Payload1 = #{
    season => Season3,
    results => e_db_season:ordered_stats(ID),
    scores => lists:map(fun e_db_score:to_map/1, e_db_score:list(ID))
  },
  Payload2 = iolist_to_binary(jiffy:encode(Payload1)).

websocket_info({timeout, _Ref, Msg}, State) ->
	{reply, {text, Msg}, State};
websocket_info(update, State) ->
  Payload = get_json_stats(1),
  {reply, {text, [<<"/season/1|">>, Payload]}, State};

websocket_info(Msg, State) ->
  {reply, {text, Msg}, State}.
