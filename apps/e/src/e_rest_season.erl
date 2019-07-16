-module(e_rest_season).
-include("e_db.hrl").
-export([
  init/2,
  allowed_methods/2,
  is_authorized/2,
  content_types_provided/2,
  content_types_accepted/2,
  resource_exists/2,
  delete_resource/2,
  render_html/2,
  upsert/2
]).

-record(state, {
  action   = index     :: atom(),
  resource = #season{} :: #season{}
}).

init(Req, Action) ->
  {cowboy_rest, Req, #state{action = Action}}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

is_authorized(Req, State) ->
  {true, Req, State}.

% resource_exists(#{bindings := #{slug := Slug}} = Req, State) ->
%   case e_db_season:read(Slug) of
%     nil -> {false, Req, State};
%     Resource -> {true, Req, State#state{resource = Resource}}
%   end;
resource_exists(Req, State) ->
  {true, Req, State}.

content_types_provided(Req, State) ->
  Provide = [{<<"text/html">>, render_html}],
	{Provide, Req, State}.

content_types_accepted(Req, State) ->
  Accept = [{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, upsert}],
  {Accept, Req, State}.

upsert(Req, #state{resource = Resource} = State) ->
  {ok, Post, Req1} = cowboy_req:read_urlencoded_body(Req),
  {Resource1, Teams} = post_update(Resource, Post),
  {ok, Resource2} = case Resource1#season.id of
    0 -> db_season:create(Resource1);
    _ -> db_season:update(Resource1, Teams)
  end,
  {{true, <<"/season/", (Resource2#season.slug)/binary, "/edit">>}, Req1, State}.

delete_resource(Req, #state{resource = Resource}) ->
  db_season:delete(Resource#season.id),
  {true, Req, #state{}}.

render_html(Req, #state{action = Action, resource = Resource} = State) ->
  Output = web_template_season:render(Action, Resource),
  {Output, Req, State}.

post_update(Resource, Post) ->
  % All dates and teams are reset from POST to allow for removing associations
  Resource1 = Resource#season{dates = []},
  {Resource2, Teams} = lists:foldr(fun({K, V}, {S, T}) ->
    case K of
      <<"season.name">> -> {S#season{name = V}, T};
      <<"season.date">> ->
        case web_formatter:input_to_date(V) of
          error -> {S, T};
          Date -> {S#season{dates = [Date | S#season.dates]}, T}
        end;
      <<"team.id">> ->
        case V of
          <<"0">> -> {S, T};
          _ -> {S, [binary_to_integer(V) | T]}
        end
    end
  end, {Resource1, []}, Post),
  {Resource2, Teams}.
