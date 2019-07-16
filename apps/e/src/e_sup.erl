-module(e_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).
-define(PORT, 8081).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Routes = [
    {"/rest", e_rest_season, nil},
    {"/socket", e_socket_season, nil}
  ],

  io:format("Liberty Scorekeeper starting on http://localhost:~p~n", [?PORT]),

  {ok, {#{
    strategy => one_for_all,
    period => 1,
    intensity => 10
  }, [
    #{id => cowboy_sup, start => {cowboy_sup, start_link, []}},
    #{id => ranch_sup, start => {ranch_sup, start_link, []}},
    #{
      id => listener,
      start => {cowboy, start_clear, [e_endpoint, [{port, ?PORT}], #{
          env => #{dispatch => cowboy_router:compile([{'_', Routes}])},
          stream_handlers => [cowboy_compress_h, cowboy_stream_h]
        }
      ]}
    }
  ]}}.
