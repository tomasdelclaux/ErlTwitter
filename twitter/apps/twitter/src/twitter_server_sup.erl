-module(twitter_server_sup).
-behaviour(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, LSock} = gen_tcp:listen(8000, [{active,once}, {packet,2}]),
    spawn_link(fun empty_listeners/0),
    SupFlags = {simple_one_for_one, 10, 3600},
    TwitterSpec = {twitter_server, {twitter_server, start_link, [LSock]}, temporary, 2000, worker, [twitter_server]},
    {ok, {SupFlags, [TwitterSpec]}}.

start_socket() ->
  supervisor:start_child(?MODULE, []).

empty_listeners() ->
  [start_socket() || _ <- lists:seq(1,20)],
  ok.