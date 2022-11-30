-module(users_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  SupFlags = {one_for_one, 10, 3600},
  UsersSpec = {users_server, {users_server, start_link, [Args]}, permanent, 2000, worker, [users_server]},
  {ok, {SupFlags, [UsersSpec]}}.
