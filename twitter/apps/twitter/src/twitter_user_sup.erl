-module(twitter_user_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = {one_for_one, 10, 3600},
  UsersSpec = {twitter_users, {twitter_users, start_link, []}, permanent, 2000, worker, [twitter_users]},
  {ok, {SupFlags, [UsersSpec]}}.
