-module(twitter_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
  {ok, LSock} = gen_tcp:listen(8000, [{active,once}, {packet,2}]),
  SupFlags = {one_for_one, 10, 3600},
  TwitterSpec = {twit_server, {twit_server, start_link, [LSock]}, temporary, 2000, worker, [twit_server]},
  UserSupSpec = {users_sup, {users_sup, start_link, [Args]}, permanent, 2000, supervisor, [users_sup]},
  {ok, {SupFlags, [TwitterSpec, UserSupSpec]}}.