-module(twitter_tweets_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  SupFlags = {one_for_one, 10, 3600},
  Tweets = {twitter_tweets, {twitter_tweets, start_link, []}, permanent, 2000, worker, [twitter_tweets]},
  {ok, {SupFlags, [Tweets]}}.
