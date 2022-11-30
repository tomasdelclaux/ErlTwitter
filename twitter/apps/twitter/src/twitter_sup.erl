%%%-------------------------------------------------------------------
%% @doc twitter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(twitter_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = {one_for_one, 10, 3600},
    TwitServSup = {twitter_server_sup, {twitter_server_sup, start_link, []}, permanent, 2000, supervisor, [twitter_server_sup]},
    UserSupSpec = {twitter_user_sup, {twitter_user_sup, start_link, []}, permanent, 2000, supervisor, [twitter_user_sup]},
    TweetsSup = {twitter_tweets_sup, {twitter_tweets_sup, start_link, []}, permanent, 2000, supervisor, [twitter_tweets_sup]},
    {ok, {SupFlags, [UserSupSpec, TweetsSup, TwitServSup]}}.

%% internal functions
