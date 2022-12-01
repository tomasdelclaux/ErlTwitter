%%%-------------------------------------------------------------------
%% @doc twitterClient top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(twitterClient_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

start_link(Username,Port, Ip) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Username,Port, Ip}).

init({Username,Port, Ip}) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ClientSpecs = {twitterClient_client, {twitterClient_client, start_link, [Username,Port,Ip, false]}, permanent, 2000, worker, [twitterClient_client]},
    {ok, {SupFlags, [ClientSpecs]}}.

