%%%-------------------------------------------------------------------
%% @doc twitterClient public API
%% @end
%%%-------------------------------------------------------------------

-module(twitterClient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Ip="127.0.0.1",
    Port=8000,
    Username="fred",
    twitterClient_sup:start_link(Username, Port, Ip).


stop(_State) ->
    ok.

%% internal functions
