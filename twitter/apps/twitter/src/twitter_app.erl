%%%-------------------------------------------------------------------
%% @doc twitter public API
%% @end
%%%-------------------------------------------------------------------

-module(twitter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    twitter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
