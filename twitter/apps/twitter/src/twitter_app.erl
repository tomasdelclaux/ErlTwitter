%%%-------------------------------------------------------------------
%% @doc twitter public API
%% @end
%%%-------------------------------------------------------------------

-module(twitter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        { <<"127.0.0.1">>, [{<<"/">>, twitter_server, []}] }
    ]),
    {ok, _} = cowboy:start_clear(
        ws,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    twitter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
