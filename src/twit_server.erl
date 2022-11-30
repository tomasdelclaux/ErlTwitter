-module(twit_server).
-behaviour(gen_server).

-export([start_link/1]).
% -export([alloc/0, free/1]).
% -export([init/1, handle_call/3, handle_cast/2]).
-compile(export_all).
-export([init/1, stop/0, register/1, handle_call/3, handle_cast/2]).
-record(state, {socket}).

%% START AND STOP

start_link(LSock) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, LSock, []).

init(LSock) ->
    io:format("Twitter server started~n"),
    gen_server:cast(?MODULE, accept),
    {ok, #state{socket=LSock}}.

stop()->
    gen_server:stop(?MODULE).

terminate(Reason, Args)->
    io:format("Terminating... ~n").

%% API FUNCTIONS

register(User)->
    gen_server:call(?MODULE, {register, User}).

search(Request)->
    case Request of
        {subscriber, Subs}->gen_server:cast(?MODULE,{subscriber,Subs});
        {hash, Hash}->gen_server:cast(?MODULE, {hash, Hash});
        {mention, Mention}->gen_server:cast(?MODULE,{mention, Mention})
    end.

%% HANDLE SYNCHRONOUS CALLS

handle_call({register, User}, From, Args)->
    io:format("User ~s is registered ~n", [User]),
    {reply, User, Args}.

%% HANDLE ASYNCHRONOUS CALLS
handle_cast({subscriber, Subs}, Args)->
    io:format("search subscriber ~s tweets~n", [Subs]),
    {noreply, Args};

handle_cast({hash, Hash}, Args)->
    io:format("search for Hashtag ~s~n", [Hash]),
    {noreply, Args};

handle_cast({mention, Mention}, Args)->
    io:format("search for Mention ~s", [Mention]),
    {noreply, Mention};

handle_cast(accept, State=#state{socket=LSock})->
    {ok, AcceptSocket} = gen_tcp:accept(LSock),
    {noreply, State#state{socket=AcceptSocket}}.


%% HANDLE INFO 

handle_info({tcp, Socket, Msg}, State) ->
    inet:setopts(Socket,[{active,once}]),
    case Msg of
        "REGISTER"++User->gen_server:call(users_server, {register, User});
        "SEND TWEET"++Tweet->io:format("TODO");
        "SUBSCRIBE"++ToUser->io:format("TODO");
        "RETWEET"++TweetId->io:format("TODO");
        "SEARCH MENTIONS"->io:format("TODO");
        "SEARCH HASH"->io:format("TODO");
        "SEARCH SUBSCRIBED"->io:format("TODO");
        "PRINT USERS"->gen_server:cast(users_server, {printUsers});
        _->io:format("~s", [Msg])
    end,
    {noreply, State}.
