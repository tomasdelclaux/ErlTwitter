-module(users_server).
-behaviour(gen_server).

-export([start_link/1]).
-compile(export_all).
-export([init/1, stop/0, register/1, handle_call/3, handle_cast/2]).
-record(user, {name, connected, subscribers=[], mentions=[]}).
-record(state,
    {
        users=[] ::Record::#user{}
    }).

%% START AND STOP

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    io:format("users server started~n"),
    {ok, #state{users=[]}}.

stop()->
    gen_server:stop(?MODULE).

terminate(Reason, Args)->
    io:format("Terminating... ~n").

%% API FUNCTIONS

register(User)->
    gen_server:call(?MODULE, {register, User}).

%% HANDLE SYNCHRONOUS CALLS

handle_call({register, User}, From, State=#state{users=Ulist})->
    io:format("Registering New User ~s ~n", [User]),
    NewUser = [#user{name=User,connected=true, subscribers=[],mentions=[]}],
    {reply, User, State#state{users=Ulist++NewUser}}.

%% HANDLE ASYNCHRONOUS CALLS
handle_cast({subscriber, Subs}, Args)->
    io:format("search subscriber ~s tweets~n", [Subs]),
    {noreply, Args};

handle_cast({printUsers}, State=#state{users=Ulist})->
    printUsers(Ulist),
    {noreply, State}.

printUsers([H|T])->
    io:format("~s~n",[H#user.name]),
    [H|printUsers(T)];

printUsers([])->
    ok.