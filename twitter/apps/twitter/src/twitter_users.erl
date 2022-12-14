-module(twitter_users).
-behaviour(gen_server).
-export([start_link/0]).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2]).
-record(user, {name, conn, subscribers=[], myTweets=[]}).
-record(state,
    {
        users=[] ::Record::#user{}
    }).

%% START AND STOP

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

init([]) ->
    io:format("users server started~n"),
    {ok, #state{users=[]}}.

terminate(Reason, Args)->
    io:format("Terminating Twitter Users ~n").

%%%%%% API FUNCTIONS

%% HANDLE SYNCHRONOUS CALLS

handle_call({register, User, Ws}, _From, State=#state{users=Ulist})->
    io:format("Registering New User ~s from ~w ~n", [User,Ws]),
    NewUser = [#user{name=User,conn=Ws, subscribers=[],myTweets=[]}],
    {reply, created, State#state{users=Ulist++NewUser}};

handle_call({connect, User, Ws}, _From, State=#state{users=Ulist})->
    Found = [X || X <- Ulist, X#user.name == User],
    case Found of
        []->{reply, userNotFound, State};
        _->
            FoundUser = lists:nth(1,Found),
            NewItem = [FoundUser#user{conn=Ws}],
            OldList = lists:delete(FoundUser,Ulist),
            {reply, connected, State#state{users=OldList++NewItem}}
    end;

handle_call({subscribe, User, ToUser}, _From, State=#state{users=Ulist})->
    Found = [X || X <- Ulist, X#user.name == ToUser],
    case Found of
        []->{reply, userNotFound, State};
        _->
            FoundUser = lists:nth(1,Found),
            PrevListSubs = FoundUser#user.subscribers,
            NewItem = [FoundUser#user{subscribers=PrevListSubs++[User]}],
            io:format("new items ~w~n", [NewItem]),
            OldList = lists:delete(FoundUser,Ulist),
            {reply, subscribed, State#state{users=OldList++NewItem}}
    end;

handle_call({get_tweets, User}, _From, State=#state{users=Ulist})->
    Found = [X || X <- Ulist, X#user.name == User],
    case Found of
        []->{reply, notFound,State};
        _->
            FoundUser= lists:nth(1,Found),
            Tids=FoundUser#user.myTweets,
            Resp = gen_server:call(twitter_tweets,{get_tweets, Tids}),
            {reply, Resp, State}
    end.

handle_cast({retweet, User, Tid}, State=#state{users=Ulist})->
    Found = [X || X <- Ulist, X#user.name == User],
    case Found of
        []->{noreply, State};
        _->
            {Tweet, Author} = gen_server:call(twitter_tweets,{get_tweet, Tid}),
            FoundUser = lists:nth(1,Found),
            Subscribers = FoundUser#user.subscribers,
            gen_server:cast(self(), {get_subs, Tweet, Author, Tid, Subscribers}),
            {noreply, State}
    end;

%%%HANDLE ASYNCHRONOUS CALLS
handle_cast({tweet, User, Tweet}, State=#state{users=Ulist})->
    Found = [X || X <- Ulist, X#user.name == User],
    io:format("State Tweet ~w", [Found]),
    case Found of
        []->{noreply, State};
        _->
            NewTwid = gen_server:call(twitter_tweets,{new_tweet, Tweet, User}),
            FoundUser = lists:nth(1,Found),
            Subscribers = FoundUser#user.subscribers,
            gen_server:cast(self(), {get_subs, Tweet, User, NewTwid, Subscribers}),
            {noreply, State}
    end;

handle_cast({add_tweet, Tid, User}, State=#state{users=Ulist})->
    Found = [X || X <- Ulist, X#user.name == User],
    case Found of
        []->{noreply, State};
        _->
            FoundUser = lists:nth(1,Found),
            PrevListIds = FoundUser#user.myTweets,
            NewItem = [FoundUser#user{myTweets=PrevListIds++[Tid]}],
            OldList = lists:delete(FoundUser,Ulist),
            {noreply, State#state{users=OldList++NewItem}}
    end;

handle_cast({get_subs, Tweet, Author, Tid, Subscribers}, State=#state{users=Ulist})->
    io:format("Subscribers ~w~n", [Subscribers]),
    SubSocks =  [getUser(X, Ulist) || X <- Subscribers],
    io:format("Sub Socks ~w~n", [SubSocks]),
    [X ! {send_tweet, Tweet, Author, Tid} || X <- SubSocks],
    [gen_server:cast(twitter_users, {add_tweet, Tid, X}) || X <- Subscribers],
    {noreply, State};


%%%%% DEBUG
handle_cast({printUsers}, State=#state{users=Ulist})->
    io:format("~w", [State]),
    printUsers(Ulist),
    {noreply, State}.

printUsers([H|T])->
    io:format("~s~n",[H#user.name]),
    [H|printUsers(T)];

printUsers([])->
    ok.

getUser(User, Ulist)->
    Sub=lists:nth(1,[X || X <- Ulist, X#user.name == User]),
    Sub#user.conn.