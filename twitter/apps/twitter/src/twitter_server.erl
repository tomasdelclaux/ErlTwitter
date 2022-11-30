-module(twitter_server).
-behaviour(gen_server).
-export([start_link/1]).
-compile(export_all).
-export([init/1, handle_info/2]).
-record(state, {socket}).

%% START AND STOP

start_link(LSock) ->
    gen_server:start_link(?MODULE, LSock, []).

init(LSock) ->
    io:format("Twitter server started~n"),
    gen_server:cast(self(), accept),
    {ok, #state{socket=LSock}}.

terminate(Reason, Args)->
    io:format("Server Conection Closed...~n").

%% API FUNCTIONS

%% HANDLE CLIENT CALLS 
handle_info({tcp, Socket, "REGISTER"++User}, State)->
    inet:setopts(Socket, [{active, once}]),
    Resp=gen_server:call(twitter_users, {register, User}),
    case Resp of
        created->gen_tcp:send(Socket, "UserRegistered"++User);
        _->gen_tcp:send(Socket, "NotRegistered")
    end,
    {noreply, State};

handle_info({tcp, Socket, "CONNECT"++User}, State)->
    inet:setopts(Socket, [{active, once}]),
    Resp=gen_server:call(twitter_users, {connect, User}),
    case Resp of
        connected->gen_tcp:send(Socket, "Connected"++User);
        userNotFound->gen_tcp:send(Socket, "UserNotFound"++User);
        _->gen_tcp:send(Socket,"NotConnected"++User)
    end,
    {noreply, State};

handle_info({tcp, Socket, "SUBSCRIBE"++Vars}, State)->
    inet:setopts(Socket, [{active, once}]),
    [User,ToUser]=string:tokens(Vars, "|"),
    Resp=gen_server:call(twitter_users, {subscribe, User, ToUser}),
    case Resp of
        subscribed->gen_tcp:send(Socket, "Subscribed"++User++"|"++ToUser);
        _->gen_tcp:send(Socket, "Not Subscribed"++User++"|"++ToUser)
    end,
    {noreply, State};

handle_info({tcp, Socket, "TWEET"++Vars}, State)->
    inet:setopts(Socket, [{active, once}]),
    [User,Tweet]=string:tokens(Vars, "|"),
    gen_server:cast(twitter_users, {tweet, User, Tweet}),
    {noreply, State};

handle_info({tcp, Socket, "RETWEET"++Vars}, State)->
    inet:setopts(Socket, [{active, once}]),
    [User,Tid]=string:tokens(Vars, "|"),
    gen_server:cast(twitter_users, {retweet, User, list_to_integer(Tid)}),
    {noreply, State};

handle_info({tcp, Socket, "GETTWEETS"++User}, State)->
    inet:setopts(Socket, [{active, once}]),
    Resp=gen_server:call(twitter_users, {get_tweets, User}),
    case Resp of
        notFound->gen_tcp:send(Socket, "notFound query");
        {found,Tweets}->send_tweets(Socket,Tweets)
    end,
    {noreply, State};

handle_info({tcp, Socket, "GETHASH"++Hash}, State)->
    inet:setopts(Socket, [{active, once}]),
    Resp=gen_server:call(twitter_tweets, {get_hash, Hash}),
    case Resp of
        notFound->gen_tcp:send(Socket, "notFound query");
        {found,Tweets}->send_tweets(Socket,Tweets)
    end,
    {noreply, State};

handle_info({tcp, Socket, "GETMENT"++User}, State)->
    inet:setopts(Socket, [{active, once}]),
    Resp=gen_server:call(twitter_tweets, {get_mention, User}),
    case Resp of
        notFound->gen_tcp:send(Socket, "notFound query");
        {found,Tweets}->send_tweets(Socket,Tweets)
    end,
    {noreply, State};

%% DISCONNECTS
handle_info({tcp, Socket, "DISCONNECT"}, State) ->
  gen_tcp:close(Socket),
  {noreply, State};

handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State};

%Debuggin call
handle_info({tcp, Socket, "PRINT USERS"}, State)->
    inet:setopts(Socket, [{active, once}]),
    gen_server:cast(users_server, {printUsers}),
    {noreply, State}.

%%INTERNAL CALLS
handle_cast(accept, State=#state{socket=LSock})->
    {ok, AcceptSocket} = gen_tcp:accept(LSock),
    twitter_server_sup:start_socket(),
    {noreply, State#state{socket=AcceptSocket}};

handle_cast({send_tweet, Tweet, FromU, Tid}, State=#state{socket=Socket})->
    gen_tcp:send(Socket, "TWEET: "++FromU++"|"++integer_to_list(Tid)++"|"++Tweet),
    io:format("sent twit~n"),
    gen_server:cast(self(), accept),
    {noreply, State}.

send_tweets(Socket, Tweets)->
    [gen_tcp:send(Socket, "TWEET: "++Y++"|"++integer_to_list(X)++"|"++Z) || {X,Y,Z} <- Tweets].