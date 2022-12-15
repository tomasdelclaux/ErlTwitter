-module(twitter_server).
-behavior(gen_server).
-export([init/2]).
-compile(export_all).
-record(connections, {ws}).


init(Req, Opts) ->
	{cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    io:format("Initializing server connection ~w", [self()]),
	{[], #connections{ws=self()}}.

websocket_handle({text, Json}, State) ->
    Map=jiffy:decode(Json, [return_maps]),
    Command =  binary_to_atom(maps:get(<<"command">>, Map)),
    io:format("command ~w~n", [Command]),
    Vars= maps:get(<<"vars">>, Map),
    {{text,Res},_}=websocket_handle_({text, {Command, Vars}}, State),
    {[{text, Res}], State};

websocket_handle(_Data, State) ->
	{[], State}.

websocket_handle_({text, {register, [User|_]}}, State=#connections{ws=Ws}) ->
    Resp=gen_server:call(twitter_users, {register, User, Ws}),
    case Resp of
        created->
            Data = #{status => <<"UserRegistered">>, user => User},
            io:format("done~n"),
            {{text, jiffy:encode(Data)}, State};
        _->
            Data = #{status => <<"NotRegistered">>, user => User},
            {{text, jiffy:encode(Data)}, State}
    end;

websocket_handle_({text, {connect, [User|_]}}, State) ->
    Resp=gen_server:call(twitter_users, {connect, User, self()}),
    case Resp of
        connected->
            Data = #{status => <<"Connected">>, user => User},
            {{text, jiffy:encode(Data)}, State};
        userNotFound->
            Data = #{status => <<"UserNotFound">>, user => User},
            {{text, jiffy:encode(Data)}, State};
        _->
            Data = #{status => <<"NotConnected">>, user => User},
            {{text, jiffy:encode(Data)}, State}
    end;

websocket_handle_({text, {subscribe, [User|[ToUser]]}}, State) ->
    Resp=gen_server:call(twitter_users, {subscribe, User, ToUser}),
    case Resp of
        subscribed->
            Data=#{status => <<"subscribed">>, user => User, toUser=>ToUser},
            {{text, jiffy:encode(Data)}, State};
        _->
            Data=#{status => <<"unable to subscribe">>, user => User, toUser=>ToUser},
            {{text, jiffy:encode(Data)}, State}
    end;

websocket_handle_({text, {tweet, [User|[Tweet]]}}, State) ->
    gen_server:cast(twitter_users, {tweet, User, Tweet}),
    Data = #{status => <<"sent">>},
    {{text, jiffy:encode(Data)}, State};

websocket_handle_({text, {retweet, [User|[Tid]]}}, State) ->
    gen_server:cast(twitter_users, {retweet, User, list_to_integer(Tid)}),
    Data = #{status => <<"sent">>},
    {{text, jiffy:encode(Data)}, State};

websocket_handle_({text, {getTweets, [User|_]}}, State) ->
    Resp=gen_server:call(twitter_users, {get_tweets, User}),
    case Resp of
        notFound->
            Data = #{status => <<"got tweets">>, tweets=>[]},
            {{text, jiffy:encode(Data)}, State};
        {found,Tweets}->
            TJson = [#{tid=>X, from=>Y, tweet=>Z} || {X,Y,Z}<-Tweets],
            Data = #{status => <<"got tweets">>, tweets=>TJson},
            {{text, jiffy:encode(Data)}, State}
    end;

websocket_handle_({text, {getHash, [Hash|_]}}, State) ->
    Resp=gen_server:call(twitter_tweets, {get_hash, Hash}),
    case Resp of
        notFound->
            Data = #{status => <<"hash not found">>, tweets=>[]},
            {{text, jiffy:encode(Data)}, State};
        {found,Tweets}->
            TJson = [#{tid=>X, from=>Y, tweet=>Z} || {X,Y,Z}<-Tweets],
            Data = #{status => <<"hash found">>, tweets=>TJson},
            {{text, jiffy:encode(Data)}, State}
    end;

websocket_handle_({text, {getMention, [User|_]}}, State) ->
    Resp=gen_server:call(twitter_tweets, {get_mention, User}),
    case Resp of
        notFound->
            Data = #{status => <<"mention not found">>, tweets=>[]},
            {{text, jiffy:encode(Data)}, State};
        {found,Tweets}->
            TJson = [#{tid=>X, from=>Y, tweet=>Z} || {X,Y,Z}<-Tweets],
            Data = #{status => <<"mention found">>, tweets=>TJson},
            {{text, jiffy:encode(Data)}, State}
    end.

handle_cast({send_tweet, Tweet, FromU, Tid}, State)->
    io:format("GOT TWEET TO SEND OUT~n"),
    Data=#{status => <<"new tweet">>, tid =>Tid, from => FromU, tweet=>Tweet},
    erlang:start_timer(1000, self(), jiffy:encode(Data)),
    {noreply, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	{[{text, Msg}], State};

websocket_info(_Info, State) ->
    io:format("GOT TWEET TO SEND OUT~n"),
    case _Info of
        {send_tweet, Tweet, FromU, Tid}->
            Data=#{status => <<"new tweet">>, tid =>Tid, from => FromU, tweet=>Tweet},
            {[{text, jiffy:encode(Data)}], State};
        _->{[], State}
    end.