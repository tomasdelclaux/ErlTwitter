-module(twitter_server).

-export([init/2]).
-compile(export_all).


init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_handle({text, Json}, State) ->
    Map=jiffy:decode(Json, [return_maps]),
    Command =  binary_to_atom(maps:get(<<"command">>, Map)),
    Vars= maps:get(<<"vars">>, Map),
    {{text,Res},_}=websocket_handle_({text, {Command, Vars}}, State),
    {[{text, Res}], State}.

websocket_handle_({text, {register, [User|_]}}, State) ->
    Resp=gen_server:call(twitter_users, {register, User}),
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
    Resp=gen_server:call(twitter_users, {connect, User}),
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

websocket_handle_({text, {tweet, [User|[Tweet]]}}, State) ->
    gen_server:cast(twitter_users, {tweet, User, Tweet}),
     Data = #{status => <<"sent">>},
    {{text, jiffy:encode(Data)}, State}.

%% TODO NEED TO ADD GET MENTION, GET HASH, SEARCH QUERIES

%% NEED TO ADD THE SEND TWEETS TO TWITTER CLIENT

