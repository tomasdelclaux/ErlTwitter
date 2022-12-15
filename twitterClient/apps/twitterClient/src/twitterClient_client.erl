-module(twitterClient_client).
-compile(export_all).
-compile(gen_server).
-record(state, {username,port,ip,conn, stream}).

start_link(Username, Port, Ip)->
    gen_server:start_link({local, ?MODULE},?MODULE, {Username, Port,Ip},[]).

init({Username, Port,Ip})->
    {ok, ConnPid} = gun:open("127.0.0.1", 8080, #{
		ws_opts => #{
			keepalive => 100,
			silence_pings => false
		}
	}),
    {ok, _} = gun:await_up(ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/", []),
	{upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
    State=#state{username=Username, conn=ConnPid, stream=StreamRef},
    {ok, State}.

handle_info({gun_ws, PId, Ref, {_, Msg}}, State)->
    Map=jiffy:decode(Msg, [return_maps]),
    prettyPrint(Map),
    {noreply, State#state{conn=PId}};

handle_info({gun_upgrade, PId, Ref, _, _}, State)->
    {noreply, State#state{conn=PId}};

handle_info({gun_up, PId, http}, State) ->
    io:format("Server is Up~n"),
    error_logger:info_msg("apns up:~p", [PId]),
    {noreply, State#state{conn=PId}};

handle_info({gun_ws, PId, Ref, {_, Msg, _}}, State)->
    io:format("Closing...~n"),
    {noreply, State#state{conn=PId}};

handle_info({gun_ws, PId, Ref, pong}, State)->
    {noreply, State#state{conn=PId}};

handle_info({gun_down, PId, _, _, _}, State) ->
    io:format("Connection got close~n"),
    io:format("try to reconnect...~n"),
    {ok, ConnPid} = gun:open("127.0.0.1", 8080, #{
		ws_opts => #{
			keepalive => 100,
			silence_pings => false
		}
	}),
    {ok, _} = gun:await_up(ConnPid),
	StreamRef = gun:ws_upgrade(ConnPid, "/", []),
    {upgrade, [<<"websocket">>], _} = gun:await(ConnPid, StreamRef),
    {noreply, #state{conn=ConnPid}}.

handle_cast({register, User}, State=#state{conn=C, stream=S})->
    io:format("Sending User ~s to be registered ~n", [User]),
    Data=#{command => <<"register">>, vars=>[User]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State#state{username=User}};

handle_cast({connect, User}, State=#state{conn=C, stream=S})->
    io:format("Connection as ~s~n", [User]),
    Data=#{command => <<"connect">>, vars=>[User]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast({subscribe, ToUser}, State=#state{conn=C, stream=S, username=User})->
    Data=#{command => <<"subscribe">>, vars=>[User, ToUser]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast({tweet, Msg}, State=#state{conn=C, stream=S,username=User})->
    Data=#{command => <<"tweet">>, vars=>[User, Msg]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast({retweet, Tid}, State=#state{conn=C, stream=S, username=User})->
    Data=#{command => <<"retweet">>, vars=>[User, integer_to_list(Tid)]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast({get_my_tweets}, State=#state{conn=C, stream=S, username=User})->
    Data=#{command => <<"getTweets">>, vars=>[User]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast({get_hash, Hash}, State=#state{conn=C, stream=S})->
    Data=#{command => <<"getHash">>, vars=>[Hash]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast({get_mention}, State=#state{conn=C, stream=S, username=User})->
    Data=#{command => <<"getMention">>, vars=>[User]},
    gun:ws_send(C, S, {text, jiffy:encode(Data)}),
    {noreply, State};

handle_cast(_, State)->
    io:format("Please pass a valid command~n"),
    {noreply, State}.


prettyPrint(Map)->
    Status=maps:get(<<"status">>, Map),
    case Status of
        <<"UserRegistered">>->
            User=maps:get(<<"user">>, Map),
            io:format("Registered user ~s~n", [User]);
        <<"NotRegistered">>->
            User=maps:get(<<"user">>, Map),
            io:format("unable to register user ~s~n", [User]);
        <<"Connected">>->
            User=maps:get(<<"user">>, Map),
            io:format("connected user ~s~n", [User]);
        <<"UserNotFound">>->
            User=maps:get(<<"user">>, Map),
            io:format("user ~s not found~n", [User]);
        <<"NotConnected">>->
            User=maps:get(<<"user">>, Map),
            io:format("user ~s not connected~n", [User]);
        <<"subscribed">>->
            User=maps:get(<<"user">>, Map),
            ToUser=maps:get(<<"toUser">>, Map),
            io:format("user ~s subscribed to user ~s~n", [User, ToUser]);
        <<"unable to subscribe">>->
            User=maps:get(<<"user">>, Map),
            ToUser=maps:get(<<"toUser">>, Map),
            io:format("user ~s unable to subscribe to user ~s~n", [User, ToUser]);
        <<"sent">>->
            io:format("tweet sent~n");
        <<"got tweets">>->
            io:format("Got tweets:~n"),
            Tweets=maps:get(<<"tweets">>, Map),
            [printTweet(X) || X <- Tweets];
        <<"hash found">>->
            io:format("Got tweets:~n"),
            Tweets=maps:get(<<"tweets">>, Map),
            [printTweet(X) || X <- Tweets];
        <<"hash not found">>->
            io:format("Hash not found~n");
        <<"mention found">>->
            io:format("Got tweets:~n"),
            Tweets=maps:get(<<"tweets">>, Map),
            [printTweet(X) || X <- Tweets];
        <<"mention not found">>->
            io:format("mention not found~n");
        <<"new tweet">>->
            io:format("New tweets:~n"),
            printTweet(Map);
        _->io:format("~w", [Map])
    end.

printTweet(Map)->
    Tweet=maps:get(<<"tweet">>, Map),
    Id=maps:get(<<"tid">>, Map),
    Auth=maps:get(<<"from">>, Map),
    io:format("     Tweet: ~s~n", [Tweet]),
    io:format("     Id: ~w~n", [Id]),
    io:format("     Author: ~s~n", [Auth]).

