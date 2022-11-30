-module(twitterClient_client).
-compile(export_all).
-compile(gen_server).
-record(state, {username,socket}).

%client needs two actors too - one for receive and one for sending
start_link(Username, Port, Ip)->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Username, Port,Ip},[]).

init({Username, Port,Ip})->
    io:format("starting client connection...~n"),
    {ok,Sock} = gen_tcp:connect(Ip,8000,[{active,once},{packet,2}]),
    {ok, #state{username=Username, socket=Sock}}.

terminate(Reason, Args)->
    io:format("Client Conection Closed...~n").

%%%%% HANDLE CLIENT CALLS

%%%Register a new user
handle_cast({register, User}, State=#state{socket=Sock})->
    io:format("Sending User ~s to be registered ~n", [User]),
    gen_tcp:send(Sock, "REGISTER"++User),
    {noreply, State#state{socket=Sock}};

handle_cast({connect, User}, State=#state{socket=Sock})->
    gen_tcp:send(Sock, "CONNECT" ++ User),
    {noreply, State#state{socket=Sock}};

handle_cast({subscribe, ToUser}, State=#state{socket=Sock, username=User})->
    gen_tcp:send(Sock, "SUBSCRIBE"++User++"|"++ToUser),
    {noreply, State};

handle_cast({tweet, Msg}, State=#state{socket=Sock,username=User})->
    gen_tcp:send(Sock, "TWEET"++User++"|"++Msg),
    {noreply, State};

handle_cast({retweet, Tid}, State=#state{socket=Sock, username=User})->
    gen_tcp:send(Sock, "RETWEET"++User++"|"++integer_to_list(Tid)),
    {noreply, State};

handle_cast({get_my_tweets}, State=#state{socket=Sock, username=User})->
    gen_tcp:send(Sock, "GETTWEETS"++User),
    {noreply, State};

handle_cast({get_hash, Hash}, State=#state{socket=Sock})->
    gen_tcp:send(Sock, "GETHASH"++Hash),
    {noreply, State};

handle_cast({get_mention}, State=#state{socket=Sock, username=User})->
    gen_tcp:send(Sock, "GETMENT"++User),
    {noreply, State}.

%%%%Process Server Responses
handle_info({tcp, Sock, "UserRegistered"++User}, State)->
    inet:setopts(Sock,[{active,once}]),
    io:format("New User ~s registered~n", [User]),
    {noreply, State#state{username=User}};

handle_info({tcp, Sock, "NotRegistered"}, State)->
    inet:setopts(Sock,[{active,once}]),
    io:format("Unable to Register~n"),
    {noreply, State};

handle_info({tcp, Sock, "Connected"++User}, State)->
    inet:setopts(Sock,[{active,once}]),
    io:format("~s connected~n", [User]),
    {noreply, State#state{username=User, socket=Sock}};

handle_info({tcp, Sock, "UserNotFound"++User}, State)->
    inet:setopts(Sock,[{active,once}]),
    io:format("~s not registered~n", [User]),
    {noreply, State};

handle_info({tcp, Sock, "NotConnected"++User}, State)->
    inet:setopts(Sock,[{active,once}]),
    io:format("~s not connected~n", [User]),
    {noreply, State};

handle_info({tcp, Sock, "Subscribed"++Vars}, State)->
    inet:setopts(Sock,[{active,once}]),
    [User,ToUser]=string:tokens(Vars, "|"),
    io:format("~s subscribed to ~s~n", [User, ToUser]),
    {noreply, State};

handle_info({tcp, Sock, "Not Subscribed"++Vars}, State)->
    inet:setopts(Sock,[{active,once}]),
    [User,ToUser]=string:tokens(Vars, "|"),
    io:format("~s unable to subscribe to ~s~n", [User, ToUser]),
    {noreply, State};

handle_info({tcp, Sock, "TWEET: "++Vars}, State) ->
    inet:setopts(Sock,[{active,once}]),
    [FromU, Tid, Tweet]=string:tokens(Vars, "|"),
    io:format("Tweet: ~s~nFrom: ~s~nText: ~s~n", [Tid,FromU,Tweet]),
    {noreply, State};

handle_info({tcp, Sock, "notFound query"}, State)->
    inet:setopts(Sock,[{active,once}]),
    io:format("tweets not found for this query~n"),
    {noreply, State};

%%DISCONNECTS

%% DISCONNECTS
handle_info({tcp, Socket, "DISCONNECT"}, State) ->
  gen_tcp:close(Socket),
  {stop, normal, State};

handle_info({tcp_closed, _Socket}, State) -> {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) -> {stop, normal, State}.



