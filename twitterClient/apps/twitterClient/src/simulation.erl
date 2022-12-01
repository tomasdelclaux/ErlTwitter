-module(simulation).
-compile(export_all).

%%SIMULATION PARAMETERS
-define(MANYSUBS, 40).
-define(PERIODS, [[5,-1,15, -2,10], [-2,13,-3,8,10], [4,-1,1,-8,15],[2,-1,7,-3,17],[1,15,-4,11,-1]]).
-define(NUM, 5).
-record(state,{id, periods=[], subscribers, period, time, disc}).
-record(tracker, {tweets, retweets, users, connecs, time}).

start(NumNodes)->
    start_clients(NumNodes, 1, NumNodes).

start_clients(0,_,_)->
    ok;

start_clients(NumNodes, I, TotalNodes)->
    if
        NumNodes==TotalNodes->startTrack();
        true->ok
    end,
    Periods=lists:nth(rand:uniform(?NUM), ?PERIODS),
    State=#state{id=NumNodes, periods=Periods, subscribers=round(TotalNodes/I), period=1, time=0, disc=true},
    start(NumNodes, State),
    start_clients(NumNodes-1, I+1, TotalNodes).

start(Id, State)->
    timer:sleep(100),
    spawn(fun()->twitterClient_client:start_link(integer_to_list(Id), 8000, "127.0.0.1", true) end),
    timer:sleep(100),
    spawn(fun()->client(Id, State)end).

client(Id, State)->
    gen_server:cast(pid(Id), {register, integer_to_list(Id)}),
    whereis(twitter_tracker) ! {regUser},
    timer:sleep(30000),
    get_subscribers(State),
    timer:sleep(2000),
    simulate(State),
    {ok, State}.

pid(Id)->
    list_to_atom("sim_client_"++integer_to_list(Id)).

get_subscribers(_State=#state{id=Id, subscribers=Subs})->
    [gen_server:cast(pid(Id), {subscribe, integer_to_list(X)})|| X <- lists:seq(1,Subs)].

simulate(State)->
    Id = State#state.id,
    Subs = State#state.subscribers,
    Disc = State#state.disc,
    Interval = lists:nth(State#state.period, State#state.periods)-State#state.time,
    if
        Interval < 0 ->
            case Disc of
                false->gen_server:cast(pid(Id), {disconnect});
                true->ok
            end,
            timer:sleep(Interval*60*1000*-1),
            P = State#state.period+1,
            case P of
                ?NUM->ok;
                _->simulate(State#state{period=P, disc=true})
            end;
        Interval > 0 ->
            gen_server:cast(pid(Id), {connect, integer_to_list(Id)}),
            whereis(twitter_tracker) ! {connect},
            if 
                Subs > ?MANYSUBS ->
                    gen_server:cast(pid(Id), {tweet, "Testing twitter"}),
                    whereis(twitter_tracker) ! {tweets},
                    timer:sleep(10000),
                    gen_server:cast(pid(Id), {tweet, "I am popular"}),
                    whereis(twitter_tracker) ! {tweets},
                    timer:sleep(10000),
                    gen_server:cast(pid(Id), {tweet, "I am very popular"}),
                    whereis(twitter_tracker) ! {tweets},
                    Time = State#state.time + 0.5,
                    simulate(State#state{time=Time, disc=false});
                true->
                    gen_server:cast(pid(Id), {tweet, "Testing twitter"}),
                    whereis(twitter_tracker) ! {tweets},
                    timer:sleep(30000),
                    Time = State#state.time + 0.5,
                    simulate(State#state{time=Time, disc=false})
            end;
        true->ok
    end.              
                    
%% TRACKER

startTrack()->
    State= #tracker{tweets=0, retweets=0, users=0, connecs=0, time=erlang:system_time()},
    ActorPid = spawn_link(?MODULE, track, [State]),
    register(list_to_atom("twitter_tracker"), ActorPid).

track(State=#tracker{tweets=T, retweets=R, users=U, connecs=C, time=Time})->
    receive
        {regUser}->track(State#tracker{users=U+1});
        {tweets}->track(State#tracker{tweets=T+1});
        {retweet}->track(State#tracker{retweets=R+1});
        {connect}->track(State#tracker{connecs=C+1});
        {finish}->
            Elapsed = (erlang:system_time()-Time)/1000/1000/1000/60,
            io:format("Users: ~w~nTweets: ~w~nReTweets: ~w~nConnections: ~w~nSimulation Time(mins): ~w~n", [U,T,R,C,Elapsed])
    end.

stop()->
    whereis(twitter_tracker) ! {finish},
    timer:sleep(30),
    exit(self(),kill).