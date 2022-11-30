-module(twitter_tweets).
-behaviour(gen_server).
-export([start_link/0]).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2]).
-record(tweet, {id, author, text}).
-record(hash, {tag, ids=[]}).
-record(mention, {user, ids=[]}).
-record(state,
    {
        mentions=[] ::Record::#mention{},
        hashtags=[] ::Record::#hash{},
        tweets=[] ::Record::#tweet{}
    }).

%% START AND STOP

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).

init([]) ->
    io:format("tweets server started~n"),
    {ok, #state{mentions=[], hashtags=[], tweets=[]}}.

terminate(Reason, Args)->
    io:format("Terminating Twitter Users ~n").

%%%%%% API FUNCTIONS

%% HANDLE SYNCHRONOUS CALLS
handle_call({new_tweet, Tweet, Author}, _From, State=#state{tweets=TList})->
    Id = length(TList)+1,
    NewTweet = [#tweet{id=Id, text=Tweet, author=Author}],
    gen_server:cast(twitter_tweets, {process_hash, Tweet, Id}),
    gen_server:cast(twitter_tweets, {process_mention, Tweet, Id}),
    {reply, Id, State#state{tweets=TList++NewTweet}};

handle_call({get_tweet, Tid}, _From, State=#state{tweets=TList})->
    Found = [X || X <- TList, X#tweet.id == Tid],
    FoundTweet = lists:nth(1,Found),
    {reply, {FoundTweet#tweet.text, FoundTweet#tweet.author}, State};

handle_call({get_hash, Hash}, _From, State=#state{tweets=TList,hashtags=HashList})->
    io:format("Tweet State ~w~n", [State]),
    Found = [X || X <- HashList, X#hash.tag == Hash],
    case Found of
        []->{reply, notFound, State};
        _->
            FoundHash=lists:nth(1,Found),
            Tids=FoundHash#hash.ids,
            Tweets=[get_tweet_info(X, TList) || X <- Tids],
            Resp=[{X#tweet.id,X#tweet.author,X#tweet.text} || X <-Tweets],
            {reply, {found, Resp}, State}
    end;

handle_call({get_mention, User}, _From, State=#state{tweets=TList,mentions=MentList})->
    Found = [X || X <- MentList, X#mention.user == User],
    case Found of
        []->
            {reply, notFound, State};
        _->
            FoundMent=lists:nth(1,Found),
            Tids=FoundMent#mention.ids,
            Tweets=[get_tweet_info(X, TList) || X <- Tids],
            Resp=[{X#tweet.id,X#tweet.author,X#tweet.text} || X <-Tweets],
            {reply, {found, Resp}, State}
    end;

handle_call({get_tweets, Tids}, _From, State=#state{tweets=TList})->
    Tweets = [get_tweet_info(X, TList) || X <- Tids],
    case Tweets of
        []->{reply, notFound, State};
        _->
            Resp=[{X#tweet.id,X#tweet.author,X#tweet.text} || X <-Tweets],
            {reply, {found, Resp}, State}
    end.

handle_cast({process_hash, Tweet, Id}, State=#state{hashtags=HashList})->
    Hash = get_hash(Tweet),
    case Hash of
        noHash->
            {noreply, State};
        _->
            Found = [X || X <- HashList, X#hash.tag == Hash],
            case Found of
                []->
                    NewHash = [#hash{tag=Hash, ids=[Id]}],
                    {noreply, State#state{hashtags=HashList++NewHash}};
                _->
                    FoundHash = lists:nth(1,Found),
                    PrevIdList = FoundHash#hash.ids,
                    Update = [FoundHash#hash{ids=PrevIdList++[Id]}],
                    OldList = lists:delete(FoundHash, HashList),
                    {noreply, State#state{hashtags=OldList++Update}}
            end
    end;

handle_cast({process_mention, Tweet, Id}, State=#state{mentions=MentList})->
    Mention = get_mention(Tweet),
    case Mention of
        noMention->{noreply, State};
        _->
            User=string:slice(Mention,1),
            Found = [X || X <- MentList, X#mention.user == User],
            case Found of
                []->
                    NewMent = [#mention{user=User, ids=[Id]}],
                    {noreply, State#state{mentions=MentList++NewMent}};
                _->
                    FoundMent = lists:nth(1,Found),
                    PrevIdList = FoundMent#mention.ids,
                    Update = [FoundMent#mention{ids=PrevIdList++[Id]}],
                    OldList = lists:delete(FoundMent, MentList),
                    {noreply, State#state{mentions=OldList++Update}}
            end
    end.

get_hash(Tweet)->
    A = string:find(Tweet, "#", trailing),
    case A of
        nomatch->noHash;
        _->lists:nth(1,string:split(A, " "))
    end.

get_mention(Tweet)->
    A = string:find(Tweet, "@", trailing),
    case A of
        nomatch->noMention;
        _->lists:nth(1,string:split(A, " "))
    end.

get_tweet_info(Tid, TList)->
    lists:nth(1,[X || X <- TList, X#tweet.id == Tid]).
