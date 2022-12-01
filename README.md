# twitter
Erlang twitter engine, server and client

# Introduction
The project consists of a twitter clone in erlang. There is a server to process requests and a client to send them.

The server supports:
- Registering new users
- Connecting users that have been previously registered
- Subscribing to users
- Sending tweets to subscribed users
- Retweeting
- Querying for received tweets
- Querying for tweets containing a given hashtag #
- Querying for tweets containing a given user mention @

# Architecture
Both the twitter client and server are OTP applications and follow OTP supervision tree structure.

## Twitter Server
It consits of three supervisors:
- A supervisor for the actor in charge of user data
- A supervisor for the actro in chager of tweets data
- A supervisor for the actors accepting tcp connections from clients

The OTP tree for this application can be seen in the screenshot below:

![Alt text](./img/architectute.png "Twitter Server")

## Twitter Client
It consists of a single supervisor monitoring the client process.

![Alt text](./img/client.png "Twitter Client")

# Running the Server
The server can be run using rebar3. For information on rebar3:
https://github.com/erlang/rebar3

The following commands are used for compiling and running the server:

```
example@example-Pro ErlTwitter % cd twitter
example@example-Pro twitter % ../rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling twitter
Erlang/OTP 25 [erts-13.1.1] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]

Eshell V13.1.1  (abort with ^G)
1> users server started
tweets server started
Twitter server started
Twitter server started
Twitter server started
Twitter server started
Twitter server started
```

# Running the Client
The client can be run as a standalone or application. Additionally the client
can also execute the simulation program to start many client processes which connect to the server.

## Standalone application


## Simulation


# Results

## Performance
```
2> simulation:stop().
Users: 400
Tweets: 4194
ReTweets: 253
Connections: 4066
Simulation Time(mins): 16.91827317285
** exception exit: killed
```

## Fault Tolerance
Todo