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

## Twitter Client
It consists of a single supervisor monitoring the client process.

# Server

# Client
TODO

# Running
TODO

# Results
TODO
