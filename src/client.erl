-module(client).
-compile(export_all).
-define(TwitPort, 8000).

%client needs two actors too - one for receive and one for sending
hello(Ip) ->
    {ok,Sock} = gen_tcp:connect(Ip,?TwitPort,[{active,false},{packet,2}]),
    loop(Sock),
    gen_tcp:close(Sock).

loop(Sock) ->
    {ok, [Command, Vars]}=io:fread("Next Command: ", "~s ~s"),
    case Command of 
        "search" -> 
            io:format("send search ~n"),
            gen_tcp:send(Sock, "SEARCH" ++ "fred");
        "register"-> 
            io:format("request registering ~s~n", [Vars]),
            gen_tcp:send(Sock, "REGISTER"++Vars);
        "tweet"->io:format("send tweet ~n");
        "print"->gen_tcp:send(Sock, "PRINT USERS")
    end,
    loop(Sock).
