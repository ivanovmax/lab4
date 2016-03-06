-module(chat_client).
-export([connect/2, loop/0]).

loop() ->
	receive
		{msg, Src, Self, Msg} -> io:format("'~s' get from '~s': ~s~n", [Self, Src, Msg]), loop();
		{stop, User, Server} -> disconnect(User, Server), io:format("User ~s stopped~n", [User])
	end.

connect(User, Server) ->
	UserPid = spawn(chat_client, loop, []),
	Server ! {connect, {User,UserPid}},
	UserPid.

disconnect(User, Server) ->
	Server ! {disconnect, {User, self()}}.