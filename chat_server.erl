-module(chat_server).
-export([loop/1, connect/2, disconnect/2, send/3, create/0]).

loop(Users) ->
	receive
		{connect, User} -> 
			case lists:member(User, Users) of
				true -> 
					io:format("User '~s' already connected~n", [User]), 
					loop(Users);
				false -> 
					io:format("User '~s' connected~n", [User]),
					loop([User|Users])
			end;
		{disconnect, User} ->
			case lists:member(User, Users) of
				true -> 
					NewUsers = lists:delete(User, Users),
					io:format("User '~s' disconnected~n", [User]),
						loop(NewUsers);
				false -> 
					io:format("User '~s' not connected~n", [User]),
					loop(Users)
			end;
		{msg, User, Msg} -> 
			case lists:member(User, Users) of
				true -> 
					io:format("~s:~s~n", [User, Msg]),
					loop(Users);
				false -> 
					loop(Users)
			end;
		{close} -> io:format("Server stopped~n", [])
	end.

create() ->
	spawn(chat_server, loop, [[]]).

connect(User, Server) -> 
	Server ! {connect, User}.

disconnect(User, Server) ->
	Server ! {disconnect, User}.

send(Msg, User, Server) ->
	Server ! {msg, User, Msg}.