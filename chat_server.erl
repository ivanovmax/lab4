-module(chat_server).
-export([loop/1, connect/2, disconnect/2, send/3, create/0]).

loop(Users) ->
	receive
		{connect, {User, UserPid}} -> 
			case lists:keymember(User, 1, Users) of
				true -> 
					io:format("User '~s' already connected~n", [User]), 
					loop(Users);
				false -> 
					io:format("User '~s' connected~n", [User]),
					loop([{User, UserPid}|Users])
			end;
		{disconnect, {User, UserPid}} ->
			case lists:member({User, UserPid}, Users) of
				true ->
					UserPid ! {stop, User},
					NewUsers = lists:keydelete(User, 1, Users),
					io:format("User '~s' disconnected~n", [User]),
					loop(NewUsers);
				false -> 
					io:format("User '~s' not connected~n", [User]),
					loop(Users)
			end;
		{msg, User, Msg} -> 
			case lists:keymember(User, 1, Users) of
				true -> 
					send_msg(Msg, User, Users),
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

send_msg(Msg, Src, []) -> io:format("Server get '~s' from '~s'~n",[Msg, Src]);
send_msg(Msg, Src, [{Src, _}|Other]) -> send_msg(Msg, Src, Other);
send_msg(Msg, Src, [{User,UserPid}|Other]) ->
	UserPid ! {msg, Src, User, Msg},
	send_msg(Msg, Src, Other).
