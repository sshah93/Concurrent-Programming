%%CS511 Erlang Lab

-module(lab).
-export([start/0, arith/2]).

arith(X, Y) -> io:fwrite("Arguments: ~p ~p~n", [X, Y]),
	Sum = X + Y,
	io:fwrite("newThread: Sum = ~p", [Sum]).

start() ->
	{ok, [X1, X2]} = io:fread("first_thread: enter two integers please> ", "~p~p"),
	spawn(newThread, arith, [X1, X2]),
	halt().