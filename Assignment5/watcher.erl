%% CS 511 
%% Assignment 5
%% Suketu Shah and Michael Peleshenko
%% watcher.erl
-module(watcher) .
-export([ watcher_start/0, watcher_run/2, watcher_spawn/2, watcher_receive/1 ]) .

watcher_start() ->
	{ok, [ Sensors ]} = io:fread("enter number of sensors to start> ", "~d") ,
	watcher_spawn(Sensors, 0) .

%% Function to spawn Sensors sensors
%% the Watcher_group represents that starting sensor ID for a given watcher
%% i.e. 0 -> 0, 1 -> 10, 2 -> 20, ..., 10 -> 100
watcher_spawn(Sensors, Watcher_group) ->
	if
		(Sensors =< 10) and (Sensors > 0) ->
			_Pid = spawn(?MODULE, watcher_run, [ Watcher_group, Sensors ]) ;
		Sensors > 10 ->
			_Pid = spawn(?MODULE, watcher_run, [ Watcher_group, 10 ]) ,
			watcher_spawn(Sensors - 10, Watcher_group + 1) ;
		true ->
			timer:sleep(10000)
	end .

watcher_run(Watcher_group, Count) ->
	%% Spawn Count sensors
	Sensors = sensor_spawn(Watcher_group, Count, 0, []) ,
	io:fwrite("STARTED: Watcher started with sensors: ~n~p~n", [Sensors]) ,
	watcher_receive(Sensors) .

sensor_spawn(Watcher_group, Count, Start_id, Sensors_list) ->
	if
		Count == 0 ->
			Sensors_list ;
		Count > 0 ->
			Sensor_id = Watcher_group * 10 + Start_id ,
			{ Pid, _ } = spawn_monitor(sensor, sensor_run, [self(), Sensor_id]) ,
			%% Append {Sensor_id, PID} tuple to list of sensors
			New_sensors_list = lists:keystore(Pid, 2, Sensors_list, {Sensor_id, Pid}) ,
			sensor_spawn(Watcher_group, Count - 1, Start_id + 1, New_sensors_list) ;
		true ->
			io:fwrite("Invalid count~n", [])
	end .
	
watcher_receive(Sensors) ->
	receive
		{Sensor_id, Measurement} ->
			io:fwrite("RECEIVED: measurement ~p from sensor ~p~n", [Measurement, Sensor_id]) ;
		{'DOWN', _Ref, process, Pid, Reason} ->
			%% Get Sensor_id from list
			Sensor_id = lists:keyfind(Pid, 2, Sensors) ,
			if
				Sensor_id == false ->
					io:fwrite("Could not find sensor PID ~p in sensor list~n", [ Pid ]) ;
				true ->
					io:fwrite("DIED: Sensor ~p died with reason ~p~n", [ Sensor_id, Reason ]) ,
					%% Restart sensor
					{ Pid, _ } = spawn_monitor(sensor, sensor_run, [self(), Sensor_id]) ,
					New_sensors = lists:keyreplace(Pid, 2, Sensors, {Sensor_id, Pid}) ,
					io:fwrite("RESTARTED: A sensor was restarted, updated list of sensors: ~n~p~n", [Sensors]) ,
					watcher_receive(New_sensors)
			end ;
		_ ->
			io:fwrite("Received unknown message~n")
	end .
			