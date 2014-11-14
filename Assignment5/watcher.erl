%% CS 511 
%% Assignment 5
%% Suketu Shah and Michael Peleshenko
%% watcher.erl
-module(watcher) .
-export([ watcher_start/0, watcher_run/2 ]) .

%% Function to get number of sensors to start from user and start watchers for them
watcher_start() ->
	%% Get number of sensors to create from user
	{ok, [ Sensors ]} = io:fread("enter number of sensors to start> ", "~d") ,
	%% Spawn watcher threads
	watcher_spawn(Sensors, 1) .

%% Function to spawn Sensors sensors
%% Sensors - number of sensors to spawn
%% Watcher_group - represents the ten's place of the sensor ID for a given watcher
%% i.e. 0 -> 0, 1 -> 10, 2 -> 20, ..., 10 -> 100
watcher_spawn(Sensors, Watcher_group) ->
	if
		(Sensors =< 10) and (Sensors > 0) ->
			% main watcher spawner thread becomes the watcher for group 0
			watcher_run(0, Sensors) ;
		Sensors > 10 ->
			_Pid = spawn(?MODULE, watcher_run, [ Watcher_group, 10 ]) ,
			watcher_spawn(Sensors - 10, Watcher_group + 1) ;
		true ->
			io:fwrite("No watchers to spawn~n")
			
	end .

%% Function to start sensons and receiving messages
%% Watcher_group - represents the ten's place of the sensor ID for a given watcher
%% Count - number of sensors to spawn
watcher_run(Watcher_group, Count) ->
	%% Spawn sensors for given watcher thread
	Sensors = sensor_spawn(Watcher_group, Count, 0, []) ,
	io:fwrite("STARTED: Watcher started with sensors: ~p~n", [ Sensors ]) ,
	%% start processing received messages
	watcher_receive(Sensors) .

%% Function to spawn sensors
%% Watcher_group - represents the ten's place of the sensor ID for a given watcher
%% Count - number of sensors to spawn
%% Start_id - number [0-9] that represents one's place in sensor ID
%% Sensors_list - list of {Sensor ID, PID} for sensors
sensor_spawn(Watcher_group, Count, Start_id, Sensors_list) ->
	if
		Count == 0 -> %% Return sensor list when done spawning sensors
			Sensors_list ;
		Count > 0 -> %% Spawn sensor
			%% Calculate sensor ID
			Sensor_id = Watcher_group * 10 + Start_id ,
			%% Spawn sensor with a monitor
			{ Pid, _ } = spawn_monitor(sensor, sensor_run, [self(), Sensor_id]) ,
			%% Add {Sensor ID, PID} tuple to list of sensors
			New_sensors_list = lists:keystore(Sensor_id, 1, Sensors_list, {Sensor_id, Pid}) ,
			sensor_spawn(Watcher_group, Count - 1, Start_id + 1, New_sensors_list) ;
		true ->
			io:fwrite("Invalid count~n")
	end .

%% Function to receive measurements from sensors
%% Sensors - list of sensors
watcher_receive(Sensors) ->
	receive
		{Sensor_id, Measurement} -> %% Handle proper measurement from sensor
			io:fwrite("RECEIVED: measurement ~p from sensor ~p~n", [Measurement, Sensor_id]) ,
			watcher_receive(Sensors) ;
		{'DOWN', _Ref, process, Pid, Reason} -> %% Handle anomalous reading from sensor
			%% Get Sensor_id from list
			{Sensor_id, _Pid} = lists:keyfind(Pid, 2, Sensors) ,
			if
				Sensor_id == false ->
					io:fwrite("Could not find sensor PID ~p in sensor list~n", [ Pid ]) ;
				true ->
					io:fwrite("DIED: Sensor ~p died with reason ~p~n", [ Sensor_id, Reason ]) ,
					%% Spawn new sensor
					{ New_pid, _ } = spawn_monitor(sensor, sensor_run, [ self(), Sensor_id ]) ,
					%% Update list of sensors with new PID
					New_sensors = lists:keyreplace(Sensor_id, 1, Sensors, {Sensor_id, New_pid}) ,
					io:fwrite("RESTARTED: A sensor was restarted, updated list of sensors: ~p~n", [ New_sensors ]) ,
					watcher_receive(New_sensors)
			end ;
		_ ->
			io:fwrite("Received unknown message~n")
	end .