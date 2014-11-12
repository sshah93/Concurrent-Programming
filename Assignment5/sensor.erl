%% CS 511 
%% Assignment 5
%% Suketu Shah and Michael Peleshenko
%% sensor.erl
-module(sensor).
-export([ sensor_run/2 ]).

%% Function to start sensor
%% Watcher_pid - PID of watcher
%% Sensor_id - ID of sensor
sensor_run(Watcher_pid, Sensor_id) ->
	%% Set random seed
	random:seed(now()) ,
	%% Start measuring
	sensor_measure(Watcher_pid, Sensor_id) .

%% Function to send measurements to watcher
%% Watcher_pid - PID of watcher
%% Sensor_id - ID of sensor
sensor_measure(Watcher_pid, Sensor_id) ->
	%% Get random sleep time
	Sleep_time = random:uniform(10000) ,
	%% Sleep
	timer:sleep(Sleep_time) ,
	%% Generate random measurement
	Measurement = random:uniform(11) ,
	if
		Measurement == 11 -> %% Crash if measurement reading is 11
			exit('anomalous_reading') ;
		true -> %% Otherwise, send measurement to watcher
			Watcher_pid ! {Sensor_id, Measurement}
	end ,
	sensor_run(Watcher_pid, Sensor_id) .