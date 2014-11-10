%% CS 511 
%% Assignment 5
%% Suketu Shah and Michael Peleshenko
%% sensor.erl
-module(sensor).
-export([ sensor_run()/2 ]).

sensor_run(Watcher_pid, Sensor_id) ->
	random:seed(now()) ,
	Sleep_time = random:uniform(10000) ,
	timer:sleep(Sleep_time) ,
	Measurement = random:uniform(11) ,
	case Measurement of
		11 ->
			exit(Watcher_pid, 'anomalous_reading') ;
		_ ->
			Watcher_pid ! {Sensor_id, Measurement}
	end ,
	sensor_run(Watcher_pid, Sensor_id) .