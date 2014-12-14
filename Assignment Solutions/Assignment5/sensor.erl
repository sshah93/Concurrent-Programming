-module(sensor).
-export([run/2]).


run(ID, Parent) ->
%%  get random int 1-11
    random:seed(now()) ,
    Measurement = random:uniform(11) ,
%%  sleep for random time 0-10 seconds
    Sleep_time = random:uniform(10000) ,
    timer:sleep(Sleep_time) ,
%%  crash with probability 1/11; otherwise send measurement back to parent
    if Measurement == 11 ->
        exit(anomalous_reading) ;
    true ->
	Parent ! { ID, Measurement } ,
        run(ID, Parent)
    end .
