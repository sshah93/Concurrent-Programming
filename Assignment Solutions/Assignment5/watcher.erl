-module(watcher).
-export([setup/0, watch/2]).


%%  start one (monitored) sensor
%%  input: ID -- ID of sensor to be started
%%  output: tuple { ID, Pid } -- tuple with sensor ID and Pid of new process
start_monitored_sensor(ID) ->
    { Pid, _ } = spawn_monitor(sensor, run, [ID, self()]) ,
    { ID, Pid } .


%%  start all sensors numbered consecutively from Bottom to Top
%%  input: Top -- highest number sensor to be started
%%  input: Bottom -- lowest number sensor to be started
%%  input: List_of_sensors -- current list of sensors; list items are { ID, Pid } tuples
%%  output: updated list of sensors
start_sensors(Top, Bottom, List_of_sensors) ->
    New_sensor = start_monitored_sensor(Bottom) ,
    if Bottom < Top ->
        start_sensors(Top, Bottom+1, [ New_sensor | List_of_sensors ]) ;
    true ->
        [ New_sensor | List_of_sensors ]
    end .


%%  given Pid of crashed sensor, search List and return sensor's ID
%%  input: Pid of sensor that crashed
%%  output: ID of tuple {ID, Pid} in List
crashed_ID(Pid, List) ->
    [ Head | Rest ] = List ,            % get 1st list item in Head
    { Head_ID, Head_PID } = Head ,      % get ID of that item
    if Head_PID == Pid ->               % if Head is the right item, return ID
        Head_ID ;
    true ->                             % otherwise, search the rest of the list
        crashed_ID(Pid, Rest)
    end .


%%  continually report readings of all sensors in Sensor_list; if any crashes, restart it
%%  input: list of sensors
%%  output: none (except print statements)
get_readings(Sensor_list) ->
    receive
        { Sensor_ID, Reading } ->
            io:fwrite("sensor number ~p sent reading ~p~n", [ Sensor_ID, Reading ]) ,
            Arg = Sensor_list ;
        { 'DOWN', _, process, Sender_PID, Reason } ->
            io:fwrite("sensor died: PID ~p, reason ~p; restarting ...~n", [ Sender_PID, Reason ]) ,
            Crashed_ID = crashed_ID(Sender_PID, Sensor_list) ,
            Still_alive = lists:delete({Crashed_ID, Sender_PID}, Sensor_list) , 
            Restarted = start_monitored_sensor(Crashed_ID) ,
            Arg = [ Restarted | Still_alive ] ,
            io:fwrite("updated list of sensors: ~p~n", [ Arg ])
    end ,
    get_readings(Arg) .


%%  watch all sensors numbered Bottom to Top
%%  input: Top -- highest number sensor to be started
%%  input: Bottom -- lowest number sensor to be started
%%  output: none
watch(Top, Bottom) ->
    Initial_sensor_list = start_sensors(Top, Bottom, []) ,
    io:fwrite("watch(~p, ~p): initial sensor list = ~p~n", [Top, Bottom, Initial_sensor_list]) ,
    get_readings(Initial_sensor_list) .


%%  from high numbers down to low numbers, start watcher processes to
%%      watch up to 10 sensors per watcher
%%  input: N -- highest numbered sensor to be started
%%  input: Num_watcher -- number of watcher to be started
%%  output: ok
setup_loop(N, Num_watcher) ->                         % e.g., if N=93 then Num_watcher=10
    Top = N ,
    Bottom = N - (N rem 10) ,                         % if N=93 then Top=93 and Bottom=90
    Pid = spawn(?MODULE, watch, [Top, Bottom]) ,      % ?MODULE means this module
    if Num_watcher > 1 ->
        setup_loop(Bottom-1, Num_watcher-1) ;         % if N=93 then args to recursive call will be 89, 9
    true ->
        ok 
    end .


%%  choose a random number of sensors, N, from 1 to 100
%%  then start watcher processes
%%  each watcher gets its own range of up to 10 sensors
%%  input: none
%%  output: none
setup() ->
    {ok, [ Range ]} = io:fread("number of sensors will be random, chosen from a range; enter limit of the range> ", "~d") ,
    if Range =< 1 ->
        io:fwrite("setup: range must be at least 2~n", []) ;
    true ->
        random:seed(now()) ,
        Rand_1toRange = random:uniform(Range) ,
        io:fwrite("setup: there will be ~p sensors~n", [ Rand_1toRange ]) ,
        N = Rand_1toRange - 1 ,              % e.g., if Rand_1toRange=3 then N=2 and sensors are numbered 0-2
        Num_watchers = 1 + (N div 10) ,
        setup_loop(N, Num_watchers) 
    end .
