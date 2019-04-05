-module(spawntest).
-export([priorities/0,start/0]).
 
priorities() ->
    receive
        a ->
        io:fwrite("Got 1"),
        priorities()


    after 0 ->
        receive 
            a -> 
                io:fwrite("Got 2"),
                priorities();

            b -> 
                io:fwrite("Got 3"),
                priorities()

        end
end.

start() ->
        Pid = spawn(?MODULE, priorities,[]),
        Pid ! a,
        Pid ! b,
        Pid ! a,
        Pid ! b,
        Pid ! a,
        Pid ! b.




