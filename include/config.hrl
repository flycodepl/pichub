-define(DB, db_mnesia).

-define(OBJ, 
        [kwejk]).

-define(HTTP_PORT, 8080).

-define(DEBUG, fun(X, Y) ->
                       D = io_lib:fwrite(X,Y),
                       Time = utils:printable_date(),
                       io:fwrite("\t~s DEBUG [~p]: ~s~n", [Time, ?MODULE, lists:flatten(D)])
               end).


