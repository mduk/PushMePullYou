-module( pmpy_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_endpoint/1 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

start_endpoint( Id ) -> 
	ChildSpec = { Id, { pmpy_endpoint, start_link, [] }, permanent, 5000, worker, [ pmpy_endpoint ] },
	supervisor:start_child( ?MODULE, ChildSpec ).

init( _ ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.
