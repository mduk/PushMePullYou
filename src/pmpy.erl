-module( pmpy ).

-export( [ start/0, stop/0, endpoint/1, subscribe/1, unsubscribe/1, notify/2 ] ).

start() ->
	application:start( sasl ),
	application:start( cowboy ),
	application:start( pmpy ).

stop() ->
	application:stop( pmpy ),
	application:stop( cowboy ),
	application:stop( sasl ).

endpoint( Id ) ->
	case ets:lookup( pmpy_endpoints, Id ) of
		[] -> 
			{ ok, Pid } = pmpy_sup:start_endpoint( Id ),
			gen_event:add_handler( Pid, echohandler, [ "Endpoint ", Id, " received" ] ),
			ets:insert( pmpy_endpoints, { Id, Pid } ),
			{ ok, Pid };
			
		[ { Id, Pid } ] -> 
			{ ok, Pid }
	end.

subscribe( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	gen_event:add_handler( Pid, pmpy_subscribe, self() ).

unsubscribe( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	gen_event:remove_handler( Pid, pmpy_subscribe, self() ).

notify( Id, Msg ) ->
	{ ok, Pid } = endpoint( Id ),
	gen_event:notify( Pid, Msg ).