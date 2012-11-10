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

% Get the pid of an endpoint. 
% If the endpoint doesn't exist, it'll be started.
endpoint( Id ) ->
	case ets:lookup( pmpy_endpoints, Id ) of
		[] -> 
			{ ok, Pid } = pmpy_sup:start_endpoint( Id ),
			ets:insert( pmpy_endpoints, { Id, Pid } ),
			{ ok, Pid };
			
		[ { Id, Pid } ] -> 
			{ ok, Pid }
	end.

% Subscribe to an endpoint
subscribe( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	gen_event:add_handler( Pid, pmpy_subscribe, self() ).

% Unsubscribe from an endpoint
unsubscribe( Id ) ->
	{ ok, Pid } = endpoint( Id ),
	gen_event:remove_handler( Pid, pmpy_subscribe, self() ).

% Send a message to an endpoint, the message gets relayed to all the subscriber processes
notify( Id, Msg ) ->
	{ ok, Pid } = endpoint( Id ),
	gen_event:notify( Pid, Msg ).