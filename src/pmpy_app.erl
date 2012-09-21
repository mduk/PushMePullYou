-module( pmpy_app ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start( _StartType, _StartArgs ) ->
	start_cowboy(),
	ets:new( pmpy_endpoints, [ public, named_table ] ),
	pmpy_sup:start_link().

stop( _State ) ->
	ok.

start_cowboy() ->
	HttpDispatchRules = [
		{ '_', [ % Any Host
			{ '_', pmpy_wshandler, [] } 
		] }
	],

	cowboy:start_listener( http, 10, 
		cowboy_tcp_transport, [ { port, 8000 } ],
		cowboy_http_protocol, [ { dispatch, HttpDispatchRules } ] 
	).