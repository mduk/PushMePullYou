-module( pmpy_wshandler ).

% This module mediates between the Websocket connection
% and the client processes. It handles translating the
% Json messages into client calls and vice versa.

-behaviour( cowboy_http_handler ).
-export( [ init/3, handle/2, terminate/2 ] ).

-behaviour( cowboy_http_websocket_handler ).
-export( [ websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3 ] ).

-record( state, {} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: cowboy_http_handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ==============================================================================
% init/3
% ==============================================================================
init( { _Any, http }, Req, [] ) ->
	case cowboy_http_req:header( 'Upgrade', Req ) of
		{ undefined, Req2 }        -> { ok, Req2, undefined };
		{ <<"websocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket };
		{ <<"WebSocket">>, _Req2 } -> { upgrade, protocol, cowboy_http_websocket }
	end.

% ==============================================================================
% handle/2
% ==============================================================================
handle( Req, State ) ->
	{ Method, _ } = cowboy_http_req:method( Req ),
	{ FullPath, _ } = cowboy_http_req:raw_path( Req ),
	case binary:split( FullPath, <<".">> ) of
		[ Path ] -> 
			serve( Method, Path, undefined, Req, State );
		[ Path, Extension ] ->
			_ = { ico, subscribe, unsubscribe }, % Just defining some atoms
			serve( Method, Path, binary_to_existing_atom( Extension, utf8 ), Req, State )
	end.

% ==============================================================================
% terminate/2
% ==============================================================================
terminate( _, _ ) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Behaviour: cowboy_http_websocket_handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ==============================================================================
% Initialise websocket handler
% ==============================================================================
websocket_init( _, Req, [] ) ->
	process_flag( trap_exit, true ),
	{ Path, _ } = cowboy_http_req:raw_path( Req ),
	pmpy:subscribe( Path ),
	{ ok, cowboy_http_req:compact( Req ), #state{}, hibernate }.

% ==============================================================================
% Received a message over the websocket
% ==============================================================================
websocket_handle( { text, Msg }, Req, State ) ->
	{ Path, _ } = cowboy_http_req:raw_path( Req ),
	Decoded = mochijson2:decode( Msg ),
	pmpy:notify( Path, Decoded ),
	{ ok, Req, State, hibernate };
% ==============================================================================
% Catch all websocket messages
% ==============================================================================
websocket_handle( _, Req, S ) ->
	{ ok, Req, S }.

%===============================================================================
% websocket_info/3
%===============================================================================
% Catch all messages
%-------------------------------------------------------------------------------
websocket_info( Msg, Req, State ) ->
	{ reply, { text, mochijson2:encode( Msg ) }, Req, State, hibernate }.

%===============================================================================
% websocket_terminate/3
%===============================================================================
% Connection closed
%-------------------------------------------------------------------------------
websocket_terminate( _Reason, Req, _State ) ->
	{ Path, _ } = cowboy_http_req:raw_path( Req ),
	pmpy:unsubscribe( Path ),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% serve/5
%===============================================================================
% Push a message over http to an endpoint
%-------------------------------------------------------------------------------
serve( 'POST', Endpoint, undefined, Req, State ) ->
	{ ok, Body, _ } = cowboy_http_req:body( Req ),
	pmpy:notify( Endpoint, Body ),
	cowboy_http_req:reply( 200, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"ok">>, Req ),
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% Get the last message sent to an endpoint
%-------------------------------------------------------------------------------
serve( 'GET', Endpoint, undefined, Req, State ) ->
	cowboy_http_req:reply( 501, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"501 - ain't nobody got time for ", Endpoint/binary>>, Req ),
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% Catch all requests
%-------------------------------------------------------------------------------
serve( _, _, _, Req, State ) ->
	cowboy_http_req:reply( 404, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"404 : ", Endpoint/binary">>, Req ),
	{ ok, Req, State }.