% Copyright 2012 Daniel Kendell <daniel.kendell@gmail.com>

% This file is part of PushMePullYou.
%
% PushMePullYou is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% PushMePullYou is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with PushMePullYou.  If not, see <http://www.gnu.org/licenses/>.

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
			serve( Method, Path, <<"">>, Req, State );
		[ Path, Extension ] ->
			serve( Method, Path, Extension, Req, State )
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
	pmpy:notify( Path, Msg ),
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
websocket_info( { _, Msg }, Req, State ) ->
	{ reply, { text, Msg }, Req, State, hibernate }.

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

%===============================================================================
% serve/5
%===============================================================================
% Push a message over http to an endpoint
%-------------------------------------------------------------------------------
serve( 'POST', Endpoint, <<>>, Req, State ) ->
	{ ok, Body, _ } = cowboy_http_req:body( Req ),
	pmpy:notify( Endpoint, Body ),
	cowboy_http_req:reply( 200, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"ok">>, Req ),
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% Get the last message sent to an endpoint
%-------------------------------------------------------------------------------
serve( 'GET', Endpoint, <<>>, Req, State ) ->
	Message = pmpy:get_latest( Endpoint ),
	cowboy_http_req:reply( 200, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], Message, Req ),
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% HTTP subscribe to an endpoint
%-------------------------------------------------------------------------------
serve( 'POST', Endpoint, <<"subscribe">>, Req, State ) ->
	http_subscription( subscribe, Endpoint, Req ),
	cowboy_http_req:reply( 200, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"ok">>, Req ),
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% HTTP subscribe to an endpoint
%-------------------------------------------------------------------------------
serve( 'POST', Endpoint, <<"unsubscribe">>, Req, State ) ->
	http_subscription( unsubscribe, Endpoint, Req ),
	cowboy_http_req:reply( 200, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"ok">>, Req ),
	{ ok, Req, State };
%-------------------------------------------------------------------------------
% Catch all requests
%-------------------------------------------------------------------------------
serve( _, _, _, Req, State ) ->
	cowboy_http_req:reply( 405, [ 
		{ <<"Content-Type">>, <<"text/plain">> } 
	], <<"405 Method Not Allowed">>, Req ),
	{ ok, Req, State }.

%===============================================================================
% http_subscription/3
%
% Extracts parameters from the request body and either subscribe to an
% endpoint or unsubscribe from one.
%===============================================================================
http_subscription( Function, Endpoint, Req ) ->
	{ PostData, _ } = cowboy_http_req:body_qs( Req ),
	true = proplists:is_defined( <<"token">>, PostData ),
	true = proplists:is_defined( <<"url">>, PostData ),
	Token = proplists:get_value( <<"token">>, PostData ),
	Url = proplists:get_value( <<"url">>, PostData ),
	{ ok, Pid } = pmpy:httpsubscriber( Token, Url ),
	pmpy_httpsubscriber:Function( Pid, Endpoint ).
